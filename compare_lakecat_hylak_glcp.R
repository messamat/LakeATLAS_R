#See pre-formatting in LakeATLAS/src/Python/Compare_lakeatlas_lakecat.py

source('packages_structure.R')
source('utility_functions.R')

lcatdir <- file.path(datadir, 'lakecat')

lattri <- fread(file.path(resdir, 'LakeATLAS_v01_poly.csv'), showProgress=T) #HydroLAKES attribute table
lattri_us <- fread(file.path(resdir, 'hylak_ustab.csv'))
nhdtab <- fread(file.path(resdir, 'NHDPlusV21_Waterbody.csv')) #NHD waterbody snapshot attribute table
interstab <- fread(file.path(resdir, 'hydrolakes_nhdwb_inters.csv')) #Intersection of HydroLAKES and NHD waterbody snapshot 
lcatpop <- fread(file.path(lcatdir, 'USCensus2010.csv')) #LakeCat data for US Census 2010
glcptab <- fread(file.path(datadir, 'glcp.csv')) %>%
  .[year == 2010, .(Hylak_id, bsn_lvl, HYBAS_ID, pop_sum)]
hybasids <- fread(file.path(resdir, 'HydroBASINS_IDs.csv')) %>%
  .[, list(HYBAS_ID = bit64::as.integer64(HYBAS_ID), bsn_lvl, SUB_AREA)]

#hylakpop <- fread(file.path(resdir, 'LakeATLAS_v01_poly.csv'), select = c('Hylak_id', 'ppd_pk_uav'))   

#Subset HydroLAKES for conterminous US
hylakus <- lattri_us[Country == 'United States of America' & Pour_lat <  49.4,]
hylakus[,.N]
interstab[!duplicated(Hylak_id), .N]

#Get full waterbody area for waterbodies with GNIS_ID
#This is needed because several hundred lake polygons are split in NHD
nhdref <- nhdtab[, NHDID := fifelse(is.na(GNIS_ID), COMID, GNIS_ID)] %>%
  .[, list(AREA_nhd = sum(AREA_nhd), FTYPE = FTYPE[[1]]), by = NHDID] %>%
  .[(FTYPE %in% c("LakePond", "Reservoir")),]

#--------------------- Check distribution of surface area ----------------------
nhdhylak_bind <- rbind(nhdref[, .(NHDID, AREA_nhd, FTYPE)], 
                       hylakus[, .(Hylak_id, area, Lake_type, AREA_hydrolakes)], 
                       use.names=T, fill=T)

nhdhylak_bind[!is.na(FTYPE), Lake_typenhd := fifelse(FTYPE=="Reservoir", 2, 1)]

nhdhylak_format <- nhdhylak_bind[, list(
  dataset = fifelse(is.na(NHDID), 'HydroLAKES', 'NHDPlus'),
  area = fifelse(is.na(NHDID), AREA_hydrolakes, AREA_nhd),
  laketype = fifelse(is.na(NHDID), Lake_type, Lake_typenhd)
)]

nhdhylak_bin <- bin_dt(nhdhylak_format, binvar='area', binfunc='manual',
                       binarg=c(10^nhdhylak_format[, seq(floor(log10(min(area))),3, 0.2)],
                                10^4, 10^6)
)

nhdhylak_ratio <- nhdhylak_bin[, list(
  Nratio = .SD[dataset=='HydroLAKES', .N]/.SD[dataset=='NHDPlus', .N], 
  arearatio = .SD[dataset=='HydroLAKES', sum(area)]/.SD[dataset=='NHDPlus', sum(area)]
), by=.(bin_lmin, bin_lmax)] 
#%>%
 # melt(id.vars='bin_lmin')

nhdhylak_sum <- nhdhylak_bin[, list(Nsum = .N), by=.(bin_lmin, dataset)] %>%
  .[, Nsum_scaled := scales::rescale(Nsum, to = c(0, 2))] %>%
  merge(nhdhylak_ratio, by=c('bin_lmin'),all.x=T, all.y=T)

areacompare_p <- ggplot(nhdhylak_sum,  aes(x=as.factor(bin_lmin), y=Nsum_scaled)) + 
  geom_step(aes(y=Nratio, group=1), size=1.2) + #aes(color=variable) + 
  geom_bar(aes(fill=dataset), stat='identity', position='identity', alpha=0.6) +
  geom_hline(yintercept=1, alpha=1/2, linetype='dotdash') +
  scale_y_continuous(name = "Number of lakes by size class",
                     breaks = c(0, 10000, 25000, 50000, 75000)/(max(nhdhylak_sum$Nsum)/2),
                     labels = c(0, 10000, 25000, 50000, 75000),
                     sec.axis = sec_axis(~ .,
                                         name = str_wrap('Ratio in number of lakes (HydroLAKES/NHDPlus) by size class', 45)
                       #, 
                       #limits=c(0,2), 
                       #expand=c(0,0)
                     )) +
  scale_x_discrete(name = expression('Lake area size class'~(km^2)),
                   breaks=unique(nhdhylak_sum$bin_lmin)[seq(4,34,5)],
                   labels=c(10^seq(-3,3))) +
  scale_fill_manual(name = 'Number of lakes', values=c('black', 'grey')) +
  # scale_color_manual(
  #   name = '',
  #   values = c('#fd8d3c', '#2171b5'), 
  #   labels = c('Relative number', 'Relative surface area')) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(size=12))

pdf(file.path(figdir, paste0('HydroLAKES_NHDPlus_compareplot_', format(Sys.Date(), '%y%m%d'), '.pdf')),
    width = 5, height = 5)
areacompare_p
dev.off()

#--------------------- Merge LakeATLAS with GLCP --------------------------------------------
lakeatlas_glcptab_join <- merge(glcptab, hybasids, by=c('bsn_lvl', 'HYBAS_ID')) %>% #Associate GLCP dataset with HydroBASINS polygon areas
  merge(lattri, all.y=T, by='Hylak_id') %>%
  .[, glcp_popdens := pop_sum/SUB_AREA]

#--------------------- Match lakes ---------------------------------------------
#Merge sub-intersections
interstab[, NHDID := fifelse(is.na(GNIS_ID), COMID, GNIS_ID)]

interstab_format <- interstab[, list(
  AREA_inters = sum(AREA_inters),
  AREA_hydrolakes = AREA_hydrolakes[[1]]
), by=.(NHDID, Hylak_id)] %>%
  merge(nhdref, by='NHDID', all.y=F)

#Compute ratios between intersection and original area
interstab_format[, `:=`(
  arearatio_intersnhd = AREA_inters/AREA_nhd,
  arearatio_intershydrolakes = AREA_inters/AREA_hydrolakes,
  smape_hydrolakesnhd = 2*(AREA_hydrolakes-AREA_nhd)/(AREA_hydrolakes+AREA_nhd))]

#Subset for each HydroLAKES, the intersection that matches its area the best
HydroLAKES_bestmatch <- interstab_format[, .SD[which.max(arearatio_intershydrolakes),], 
                                         by = Hylak_id]

#Compute the number of HydroLAKES polygons that are intersected by an NHDPlus 
#polygon by at least a given area ratio threshold
HydroLAKESmatchthresh_dt <- lapply(seq(0,1,0.01), function(minratio) {
  HydroLAKES_bestmatch[(arearatio_intershydrolakes) >= minratio,
                       list(minratio = minratio,
                            Nlakes = .N)]
}) %>% rbindlist %>%
  .[, `:=`(totalintersN = interstab_format[, length(unique(Hylak_id))], #Total # of HydroLAKES with any intersection
           totalhylakN = nrow(hylakus)  #Total # of HydroLAKES in US
           )] 


#Compute the HydroLAKES-NHDPlus lakes that match on both sides and have same surface area to given thresholds
totalmatchthresh_dt <- lapply(seq(0.5,1,0.01), function(minratio) {
  interstab_format[arearatio_intershydrolakes >= minratio & 
                     arearatio_intersnhd >= minratio &
                     smape_hydrolakesnhd <= 1-minratio,
                   list(minratio = minratio,
                        Nlakes = .N)]
}) %>% rbindlist
       
lakes_minmatch <- interstab_format[arearatio_intershydrolakes >= 0.9 & 
                                       arearatio_intersnhd >= 0.9,] %>% 
  merge(nhdtab, by='NHDID', all.x = T, all.y = F) %>%
  merge(lcatpop, by='COMID', all.x = T, all.y = F) %>%
  .[!duplicated(NHDID),] %>%
  merge(hylakus, by='Hylak_id', all.x = T, all.y = F) %>%
  merge(lattri[, .(Hylak_id, ppd_pk_uav, pop_ct_usu)], by='Hylak_id', all.x = T, all.y = F) %>%
  merge(lakeatlas_glcptab_join[, .(Hylak_id, SUB_AREA, pop_sum)], by = 'Hylak_id',
        all.x = T, all.y = F) %>%
  .[, `:=`(pop_lakecatWs = PopDen2010Ws*WsAreaSqKm,
           pop_lakecatWs1000 = PopDen2010Ws*WsAreaSqKm/1000,
           pop_lakecatCat = PopDen2010Cat*CatAreaSqKm
           )]

#Number of matched lakes between HydroLAKES and LakeCAT
lakes_minmatch[!is.na(WsAreaSqKm), .N]

#Number of matched lakes between HydroLAKES and GLCP
lakeatlas_glcptab_join[!is.na(HYBAS_ID), .N]
check <- lakeatlas_glcptab_join[is.na(HYBAS_ID), ]

#Number of matched lakes between GLCP and LakeCAT
lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA), .N]
lakes_minmatch[!is.na(WsAreaSqKm) & is.na(SUB_AREA), Hylak_id]

#--------------------- Compare watershed areas between HydroLAKES, LakeCAT, and GLCP ---------------------------------------------
min(lakes_minmatch$WsAreaSqKm, na.rm=T)
min(lakes_minmatch$Wshd_area, na.rm=T)
plotbreaks_wsarea <- seq(-2, 6, 2)
plotbreaks_wspop <- seq(-10, 6, 4)


areacompare_p_hylaknhd <- ggplot(lakes_minmatch[!is.na(WsAreaSqKm),],
                                 aes(x=log10(Wshd_area), y=log10(WsAreaSqKm))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count',
                       limits=c(1,45), breaks=c(1,20,40)) +
  scale_x_continuous(name = 'LakeATLAS',
                     breaks = plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  scale_y_continuous(name = 'LakeCAT', 
                     breaks=plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())

areacompare_p_glcpnhd <- ggplot(lakes_minmatch[!is.na(WsAreaSqKm),],
                            aes(x=log10(SUB_AREA), y=log10(WsAreaSqKm))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count',
                       limits=c(1,45), breaks=c(1,20,40)) +
  scale_x_continuous(name = 'GLCP',
                     breaks = plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  scale_y_continuous(name = 'LakeCAT', 
                     breaks=plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())


areacompare_p_glcphylak <- ggplot(lakeatlas_glcptab_join,
                                aes(x=log10(SUB_AREA), y=log10(Wshd_area))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count', trans = "log",
                       limits=c(1,9000), breaks=c(1,10,100,1000,9000)) +
  scale_x_continuous(name = 'GLCP',
                     breaks = plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  scale_y_continuous(name = 'LakeATLAS', 
                     breaks=plotbreaks_wsarea,
                     labels = format(10^plotbreaks_wsarea, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())


popcompare_p_hylaknhd <- ggplot(lakes_minmatch[!is.na(WsAreaSqKm),],
                                 aes(x=log10(pop_ct_usu*1000), y=log10(pop_lakecatWs))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count',
                       #palette='RdBu',
                       limits=c(1,17), breaks=c(1,5,10,17)) +
  scale_x_continuous(name = 'LakeATLAS',
                     breaks = plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  scale_y_continuous(name = 'LakeCAT', 
                     breaks=plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())

popcompare_p_glcpnhd <- ggplot(lakes_minmatch[!is.na(WsAreaSqKm),],
                                aes(x=log10(pop_sum), y=log10(pop_lakecatWs))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count',
                       #palette='RdBu',
                       limits=c(1,17), breaks=c(1,17)) +
  scale_x_continuous(name = 'GLCP',
                     breaks = plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  scale_y_continuous(name = 'LakeCAT', 
                     breaks=plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())

popcompare_p_glcphylak <- ggplot(lakeatlas_glcptab_join,
                               aes(x=log10(pop_sum), y=log10(1000*pop_ct_usu))) +
  #geom_point(alpha=1/3) +
  geom_abline() +
  stat_bin_hex(bins = 100, alpha=0.9) +
  scale_fill_viridis_c(name = 'Lake count', trans='log',
                       #colors= c('#2171b5','#800026'),
                       #low = '#2171b5', mid = '#fd8d3c', high = '#800026', 
                       #palette='RdBu',
                       limits=c(1,5000), breaks=c(1,10,100,1000,5000)) +
  scale_x_continuous(name = 'GLCP',
                     breaks = plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  scale_y_continuous(name = 'LakeATLAS', 
                     breaks=plotbreaks_wspop,
                     labels = format(10^plotbreaks_wspop, scientific = T)) +
  #geom_smooth(method = "lm") +
  #geom_quantile(quantiles=c(0.5)) +
  #scale_x_log10() +
  #scale_y_log10() +
  #coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none', #c(0.15, 0.80),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.grid.minor = element_blank())


# png(file.path(figdir, paste0('WshdAreaPopcompareplot_', format(Sys.Date(), '%y%m%d'), '.png')),
#     width = 8, height = 8, units = 'in', res = 600)
# (((areacompare_p_hylaknhd | areacompare_p_glcpnhd | areacompare_p_glcphylak))/
#   ((popcompare_p_hylaknhd | popcompare_p_glcpnhd | popcompare_p_glcphylak))
#   ) + plot_annotation(tag_levels = 'a')
# dev.off()

pdf(file.path(figdir, paste0('WshdAreaPopcompareplot_20210815.pdf')),
    width = 8, height = 8)
(((areacompare_p_hylaknhd | areacompare_p_glcpnhd | areacompare_p_glcphylak))/
    ((popcompare_p_hylaknhd | popcompare_p_glcpnhd | popcompare_p_glcphylak))
) + plot_annotation(tag_levels = 'a')
dev.off()

expression('Watershed area'~(km^2))
'Population count in watershed (2010)'

#------------------------- Watershed area quantitative comparison --------------------------
#Between lakeCAT and lakeATLAS
getqstats(dt = lakes_minmatch[!is.na(WsAreaSqKm) & AREA_hydrolakes.x > 0.1,],
          x = 'WsAreaSqKm', y = 'Wshd_area',
          rstudthresh= 3, log=TRUE)

#between lakeCAT and GLCP
getqstats(dt = lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA),],
          x = 'WsAreaSqKm', y = 'SUB_AREA',
          rstudthresh= 3, log=TRUE)

#------------------------- Watershedpopulation count quantitative comparison --------------------------
#between lakeCAT and lakeATLAS
getqstats(dt = lakes_minmatch[!is.na(WsAreaSqKm),],
          x = 'pop_lakecatWs1000', y = 'pop_ct_usu',
          rstudthresh= 3, log=TRUE)

#between lakeCAT and GLCP
getqstats(dt = lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA),],
          x = 'pop_lakecatWs', y = 'pop_sum',
          rstudthresh= 3, log=TRUE)

lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA) & (pop_sum > pop_lakecatWs), .N]/lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA), .N]
lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA) & (pop_sum > pop_lakecatWs) & WsAreaSqKm < 100, .N]/lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA) & WsAreaSqKm < 100, .N]

#Between lakeCAT and GLCP for catchment
getqstats(dt = lakes_minmatch[!is.na(WsAreaSqKm) & !is.na(SUB_AREA),],
          x = 'pop_lakecatCat', y = 'pop_sum',
          rstudthresh= 3, log=TRUE)

#Quantitative comparison in watershed area between GLCP and lakeATLAS
getqstats(dt = lakeatlas_glcptab_join[!is.na(SUB_AREA),],
          x = 'Wshd_area', y = 'pop_sum',
          rstudthresh= 3, log=TRUE)




#https://stats.stackexchange.com/questions/129200/r-squared-in-quantile-regression
rqnull <- rq(log10(WsAreaSqKm)~1,
            tau = 0.75,data=lakes_minmatch)
rqmod <- rq(log10(WsAreaSqKm)~log10(Wshd_area),
            tau = 0.75,data=lakes_minmatch)
rho <- function(u,tau=.75)u*(tau - (u < 0))
Vmod <- sum(rho(rqmod$resid, rqmod$tau))
Vnull <- sum(rho(rqnull$resid, rqnull$tau))
R1 <- 1 - Vmod/Vnull
R1




