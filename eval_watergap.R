source('packages_structure.R')
source('utility_functions.R')

#
grdcdatdir <- file.path(datadir, "GRDCdat_day")

#-------------------------- Import tables --------------------------------------
#Find stations within lake or downstream such that drainage area at lake 
#pourpoint is > 90% of that of lake
lattri <- fread(file.path(resdir, 'LakeATLAS_v01_poly.csv'), showProgress=T)

#See src/Python/link_hydrolakes_riveratlas.py
linkpoltab <- fread(file.path(resdir, 'hylakp_link_str_pol.csv'),
                   select = c("HydroLAKES_points_v10", "link_str_pol_Band_1"),
                   colClasses = rep("integer", 5), showProgress = T) 

linkriv <- fread(file.path(resdir, "link_hyriv_v10.csv"),
                   select = c('HYRIV_ID', 'LINK_RIV', 'NEXT_DOWN', 'UPLAND_SKM'), 
                   showProgress = T) 

#Import GRDC stations data
grdctab = fread(file.path(resdir, 'grdcstations_riverjoinedit.csv'))

#-------------------------- Find all GRDC stations-lake links ------------------
lakrivtab <-merge(linkriv[, .(HYRIV_ID, LINK_RIV)], linkpoltab, 
                  by.x = "LINK_RIV", by.y = "link_str_pol_Band_1",
                  all.x=F, all.y=F) %>%
  .[, -'LINK_RIV', with=F] %>%
  setnames("HydroLAKES_points_v10", "Hylak_id") %>%
  merge(lattri[, .(Hylak_id, Lake_type, Lake_area, Wshd_area, dis_m3_pyr)],
        by = 'Hylak_id')
remove(linkpoltab)
gc() #Free memory for other applications 

#Identify grdc stations that have HydroLAKES in their immediate watershed
grdclak <- merge(grdctab, lakrivtab, by="HYRIV_ID", all.x=F, all.y=F)
grdclak[, wsratio := Wshd_area/UPLAND_SKM] #Computer ratio of lake watershed area to gauge watershed area

grdclakN <- lapply(seq(0, 1, 0.05), function(x) {
  data.table(wsarearatio = x, 
             Ngauges = grdclak[wsratio >= x, .N])
}) %>% rbindlist

ggplot(grdclakN, aes(x=wsarearatio, y=Ngauges)) + 
  geom_step() + 
  scale_x_continuous(breaks=seq(0, 1, 0.05))

grdclak95 <- grdclak[wsratio >= 0.95,] %>%
  .[, .SD[which.min(1-wsratio),], by=Hylak_id]



#--------------------------Evaluate watergap -----------------------------------
GRDCgauged_filenames <- read_GRDCgauged_paths(
  inp_GRDCgaugedir = grdcdatdir,
  in_gaugep = grdclak95,
  exclude_missing = T) %>%
  unique

GRDCqstats <- rbindlist(lapply(GRDCgauged_filenames,
                               comp_GRDCqstats,
                               maxgap = 20,
                               minyear = 1971,
                               maxyear = 2000,
                               verbose = F)) %>%
  .[!(GRDC_NO %in% c(5101201,  #Exclude Burdekin River, erroneous 0 flow
                     6233365, 1303911, 1056097, 4119454, 4122133, 4214810 #Issue in merging of the gauge with network
                     ))]

grdclak95[, GRDC_NO := as.character(GRDC_NO)]

# watergap_check <- eval_watergap(in_qstats = GRDCqstats,
#                                 in_selgauges = grdclak95,
#                                 binarg = c(1, 10, 100, 1000, 10000, 1000000),
#                                 nyears_min = 20,
#                                 discol = 'dis_m3_pyr.y'
# )

watergap_check <- eval_watergap(in_qstats = GRDCqstats,
                                in_selgauges = grdclak95,
                                binvar = 'Lake_area',
                                binarg = c(1, 10, 100),
                                nyears_min = 20,
                                discol = 'dis_m3_pyr.y'
)

fwrite(watergap_check, file.path(resdir, 'watergap_check_table.csv'))

watergap_natural <- eval_watergap(in_qstats = GRDCqstats,
                                  in_selgauges = grdclak95[Lake_type == 1,],
                                  binvar = 'Lake_area',
                                  binarg = c(1, 10, 100),
                                  nyears_min = 20,
                                  discol = 'dis_m3_pyr.y'
)

watergap_res <- eval_watergap(in_qstats = GRDCqstats,
                                  in_selgauges = grdclak95[Lake_type > 1,],
                                  binvar = 'Lake_area',
                                  binarg = c(1, 10, 100),
                                  nyears_min = 20,
                                  discol = 'dis_m3_pyr.y'
)


#-------------------------- Map gauges ----------------------------------------
grdclakmerge <- merge(GRDCqstats[!is.na(GRDC_NO) & nyears >= 20,],
                      grdclak95[!is.na(GRDC_NO),], 
                      by='GRDC_NO', all.x=F, all.y=F) %>%
  .[, dispercerror := 100*(dis_m3_pyr.y-qmean)/qmean]

crs_wintri = "+proj=wintri +datum=WGS84 +no_defs +over"
lakp <- st_as_sf(x = merge(grdclakmerge, lattri[, .(Hylak_id, Pour_long, Pour_lat)], by='Hylak_id', all.y=F),
                 coords = c("Pour_long", "Pour_lat"),
                 crs = 4326) %>%
  lwgeom::st_transform_proj(crs = crs_wintri)


world_sf <- sf::st_as_sf(getMap(resolution = "low")) 
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs_wintri)

gmap <- ggplot() +
  geom_sf(data=world_wintri, color='#bdbdbd', alpha=1/2) +
  geom_sf(data = lakp, aes(color=as.factor(Lake_type)), size=1, alpha=0.5) + #color = alpha('#3182bd', 1/2)
  scale_color_manual(name='',
                     values=c('#2171b5','#fd8d3c','#800026'),
                     labels=c('Natural lake', 'Human-made reservoir', 'Controled lake')) +
  coord_sf(datum = NA, expand=F,
           xlim=c(-12000000, 18000000), ylim=c(-7000000,9300000)) +
  theme_map() +
  theme(legend.position='none', #c(0.4, 0.15),
        #legend.direction="horizontal",
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        legend.key.height = unit(0.1,"in"),
        legend.background = element_blank())


#-------------------------- Make scatterplot ----------------------------------------
scatter1to1 <- ggplot(grdclakmerge, aes(x=dis_m3_pyr.y, y=qmean, color=as.factor(Lake_type))) + 
  geom_abline() +
  geom_point(alpha=0.75, size=2.5) +
  #geom_smooth() +
  scale_x_log10(name=expression('Modeled discharge'~(m^3~s^-1))) + 
  scale_y_log10(name=expression('Observed discharge'~(m^3~s^-1))) + 
  scale_color_manual(values=c('#2171b5','#fd8d3c','#800026'),
                     labels=c('Natural lake', 'Human-made reservoir', 'Controled lake')) +
  guides(color = guide_legend(# title.hjust = 1, # adjust title if needed
    label.position = "left",
    label.hjust = 1)
    )+
  coord_fixed() + 
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.background = element_blank(),
        legend.key.size = unit(0, 'lines'),
        text = element_text(size=12),
        plot.margin = unit(rep(0.35, 4), 'cm'))


grdclakmerge_o100error <- grdclakmerge[dispercerror > 100 | dispercerror < -50, 
                                       .(GRDC_NO, nyears, qmean, dis_m3_pyr.y, Hylak_id, 
                                         Lake_area,
                                         Wshd_area, dispercerror, Lake_type)]

#70% are reservoirs
grdclakmerge_o100error[(Lake_type == 2) | 
                         Hylak_id %in% c(184622, 8313, 992825, 993765, 111839, 
                                         9562, 1065810, 184898, 16136), .N]

#Embed map within scatterplot
pdf(file.path(figdir, paste0('GRDCevalscatter_2010815.pdf')),#
    width = 4, height = 4)
scatter1to1 +
  annotation_custom(grob = ggplotGrob(gmap),
                    xmin = -0.65, xmax = 2.5, ymin = 2.2, ymax = 4.2)
dev.off()

# png(file.path(figdir, paste0('GRDCevalscatter_', format(Sys.Date(), '%y%m%d'), '.png')),
#     width = 4, height = 4, units = 'in', res = 600)
# scatter1to1 +
#   annotation_custom(grob = ggplotGrob(gmap), 
#                     xmin = -0.65, xmax = 2.5, ymin = 2.2, ymax = 4.2)
# dev.off()




