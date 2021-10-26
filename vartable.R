source('packages_structure.R')

inp_riveratlas_meta <- file.path(datadir, "HydroATLAS_metadata_MLMv11_LakeATLAS.xlsx")

# Read the feature class
lattri <- fread(file.path(resdir, 'LakeATLAS_v01_poly.csv'), showProgress=T)


##################### FORMAT variable names ####################################
#Get predictor variable names
metaall <- readxl::read_xlsx(inp_riveratlas_meta,
                             sheet='Overall') %>%
  setDT

metascale <- readxl::read_xlsx(inp_riveratlas_meta,
                               sheet='scale') %>%
  setDT %>%
  setnames(c('Key','Spatial representation'),
           c('Keyscale', 'Spatial.representation'))

metastat <- readxl::read_xlsx(inp_riveratlas_meta,
                              sheet='stat') %>%
  setDT %>%
  setnames(c('Key','Temporal or statistical aggregation or other association'),
           c('Keystat', 'Temporal.or.statistical.aggregation.or.other.association'))

meta_format <- as.data.table(expand.grid(`Column(s)`=metaall$`Column(s)`,
                                         Keyscale=metascale$Keyscale,
                                         Keystat=metastat$Keystat)) %>%
  .[metaall, on='Column(s)'] %>%
  .[metascale, on = 'Keyscale'] %>%
  .[metastat, on = 'Keystat',
    allow.cartesian=TRUE]

meta_format[, `:=`(
  unit = substr(`Column(s)`, 5, 6),
  varcode = paste0(gsub('[-]{3}', '', `Column(s)`),
                   Keyscale,
                   fifelse(grepl("[0-9]",Keystat),
                           str_pad(Keystat, 2, side='left', pad='0'),
                           Keystat)),
  varname = paste(Variable,
                  Spatial.representation,
                  Temporal.or.statistical.aggregation.or.other.association))]


varmerge <- merge(data.table(varcode=colnames(lattri)), meta_format, 
                  by='varcode', all.x=T, all.y=F) %>%
  setnames(old='Temporal.or.statistical.aggregation.or.other.association',
           new='tsagg')

#Remove duplicates (which were created with keystat meaning different things e.g; 09 meaning september, 2009, class 9)
varmerge_nodupli <- varmerge[!(
  (Category == "Climate" &
     grepl('Class.*', tsagg)) |
    (Category == "Landcover" &
       !grepl('(Class|Spatial).*', tsagg))
),]  %>%
  .[grepl('hft_ix_[cuv]09', varcode),
    `:=`(tsagg='2009',
         varname = gsub('(?<=Human\\sFootprint\\s(watershed|catchment)).*',
                        ' 2009',
                        varname,
                        perl=T)
    )] %>%
  .[grepl('hft_ix_[cuv]93', varcode),
    `:=`(tsagg='1993',
         varname = gsub('(?<=Human\\sFootprint\\s(watershed|catchment)).*',
                        ' 1993',
                        varname,
                        perl=T)
    )] %>%
  unique(by='varcode')


vartabformat <- dcast(varmerge_nodupli,
                      formula = ID+Category+Variable+`Source Data`+`Source Resolution (G: Grid V: Vector)`+
                        `Source year`+Reference~`Spatial.representation`
)

fwrite(vartabformat, file = file.path(resdir, 'LakeATLAS_vartab.csv'))

varmerge_nodupli[, .N, by='Spatial.representation']

