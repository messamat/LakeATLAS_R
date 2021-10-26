source('packages_structure.R')
source('utility_functions.R')


testdir <- file.path(datadir, '16695913')

#Exported csv tables of each of the layers provided

########## GDB PNT ############
pnt_gdb <- fread(file.path(testdir, 'LakeATLAS_v01_pnt_gdb.csv'))
dim(pnt_gdb) #306 columns: 281 hydro-environmental attributes + 24 HydroLAKES attributes + OBJECTID
names(pnt_gdb)

#Compare names to RiverATLAS
riveratlas_head <- fread(file.path(dirname(rootdir), 'globalIRmap', 'results', 'RiverATLAS_v10tab.csv'),
                         nrow=1)
names(riveratlas_head)[substr(names(riveratlas_head),1, 7) %in% substr(names(pnt_gdb), 1, 7)] #The 281 hydro-environmental attributes match that of RiverATLAS

#check min and max
pnt_gdb[, sapply(.SD, function(x) c(min(x), median(x), max(x)))]

remove(pnt_gdb)

########## SHP PNT ############
pnt_shp1 <- fread(file.path(testdir, 'LakeATLAS_v01_pnt_shp_af_as_au_eu_sa_si.csv'))
pnt_shp2 <- fread(file.path(testdir, 'LakeATLAS_v01_pnt_shp_ar_gr_na.csv'))
pnt_shp <- rbind(pnt_shp1, pnt_shp2)
names(pnt_shp)
dim(pnt_shp) #307 columns. Includes both FID and OBJECTID
str(pnt_shp[, 1:50])
names(riveratlas_head)[substr(names(riveratlas_head),1, 7) %in% substr(names(pnt_shp), 1, 7)] #The 281 hydro-environmental attributes match that of RiverATLAS

mergetab <- merge(
  unique(data.table(name = substr(names(riveratlas_head),1, 7),
             type_riv = riveratlas_head[, sapply(.SD, class)])),
  unique(data.table(name = substr(names(pnt_shp),1, 7),
             type_lak = pnt_shp[, sapply(.SD, class)])),
  on='name')

########## GDB POLY ############
pol_gdb <- fread(file.path(testdir, 'LakeATLAS_v01_pol_gdb.csv'))
dim(pol_gdb) #306 columns: 281 hydro-environmental attributes + 24 HydroLAKES attributes + OBJECTID
names(pol_gdb)
str(pol_gdb[, 1:50])
names(riveratlas_head)[substr(names(riveratlas_head),1, 7) %in% substr(names(pol_gdb), 1, 7)] #The 281 hydro-environmental attributes match that of RiverATLAS

mergetab <- merge(
  unique(data.table(name = substr(names(riveratlas_head),1, 7),
                    type_riv = riveratlas_head[, sapply(.SD, class)])),
  unique(data.table(name = substr(names(pol_gdb),1, 7),
                    type_lak = pol_gdb[, sapply(.SD, class)])),
  on='name')

############ SHP POLY ###############
pol_shp1 <- fread(file.path(testdir, 'LakeATLAS_v01_pol_shp_af_as_au_eu_sa_si.csv'))
pol_shp2 <- fread(file.path(testdir, 'LakeATLAS_v01_pol_shp_ar_gr_na.csv'))
pol_shp <- rbind(pol_shp1, pol_shp2)
names(pol_shp)
dim(pol_shp) #307 columns. Includes both FID and OBJECTID
str(pol_shp[, 1:50])
names(riveratlas_head)[substr(names(riveratlas_head),1, 7) %in% substr(names(pol_shp), 1, 7)] #The 281 hydro-environmental attributes match that of RiverATLAS

mergetab <- merge(
  unique(data.table(name = substr(names(riveratlas_head),1, 7),
                    type_riv = riveratlas_head[, sapply(.SD, class)])),
  unique(data.table(name = substr(names(pol_shp),1, 7),
                    type_lak = pol_shp[, sapply(.SD, class)])),
  on='name')

