## File to join SIMANFOR inventories to coordinates data of each plot
## Author: Aitor Vázquez Veloso
## 29/07/2021
## Very important corrections: Cristóbal Ordóñez

setwd("./source")
dir()
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("openxlsx")
library("openxlsx")

# transform plot data into spatial dataframe
# ED50 / UTM zone 30N   <23030>
library(rgdal)

setwd("./source")
dir()
# import data
data_mix <- read_excel('Mix.xlsx', sheet = "Parcelas")
data_cat <- read.csv('plots_pure_Cat_IFN.csv')
data_gal <- read.csv('plots_pure_Gal_IFN.csv')
data_normal <- read.csv('plots_pure_IFN.csv')

setwd("../plots")
dir()
IFN3_coord <- read.csv('./pcdatosmap.csv')


# transform plot data into spatial dataframe
# ED50 / UTM zone 30N   <23030>

CRS_28 <- "+proj=utm +zone=28 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_29 <- "+proj=utm +zone=29 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_30 <- "+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_31 <- "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
## CRS_1 <- "+proj=utm +zone=30 +ellps=intl +units=m +towgs84=-87,-98,-121,0,0,0,0"  # utm gs84 
## CRS_2 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"                 # wgs84 
IFN3_coord$huso <- with(IFN3_coord,
                          case_when( 
                            Origen %in% c(38, 35) ~ 28, #huso 28: Canarias
                            Origen %in% c(15, 27, 36, 32, 21)  ~ 29, #huso 29: Galicia y Huelva (casi seguro que no tiene nada en el 30)
                            Origen %in% c(33, 24, 49, 37, 10, 6, 41, 11) ~ ifelse(CoorX>500000, 29, 30), # Husos 29 y 30: Asturias, León, Zamora, Salamanca, Extremadura (ambas provincias), Sevilla y Cádiz
                            Origen %in% c(31, 39, 1, 20, 48, 47, 9, 34, 42, 5, 40, 26, 28, 2, 13, 16, 19, 45, 30, 4, 14, 18, 23, 29, 51)  ~ 30, #Huso 30: Cantabria, Pais Vasco, Castilla y León (salvo las 3 provincias leonesas), La Rioja, Madrid, Castilla La Mancha, Murcia, Andalucía (Salvo las 3 provincias más occidentales mencionadas)
                            Origen %in% c(46, 12, 3, 22, 50, 44) ~ ifelse(CoorX>500000, 30, 31), # Huso 30 y 31: Comunidad Valenciana y Aragon
                            Origen %in% c(8, 25, 17, 43, 7)  ~ 31,# huso 31:Cataluña y Baleares
                          )
)
#names(IFN3_coord)
IFN3_coord <- IFN3_coord[, c("Provincia", "Estadillo", "Clase",     "CoorX",     "CoorY",    "huso" )]
head(IFN3_coord)
str(IFN3_coord)

# adapt IFN data to the SIMANFOR PLOT_ID structure
IFN3_coord$PLOT_ID_IFN2 <- paste(IFN3_coord$Provincia, IFN3_coord$Estadillo, 'IFN2', sep = '_')
IFN3_coord$PLOT_ID_IFN3 <- paste(IFN3_coord$Provincia, IFN3_coord$Estadillo, 'IFN3', sep = '_')
IFN3_coord$Provincia_mix <- ifelse(nchar(IFN3_coord$Provincia) == 1, paste(0, IFN3_coord$Provincia, sep = ''), IFN3_coord$Provincia)
IFN3_coord$PLOT_ID_mix <- paste(IFN3_coord$Provincia_mix, IFN3_coord$Estadillo, sep = '_')

# adapt inputs to join the coordinates information
data_normal$ID_to_join <- sub("_A_.*", "_IFN3", data_normal$PLOT_ID)
data_gal$ID_to_join <- sub("_A_.*", "_IFN3", data_gal$PLOT_ID)
data_cat$ID_to_join <- sub("_A_.*", "_IFN3", data_cat$PLOT_ID)
data_mix$ID_to_join <- sub("_SC1", "", data_mix$PLOT_ID)
data_mix$ID_to_join <- sub("(.{2})(.*)", "\\1_\\2", data_mix$ID_to_join)
data_mix$ID_to_join <- sub("_0", "_", data_mix$ID_to_join)
data_mix$ID_to_join <- sub("_0", "_", data_mix$ID_to_join)
data_mix$ID_to_join <- sub("_0", "_", data_mix$ID_to_join)

# join both databases
data_cat_new <- merge(data_cat, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN2')
data_cat_new_1 <- merge(data_cat, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN3')
data_cat_new$PLOT_ID_IFN3 <- NULL
data_cat_new_1$PLOT_ID_IFN2 <- NULL
data_cat_new <- rbind(data_cat_new, data_cat_new_1)

data_gal_new <- merge(data_gal, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN2')
data_gal_new_1 <- merge(data_gal, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN3')
data_gal_new$PLOT_ID_IFN3 <- NULL
data_gal_new_1$PLOT_ID_IFN2 <- NULL
data_gal_new <- rbind(data_gal_new, data_gal_new_1)

data_normal_new <- merge(data_normal, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN2')
data_normal_new_1 <- merge(data_normal, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_IFN3')
data_normal_new$PLOT_ID_IFN3 <- NULL
data_normal_new_1$PLOT_ID_IFN2 <- NULL
data_normal_new <- rbind(data_normal_new, data_normal_new_1)

data_mix_new <- merge(data_mix, IFN3_coord, by.x = 'ID_to_join', by.y = 'PLOT_ID_mix')


head(data_mix_new)
head(data_mix_new)

































# rename information to make it readable by SIMANFOR

data_cat_new <- rename(data_cat_new, c(
  'LONGITUDE' = 'CoorX',
  'LATITUDE' = 'CoorY'
))
data_gal_new <- rename(data_gal_new, c(
  'LONGITUDE' = 'CoorX',
  'LATITUDE' = 'CoorY'
))
data_normal_new <- rename(data_normal_new, c(
  'LONGITUDE' = 'CoorX',
  'LATITUDE' = 'CoorY'
))
data_mix_new$LONGITUDE <- NULL
data_mix_new$LATITUDE <- NULL
data_mix_new <- rename(data_mix_new, c(
  'LONGITUDE' = 'CoorX',
  'LATITUDE' = 'CoorY'
))

# save new dataframes

write.csv(data_cat_new, '/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/plots_pure_Cat_IFN.csv')
write.csv(data_normal_new, '/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/plots_pure_IFN.csv')
write.csv(data_gal_new, '/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/plots_pure_Gal_IFN.csv')

data_mix_trees <- read_excel('/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/original_db/Mix.xlsx', sheet = "PiesMayores")
wb <- createWorkbook(creator = "Aitor")
  saveWorkbook(wb, file = "/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/Mix.xlsx", overwrite = TRUE)
renameWorksheet(wb, 1, "Parcelas")
addWorksheet(wb, "PiesMayores")

writeData(wb,
          "Parcelas", 
          data_mix_new,
          startCol = 1,
          startRow = 1,
          xy = NULL,
          colNames = TRUE,
          rowNames = FALSE)
writeData(wb,
          "PiesMayores", 
          data_mix_trees,
          startCol = 1,
          startRow = 1,
          xy = NULL,
          colNames = TRUE,
          rowNames = FALSE)

saveWorkbook(wb, file = "/media/aitor/HDD/iuFOR_trabajo/Proyectos/CROSSFOREST/inventarios_SM4/definitivos/Mix.xlsx", overwrite = TRUE)
