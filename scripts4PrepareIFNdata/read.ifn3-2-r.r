## Script para leer bases de datos .mdb del IFN3

rm(list=ls())


## Usar librería 'Hmisc 'para importar las extensiones de MS access
libraries <- c("readxl", "dplyr", "openxlsx", 'rgdal')
packages <-c('Hmisc', 'dplyr' ,libraries)
## lapply(packages,  install.packages, character.only=TRUE)
lapply(packages, library, character.only=TRUE)



dir()
setwd('../ifn3/tables')

## read csv file of trees together
trees.if3 <- read.csv('pcmayores.csv')
str( trees.if3 )

## Change colname of one column
## data %>% rename(new_name1 = old_name1, new_name2 = old_name2, ....)
trees.if3 <- trees.if3 %>% rename( OrdenIFAnterior = OrdenIf2,
                      OrdenIFActual   = OrdenIf3,
                      ## Provincia       = Origen,
                      Distancia       = Distanci,
                      Diametro1       = Dn1,
                      Diametro2       = Dn2,
                      Altura          = Ht )

trees.if3$Provincia <- trees.if3$Origen
trees.if3$PlotID0 <- with(trees.if3, paste0( as.character(trees.if3$Provincia), '.', trees.if3$Estadillo) )
trees.if3$PlotID  <- with(trees.if3, paste0( as.character(trees.if3$PlotID0), '.', trees.if3$Cla, '.', trees.if3$Subclase) )

names(trees.if3)
trees.if3 <- trees.if3[c('PlotID0', 'PlotID', "Provincia", 'Origen',
                         "Estadillo", "Cla", "Subclase",
                         'nArbol', "OrdenIFActual", "OrdenIFAnterior",
                         "Rumbo", "Distancia", "DRed",
                         "Especie", 'Diametro1', 'Diametro2', 'Altura',
                         "Calidad", "Forma",
                         "ParEsp", "Agente", "Import", "Elemento", "Compara") ]
                      
unique(trees.if3$Provincia)




## Plot dataframe with plot species information

plots.sp.if3 <- read.csv( "pcespparc.csv" )
names( plots.sp.if3 )
nrow( plots.sp.if3 )

plots.sp.if3$Provincia <- plots.sp.if3$Origen
plots.sp.if3$PlotID0 <- with(plots.sp.if3, paste0( as.character(plots.sp.if3$Provincia), '.', plots.if3$Estadillo) )
plots.sp.if3$PlotID  <- with(plots.sp.if3, paste0( as.character(plots.sp.if3$PlotID0), '.', plots.sp.if3$Cla, '.', plots.sp.if3$Subclase) )





## Plot dataframe with survey information

## put data.frames into list
plots.if3 <- read.csv("pcparcelas.csv")

str( plots.if3 )
names( plots.if3 )

plots.if3 <- plots.if3 %>% rename( CoorY = Coory,
                                   Rocosidad   = Rocosid)

plots.if3$PlotID0 <- with(plots.if3, paste0( as.character(plots.if3$Provincia), '.', plots.if3$Estadillo) )
plots.if3$PlotID  <- with(plots.if3, paste0( as.character(plots.if3$PlotID0), '.', plots.if3$Cla, '.', plots.if3$Subclase) )






maps.if3 <- read.csv( "pcdatosmap.csv" )
names( maps.if3 )
head( maps.if3)
maps.if3$PlotID0 <- with(maps.if3, paste0( as.character(maps.if3$Provincia), '.', maps.if3$Estadillo) )
maps.if3$PlotID  <- with(maps.if3, paste0( as.character(maps.if3$PlotID0), '.', maps.if3$Cla, '.', maps.if3$Subclase) )





# transform plot data into spatial dataframe
# ED50 / UTM zone 30N   <23030>
coord <- maps.if3[, c('Provincia', 'PlotID0', 'CoorX', 'CoorY', 'Origen')]
head(coord)

CRS_28 <- "+proj=utm +zone=28 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_29 <- "+proj=utm +zone=29 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_30 <- "+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
CRS_31 <- "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
## CRS_1 <- "+proj=utm +zone=30 +ellps=intl +units=m +towgs84=-87,-98,-121,0,0,0,0"  # utm gs84 
## CRS_2 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"                 # wgs84 
coord$huso <- with(coord,
                          case_when( 
                            Origen %in% c(38, 35) ~ 28, #huso 28: Canarias
                            Origen %in% c(15, 27, 36, 32, 21)  ~ 29, #huso 29: Galicia y Huelva (casi seguro que no tiene nada en el 30)
                            Origen %in% c(33, 24, 49, 37, 10, 6, 41, 11) ~ ifelse(CoorX>500000, 29, 30), # Husos 29 y 30: Asturias, León, Zamora, Salamanca, Extremadura (ambas provincias), Sevilla y Cádiz
                            Origen %in% c(31, 39, 1, 20, 48, 47, 9, 34, 42, 5, 40, 26, 28, 2, 13, 16, 19, 45, 30, 4, 14, 18, 23, 29, 51)  ~ 30, #Huso 30: Cantabria, Pais Vasco, Castilla y León (salvo las 3 provincias leonesas), La Rioja, Madrid, Castilla La Mancha, Murcia, Andalucía (Salvo las 3 provincias más occidentales mencionadas)
                            Origen %in% c(46, 12, 3, 22, 50, 44) ~ ifelse(CoorX>500000, 30, 31), # Huso 30 y 31: Comunidad Valenciana y Aragon
                            Origen %in% c(8, 25, 17, 43, 7)  ~ 31,# huso 31:Cataluña y Baleares
                          )
)

## names(coord)
head(coord)
str(coord)

summary(coord$huso)
## summary(plot3.datmap$CoorX)
## table(plot3.datmap$Origen, plot3.datmap$huso)

coord28 <- coord[coord$huso==28,]
coord29 <- coord[coord$huso==29,]
coord30 <- coord[coord$huso==30,]
coord31 <- coord[coord$huso==31,]

## convertir coordenadas en lista de puntos en formato coordenadas de R
coord.spt28 <- SpatialPointsDataFrame(coord28[,c('CoorX','CoorY')], coord28, proj4string = CRS(CRS_28) )
coord.spt29 <- SpatialPointsDataFrame(coord29[,c('CoorX','CoorY')], coord29, proj4string = CRS(CRS_29) )
coord.spt30 <- SpatialPointsDataFrame(coord30[,c('CoorX','CoorY')], coord30, proj4string = CRS(CRS_30) )
coord.spt31 <- SpatialPointsDataFrame(coord31[,c('CoorX','CoorY')], coord31, proj4string = CRS(CRS_31) )

head(coord.spt30)

coordLT28 <- spTransform(coord.spt28, CRS("+proj=longlat +ellps=WGS84")) # convertimos el spatial dataframe "coord.spt28" que esta en coordenadas planas a sptatial dataframe en coordenadas globales Longitud/Latitud "dataLT"
coordLT29 <- spTransform(coord.spt29, CRS("+proj=longlat +ellps=WGS84"))
coordLT30 <- spTransform(coord.spt30, CRS("+proj=longlat +ellps=WGS84"))
coordLT31 <- spTransform(coord.spt31, CRS("+proj=longlat +ellps=WGS84"))
coordLT.if3 <- rbind(coordLT28, coordLT29, coordLT30, coordLT31)
head(coordLT.if3)


## ponemos coordenadas a las parcelas que no tienen
plots.if3 <- merge( plots.if3, maps.if3[ , c("PlotID0", "CoorX",  "CoorY", 'long', 'lat') ], all.x=TRUE )



ls()
str(trees.if3); str(plots.if3); str(maps.if3); str(plots.sp.if3)

save(trees.if3, plots.if3, plots.sp.if3, maps.if3, coordLT.if3, file='../../if3.RData')
