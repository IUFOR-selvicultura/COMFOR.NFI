## Script para leer bases de datos .accdb del IFN4

rm(list=ls())

## Usar librería 'Hmisc 'para importar las extensiones de MS access
libraries <- c("readxl", "dplyr", "openxlsx", 'rgdal')
packages <-c('Hmisc' , 'dplyr', libraries)
## lapply(packages,  install.packages, character.only=TRUE)
lapply(packages, library, character.only=TRUE)

dir()
setwd('../ifn4/data-raw')
## files <- dir(pattern='*.accdb')
files <- dir(pattern='Ifn4*')
ifn4 <- list()


## lectura de datos del ifn4
for (i in 1:length(files)){
    ifn4[[i]] <- mdb.get(files[i])
}


## put data.frames into list
pcmayores <- list()
for (i in 1:length(files)){
    pcmayores[[i]] <- ifn4[[i]][["PCMayores" ]]
}
## get all variable names
allNms <- unique( unlist(lapply( pcmayores, names ) ) )
## put em all together
trees.if4 <- do.call( rbind,                                    
        c( lapply( pcmayores,
                 function(x) data.frame( c( x, sapply( setdiff( allNms, names(x) ),
                                                    function(y) NA)))),
          make.row.names = FALSE ) )
str( trees.if4 )

## Change colname of one column
## data %>% rename(new_name1 = old_name1, new_name2 = old_name2, ....)
trees.if4 <- trees.if4 %>% rename( OrdenIFAnterior = OrdenIf3,
                      OrdenIFActual   = OrdenIf4,
                      Distancia       = Distanci,
                      Diametro1       = Dn1,
                      Diametro2       = Dn2,
                      Altura          = Ht )
trees.if4$Origen <- trees.if4$Provincia
trees.if4$PlotID0 <- with(trees.if4, paste0( as.character(trees.if4$Provincia), '.', trees.if4$Estadillo) )
trees.if4$PlotID  <- with(trees.if4, paste0( as.character(trees.if4$PlotID0), '.', trees.if4$Cla, '.', trees.if4$Subclase) )

names(trees.if4)
trees.if4 <- trees.if4[c('PlotID0', 'PlotID', "Provincia", 'Origen',
                         "Estadillo", "Cla", "Subclase",
                         'nArbol', "OrdenIFActual", "OrdenIFAnterior",
                         "Rumbo", "Distancia", 
                         "Especie", 'Diametro1', 'Diametro2', 'Altura', 'Hcopa',
                         "Calidad", "Forma",
                         "ParEsp", 'NPae', 'CPae', 'VPae',
                         "Agente", "Import", "Elemento") ]

unique(trees.if4$Provincia)






plots.if4 <- ifn4[[1]] [[ "PCParcelas" ]]
for (i in 2 : ( length(files) ) ){
    plots.if4 <- rbind( plots.if4, ifn4[[i]] [[ "PCParcelas" ]] )
    }
str(plots.if4)
names(plots.if4)
head(plots.if4)

plots.if4 <- plots.if4 %>% rename( Rocosidad   = Rocosid)
plots.if4$Origen <- plots.if4$Provincia
plots.if4$PlotID0 <- with(plots.if4, paste0( as.character(plots.if4$Provincia), '.', plots.if4$Estadillo) )
plots.if4$PlotID  <- with(plots.if4, paste0( as.character(plots.if4$PlotID0), '.', plots.if4$Cla, '.', plots.if4$Subclase) )





plots.sp.if4 <- ifn4[[1]] [[ "PCEspParc" ]]
for (i in 2 : ( length(files) ) ){
    plots.sp.if4 <- rbind( plots.sp.if4, ifn4[[i]] [[ "PCEspParc" ]] )    
    }


plots.sp.if4$Origen <- plots.sp.if4$Provincia
plots.sp.if4$PlotID0 <- with(plots.sp.if4, paste0( as.character(plots.sp.if4$Provincia), '.', plots.sp.if4$Estadillo) )
plots.sp.if4$PlotID  <- with(plots.sp.if4, paste0( as.character(plots.sp.if4$PlotID0), '.', plots.sp.if4$Cla, '.', plots.sp.if4$Subclase) )
head(plots.sp.if4)






pcdatosmap <- list()
for (i in 1:length(files)) {
    pcdatosmap[[i]] <- ifn4[[i]][[ "PCDatosMap" ]]
}
## get all variable names
allNms <- unique( unlist(lapply( pcdatosmap, names ) ) )
## put em all together
maps.if4 <- do.call( rbind,                                    
        c( lapply( pcdatosmap,
                 function(x) data.frame( c( x, sapply( setdiff( allNms, names(x) ),
                                                    function(y) NA)))),
          make.row.names = FALSE ) )
str( maps.if4 )
names( maps.if4 )
head( maps.if4 )
maps.if4$Origen <- maps.if4$Provincia
maps.if4$PlotID0 <- with(maps.if4, paste0( as.character(maps.if4$Provincia), '.', maps.if4$Estadillo) )
## maps.if4$PlotID  <- with(maps.if4, paste0( as.character(maps.if4$PlotID0), '.', maps.if4$Cla, '.', maps.if4$Subclase) )

maps.if4[maps.if4$PlotID0==47.242,'CoorY'] <- 4605000 ## correccion de la coordenada Y de una parcela con el valor del IFN3





# transform plot data into spatial dataframe
# ED50 / UTM zone 30N   <23030>
coord <- maps.if4[, c('Provincia', 'PlotID0', 'CoorX', 'CoorY', 'Origen')]
head(coord)

## https://epsg.io/?q=spain+kind%3APROJCRS+ed50+31N
## Dátum ED50 en Navarra, Galicia, Asturias,Cantabria, Murcia, Islas Baleares, País  Vasco, La Rioja,Madrid y Cataluña.
## Dátum ETRS89 en el resto de provincias o CC.AA. peninsulares.
## En Canarias: Dátum WGS84.

coord$datum <- with(coord,
                          case_when( 
                            Origen %in% c(38, 35) ~ 'WGS84', # Canarias
                            Origen %in% c(15, 27, 36, 32,    # Galicia
                                          33,                # Asturias
                                          31,                # Navarra
                                          39,                # Cantabria
                                          1, 20, 48,         # Pais Vasco
                                          26,                # Rioja
                                          28,                # Madrid
                                          30,                # Murcia
                                          8, 25, 17, 43,     # Cataluña
                                          7) ~ 'ED50',       # Baleares
                            Origen %in% c(24, 49, 37, 47, 9, 34, 42, 5, 40, # Cast y Leon
                                          10, 6,                            # Extremadura 
                                          41, 11, 21, 4, 14, 18, 23, 29,    # Andalucia
                                          2, 13, 16, 19, 45,                # Castilla La Mancha
                                          46, 12, 3,                        # Pais Valencia 
                                          50, 22, 44,                       # Aragon
                                          51, 52)  ~ 'ETRS89', #
                            )
                    )

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
summary(coord$CoorX)
summary(coord$CoorY)
## summary(plot3.datmap$CoorX)
## table(plot3.datmap$Origen, plot3.datmap$huso)

coord25828 <- coord[coord$huso==28,]
coord25829 <- coord[coord$datum=='ETRS89' & coord$huso==29,]
coord25830 <- coord[coord$datum=='ETRS89' & coord$huso==30,]
coord25831 <- coord[coord$datum=='ETRS89' & coord$huso==31,]
coord23029 <- coord[coord$datum=='ED50' & coord$huso==29,]
coord23030 <- coord[coord$datum=='ED50' & coord$huso==30,]
coord23031 <- coord[coord$datum=='ED50' & coord$huso==31,]

## https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
## convertir coordenadas en lista de puntos en formato coordenadas de R

if( nrow( coord25828 ) > 0) { coord.spt25828 <- SpatialPointsDataFrame(coord25828[,c('CoorX','CoorY')], coord25828, proj4string = CRS('+init=epsg:25828') ) }
if( nrow( coord25829 ) > 0) { coord.spt25829 <- SpatialPointsDataFrame(coord25829[,c('CoorX','CoorY')], coord25829, proj4string = CRS('+init=epsg:25829') ) }
if( nrow( coord25830 ) > 0) { coord.spt25830 <- SpatialPointsDataFrame(coord25830[,c('CoorX','CoorY')], coord25830, proj4string = CRS('+init=epsg:25830') ) }
if( nrow( coord25831 ) > 0) { coord.spt25831 <- SpatialPointsDataFrame(coord25831[,c('CoorX','CoorY')], coord25831, proj4string = CRS('+init=epsg:25831') ) }
if( nrow( coord23029 ) > 0) { coord.spt23029 <- SpatialPointsDataFrame(coord23029[,c('CoorX','CoorY')], coord23029, proj4string = CRS('+init=epsg:23029') ) }
if( nrow( coord23030 ) > 0) { coord.spt23030 <- SpatialPointsDataFrame(coord23030[,c('CoorX','CoorY')], coord23030, proj4string = CRS('+init=epsg:23030') ) }
if( nrow( coord23031 ) > 0) { coord.spt23031 <- SpatialPointsDataFrame(coord23031[,c('CoorX','CoorY')], coord23031, proj4string = CRS('+init=epsg:23031') ) }

if( nrow( coord25828 ) > 0) { coordLT25828 <- spTransform(coord.spt25828, CRS("+proj=longlat +ellps=WGS84")) } # convertimos el spatial dataframe "coord.spt28" que esta en coordenadas planas a sptatial dataframe en coordenadas globales Longitud/Latitud "dataLT"
if( nrow( coord25829 ) > 0) { coordLT25829 <- spTransform(coord.spt25829, CRS("+proj=longlat +ellps=WGS84")) }
if( nrow( coord25830 ) > 0) { coordLT25830 <- spTransform(coord.spt25830, CRS("+proj=longlat +ellps=WGS84")) }
if( nrow( coord25831 ) > 0) { coordLT25831 <- spTransform(coord.spt25831, CRS("+proj=longlat +ellps=WGS84")) }
if( nrow( coord23029 ) > 0) { coordLT23029 <- spTransform(coord.spt23029, CRS("+proj=longlat +ellps=WGS84")) }
if( nrow( coord23030 ) > 0) { coordLT23030 <- spTransform(coord.spt23030, CRS("+proj=longlat +ellps=WGS84")) }
if( nrow( coord23031 ) > 0) { coordLT23031 <- spTransform(coord.spt23031, CRS("+proj=longlat +ellps=WGS84")) }


coordLT.if4 <- data.frame()
if( nrow( coord25828 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT25828) } else {coordLT.if4 <- coordLT25828} }
if( nrow( coord25829 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT25829) } else {coordLT.if4 <- coordLT25829} }
if( nrow( coord25830 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT25830) } else {coordLT.if4 <- coordLT25830} }
if( nrow( coord25831 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT25831) } else {coordLT.if4 <- coordLT25831} }
if( nrow( coord23029 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT23029) } else {coordLT.if4 <- coordLT23029} }
if( nrow( coord23030 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT23030) } else {coordLT.if4 <- coordLT23030} }
if( nrow( coord23031 ) > 0) { if( nrow( coordLT.if4 ) >0 ) { coordLT.if4 <- rbind(coordLT.if4, coordLT23031) } else {coordLT.if4 <- coordLT23031} }

head(coordLT.if4)
summary(coordLT.if4$CoorY)

plot(coordLT.if4)

str(trees.if4)
str(plots.if4)
str(maps.if4)
str(plots.sp.if4)



save(trees.if4, coordLT.if4, plots.if4, plots.sp.if4, maps.if4, file='../../if4.RData')
