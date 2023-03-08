#+eval=FALSE 
##    https://cran.r-project.org/web/packages/basifoR/index.html
##    https://search.r-project.org/CRAN/refmans/basifoR/html/00Index.html
## rm(list=ls())
## install.packages('basifoR')
library('basifoR')
ls()
getwd()
setwd("~/Disk.IUGFS/proyectos/2020.COMFOR-SUDOE/IFN")

 help(urlToTemp)
      madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
      tfmad <- urlToTemp(madridNFI)
      tfmad
 
 help(readNFI)
      path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
      url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
      rnfi <- readNFI(url2)
      head(rnfi,3)
 
 help(dbhMetric)
      dbh <- dbhMetric(c(10.7, 11.5), 'h')# average tree height (dm) 
 
 help(nfiMetrics)
      ## seconf NFI
      madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
      rmad <- readNFI(madridNFI)[1:10,]
      mmad <- nfiMetrics(rmad)
      head(rmad,3)
      ## see metric units
      attr(rmad,'units')
 
 help(dendroMetrics)
      path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
      url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
      dmad <- dendroMetrics(url2, cut.dt = 'h >= 11')
      head(dmad)
      attr(dmad, 'units')
      
 help(metrics2Vol)
      madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
      rmad <- readNFI(madridNFI)[1:10,]
      vmad <- metrics2Vol(rmad)
      head(vmad)
      ## see metric units
      attr(vmad,'units')

############################################################
## Las operaciones básicas que deberemos realizar son:
## i) lectura de datos, 
                                        # store IFN url
url.ifn <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/"
## url.ifn <- "https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/"
## ifn4_cantabria_tcm30-536602.zip
                                        # reading CyL plots from ifn2
## dir(path='./data/ifn2/', pattern='*.zip')
file.9.if2 <- '_tcm30-.zip'
url.9.if2 <- paste0(url.ifn, file.9.if2)
trees.9.if2 <- readNFI(url.9.if2)
plots.9.if2 <- readNFI(url.9.if2, dt.nm = "DATEST")

                                        # reading Cantabria plots from ifn3
## dir(path='./data/ifn3/', pattern='*.zip')
file.9.if3 <- 'ifn3p09_tcm30-.zip'
url.9.if3 <- paste0(url.ifn, file.9.if3)
trees.9.if3 <- readNFI(url.9.if3, dt.nm = "PCMayores")
plots.9.if3 <- readNFI(url.9.if3, dt.nm = "PCParcelas")
maps.9.if3 <- readNFI(url.9.if3, dt.nm = "PCDatosMap")

                                        # reading Cantabria plots from ifn4
## dir(path='./data/ifn4/', pattern='*.zip')
file.39.if4 <- './data/ifn4/Ifn4p39.zip'
trees.39.if4 <- readNFI(file.39.if4, dt.nm = "PCMayores")
plots.39.if4 <- readNFI(file.39.if4, dt.nm = "PCParcelas")
maps.39.if4 <- readNFI(file.39.if4, dt.nm = "PCDatosMap")


                                        # reading Cantabria plots from ifn4 from the web
## dir(path='./data/ifn4/', pattern='*.zip')
file.39.if4 <- 'ifn4_cantabria_tcm30-536602.zip'
url.ifn4<-'https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/'
url.39.if4 <- paste0(url.ifn4, file.39.if4)
trees.39.if4 <- readNFI(url.39.if4, dt.nm = "PCMayores")
plots.39.if4 <- readNFI(url.39.if4, dt.nm = "PCParcelas")
maps.39.if4 <- readNFI(url.39.if4, dt.nm = "PCDatosMap")

str(trees.39.if4)

############################################################
## ii) cálculo de variables básicas de árbol y parcela,

###### para el IFN2

#### variables por parcela 
dendro.esta <- dendroMetrics(trees.39.if2, summ.vr = c('Esta') )#; str(dendro.esta)
## variables por parcela y especie
dendro.esta.esp <- dendroMetrics(trees.39.if2, summ.vr = c('Esta', 'espe') )#; str(dendro.esta.esp)
## unión de datos de parcela y datos por especie y parcela 
plot.esp.39.if2 <- merge( dendro.esta[,c(3:10)], dendro.esta.esp[,c(2:4,8:10)], by = c('ESTADILLO') )
## cálculo de porcentajes por especie en cada parcela
plot.esp.39.if2$p.n <-  with(plot.esp.39.if2, n.y*100/n.x)
plot.esp.39.if2$p.ba <- with(plot.esp.39.if2, ba.y*100/ba.x)
plot.esp.39.if2$p.v <-  with(plot.esp.39.if2, v.y*100/v.x)

#### variables por árbol
trees.vol.39.if2 <- metrics2Vol(trees.39.if2)

###### para el IFN3

#### variables por parcela 
dendro.esta <- dendroMetrics(trees.39.if3, summ.vr = c('Esta') )#; str(dendro.esta)
## variables por parcela y especie
dendro.esta.esp <- dendroMetrics(trees.39.if3, summ.vr = c('Esta', 'espe') )#; str(dendro.esta.esp)
## unión de datos de parcela y datos por especie y parcela 
plot.esp.39.if3 <- merge( dendro.esta[,c(3:10)], dendro.esta.esp[,c(2:4,8:10)], by = c('Estadillo') )#; str(plot.esp.39.if3)
## cálculo de porcentajes por especie en cada parcela
plot.esp.39.if3$p.n <-  with(plot.esp.39.if3, n.y*100/n.x)
plot.esp.39.if3$p.ba <- with(plot.esp.39.if3, ba.y*100/ba.x)
plot.esp.39.if3$p.v <-  with(plot.esp.39.if3, v.y*100/v.x)

#### variables por árbol
trees.vol.39.if3 <- metrics2Vol(trees.39.if3)

###### para el IFN4

#### variables por parcela 
dendro.esta <- dendroMetrics(trees.39.if4, summ.vr = c('Esta') )#; str(dendro.esta)
## variables por parcela y especie
dendro.esta.esp <- dendroMetrics(trees.39.if4, summ.vr = c('Esta', 'espe') )#; str(dendro.esta.esp)
## unión de datos de parcela y datos por especie y parcela 
plot.esp.39.if4 <- merge(dendro.esta[,c(3:10)], dendro.esta.esp[,c(2:4,8:10)], by = c('Estadillo') )
## cálculo de porcentajes por especie en cada parcela
plot.esp.39.if4$p.n <-  with(plot.esp.39.if4, n.y*100/n.x)
plot.esp.39.if4$p.ba <- with(plot.esp.39.if4, ba.y*100/ba.x)
plot.esp.39.if4$p.v <-  with(plot.esp.39.if4, v.y*100/v.x)

#### variables por árbol
trees.vol.39.if4 <- metrics2Vol(trees.39.if4)


getwd()

############################################################
## grabación de los cálculos para posteriores sesiones
## Save whole workspace to working directory
save.image("./data/savespace/all_data.RData")


############################################################
## recuperación de la imagen grabada
## restore whole workspace to working directory
rm(list=ls())
load("./data/savespace/all_data.RData")




############################################################
## iii) realización del corte o consulta de selección de datos 

## Podemos utilizar la función de basifoR con el parámetro 'cut.dt' para seleccionar 
## las parcelas que tengan un área basimétrica mayor de 15 m2/ha
cut.esta <- dendroMetrics(trees.39.if2, summ.vr = c('Esta'), cut.dt = 'ba > 15')
estadillos <- cut.esta[,c('ESTADILLO')]
## o las parcelas que incluyan pies de haya y área basimétrica mayor de 15 m2/ha
cut.esta.esp <- dendroMetrics(trees.39.if2, summ.vr = c('Esta', 'espe'), cut.dt = '(ba > 15)&(ESPECIE==71)')
estadillos <- cut.esta.esp[,c('ESTADILLO','ESPECIE')]

## o bien, para consultas más compleja podemos utilizar el resultado de los cálculos iniciales
## para seleccionar las parcelas con area basimétrica mayor de 15 m2/ha y
## con una proporción de haya superior al 75%
estadillos <- plot.esp.39.if2[(plot.esp.39.if2$ba.x > 15) & 
                              (plot.esp.39.if2$ESPECIE==71) &
                              (plot.esp.39.if2$p.ba >75) , c('ESTADILLO','ESPECIE') ] 

## En todos los casos tenemos un listado de las parcelas (ESTADILLO) seleccionadas



############################################################
## y iv) cáculo de variables y almacenamiento o análisis.

trees.if2 <- trees.39.if2[ trees.39.if2$ESTADILLO %in% estadillos$ESTADILLO,]; str(trees.if2)
met.trees.if2 <- nfiMetrics(trees.if2, levels = c('esta'), var = c('d','n','h','Hd')); str(met.trees.if2)
vol.trees.if2 <- metrics2Vol(trees.if2); str(vol.trees.if2)

cut.esta.esp <- dendroMetrics(trees.if2, summ.vr = c('Esta'))
plots.if2 <- plots.39.if2[ plots.39.if2$ESTADILLO %in% estadillos$ESTADILLO, ]; str(plots.if2)







############################################################
## Una última operación en el caso de trabajar con más de una edición será v) comparación de inventarios.

## Con el listado de parcelas obtenido por cualquiera de los dos métodos, seleccionamos los árboles incluidos en esas parcelas en las distintas ediciones del IFN

trees.if2 <- trees.39.if2[ trees.39.if2$ESTADILLO %in% estadillos$ESTADILLO, ]

trees.if3 <- trees.39.if3[ trees.39.if3$Estadillo %in% estadillos$ESTADILLO, ]





############################################################
## Finalmente podemos representar de forma gráfica las parcelas que hemos utilizado
plots.if2 <- plots.39.if2[ plots.39.if2$ESTADILLO %in% estadillos$ESTADILLO, ]
plots.if2$UTMx <- as.integer(plots.if2$COORDEX) * 1000
plots.if2$UTMy <- as.integer(plots.if2$COORDEY) * 1000
head(plots.if2)

plots.if3 <- plots.39.if3[ plots.39.if3$Estadillo %in% estadillos$ESTADILLO, ]
maps.if3  <- maps.39.if3[  maps.39.if3$Estadillo %in% estadillos$ESTADILLO, ]

## calculamos las coordenadas para representarlas
require('rgdal')      #librería necesaria para cambio de coordenadas
maps.if3.UTM<-SpatialPointsDataFrame(  
    maps.if3[,c('CoorX','CoorY')],  # campos con las coordenadas x e y
    maps.if3[,c("Provincia", "Estadillo", "Clase", "Subclase", "Hoja50", "CoorX", "CoorY", "FccArb")],
    proj4string=CRS("+init=epsg:23030"))  # indicamos el sistema de referencia original del IFN, ED1950
## Transformamor el spatial dataframe de coordenadas UTM a geográficas
maps.if3.LT<-spTransform(maps.if3.UTM,CRS("+proj=longlat +ellps=WGS84"))
## Convertimos el spatial dataframe a dataframe convencional para representarlo en un mapa
mapsdf.if3.LT <- as.data.frame(maps.if3.LT)

## Carga de paquetes para mapas y gráficos
packs <- c('maps', 'mapdata', 'ggplot2', 'ggmap'); sapply(packs, require, character.only = TRUE)

## Descargamos el mapa de relieve de Cantabria, por sus límites geográficos
base2 = get_map(location = c(-5.0, 42.75, -3, 43.5), maptype = "toner-background"); map2 = ggmap(base2)

## Creamos y exportamos un mapa con las parcelas a analizar
jpeg(file = "plots.map.jpeg", quality = 100, width = 1000, height = 577,#, width = 1360, height = 510,
     units = "px", pointsize = 12, bg = "transparent")
map2 + geom_point(data = mapsdf.if3.LT, mapping = aes(x = CoorX.1, y = CoorY.1), color = "red") +
    labs(title = "Parcelas analizadas", subtitle = "",caption = "Fuente: Mapa de relieve paquete 'maps'")
dev.off()
    





q()
n
