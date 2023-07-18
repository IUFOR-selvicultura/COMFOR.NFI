rm(list=c())
library(ggplot2)
library(sp)
library(rnaturalearth)
################################################################################
##
##                 Leemos los datos que necesitamos
##
################################################################################
setwd("C:/Users/Irene/Documents/COMFOR.NFI2")
## Inventario 
id <- 3
## Ruta de los datos
dir <- './data/'

lecturaDatos<-function(id){
  ## lectura de parcelas
  resultMD     <- read.csv(paste0(dir,'of_if',as.character(id), '_resultHeightPlurimodal.csv'), row.names = 1)
  resultMDiamF <- read.csv(paste0(dir,'of_if',as.character(id), '_resultDiamPlurimodal.csv'), row.names = 1)
  plotsPluriSP <- read.csv(paste0(dir,'of_if',as.character(id), "_plotsPluriSP.csv"),row.names=1)
  plotsMonoSP  <- read.csv(paste0(dir,'of_if',as.character(id), '_plotsMonoSP.csv'), row.names = 1)
  
  return(list(resultMD,resultMDiamF,plotsPluriSP,plotsMonoSP))
}

## Plot coordinates from 3rd NFI edition
PlotCoord <- read.csv(paste0(dir,"if3_coordLT.csv"))
PlotCoord$PlotID <- PlotCoord$PlotID0
names(PlotCoord)[c(8,9)] <- c('lng','lat') ## change coordinate label to use leaflet pkg

## Nos quedamos con las columnas que nos interesan
PlotCoord<-PlotCoord[,c("PlotID","Provincia","lng","lat")]

################################################################################
##
##                    Preparamos los datos
##
################################################################################

prepararDatos<-function(id){
  
  resultMD<-lecturaDatos(id)[[1]]
  resultMDiamF<-lecturaDatos(id)[[2]]
  plotsPluriSP<-lecturaDatos(id)[[3]]
  plotsMonoSP<-lecturaDatos(id)[[4]]
  
  
  ## PLURIESPECIFICAS
  plotsPluriSPi<-plotsPluriSP[1]
  plotsPluriSPi$Tipo<-"Pluriespecificas"
  
  ## PLURIMODALES
  resultMModal<-merge(resultMD,resultMDiamF,by='PlotID',all=T)
  positionPluri<-which((as.numeric(resultMModal$Pvalor.x)<0.05)&(as.numeric(resultMModal$Pvalor.y)<0.05))
  plotsPluriMod<-resultMModal[positionPluri,]
  plotsPluriMod<-plotsPluriMod[1]
  plotsPluriMod$Tipo<-"Plurimodales"
  
  ## MONOESPECIFICAS
  plotsMonoMd<-resultMModal[-positionPluri,]
  plotsMonoMd<-plotsMonoMd[1]
  plotsMonoSP<-plotsMonoSP[1]
  plotsMonoSPi<-rbind(plotsMonoSP,plotsMonoMd)
  plotsMonoSPi$Tipo<-"Monoespecificas"
  
  ## Unimos los datos
  parcelas<-rbind(plotsPluriSPi,plotsPluriMod,plotsMonoSPi)
  
  ## Nos quedamos con un PLOTID que contenga solo parcela y estadillo
  PlotID <- c()
  Provincia<-c()
  for(i in 1:dim(parcelas)[1]){
    partes <- strsplit( as.character( parcelas$PlotID[i] ), split = "\\.")[[1]]
    resultado <- paste0(partes[1],".",partes[2])
    prov<-partes[1]
    PlotID <- c(PlotID,resultado)
    Provincia<-c(Provincia,prov)
  }
  parcelas$PlotID<-PlotID
  parcelas$Provincia<-Provincia
  
  return(parcelas)
}

################################################################################
##
##                            Selección CCAA
##
################################################################################
## lista de nombres de CCAA
cNamesCCAA <- c("Andalucia", "Aragon"        , "Asturias", "Baleares"     , "canarias"   , "Cantabria"     ,
                "Cast-León", "Cast-La Mancha", "Cataluña", "C. Valenciana", "Extremadura", "Galicia"       ,
                "Madrid"   , "Murcia"        , "Navarra" , "País Vasco"   , "La Rioja"   ) 
## lista de provincias de cada CCAA
cProvinciasCCAA <- list(c("4" ,"11","14","18","21","23","29","41"),                    
                        c("22","44","50"),
                        c("33"),
                        c("7"),
                        c("35","38"),
                        c("39"),
                        c("5","9","24","34","37","40","42","47","49"),
                        c("2","13","16","19","45"),
                        c("8","17","25","43"),
                        c("3","12","46"),
                        c("6","10"),
                        c("15","27","32","36"),
                        c("28"),
                        c("30"),
                        c("31"),
                        c("1","48","20"),
                        c("26") )

################################################################################
##
##                          Datos CCAA
##
################################################################################
CCAA<-function(provincias){
  datCCAA<-c()
  for(i in 1:length(provincias)){
    datCCAA <- rbind(datCCAA, parcelas[parcelas$Provincia == provincias[i],]) 
  }
  return(datCCAA)
}

################################################################################
##
##                                MAPA
##
################################################################################

representacionMapa<-function(datos){
  
  parcelasCoord<-merge(PlotCoord,datos,by="PlotID")
  
  ## Limites para representar la CCAA
  maxCoordX <- max(parcelasCoord$lng) #Longitud
  minCoordX <- min(parcelasCoord$lng)
  maxCoordY <- max(parcelasCoord$lat) #Latitud
  minCoordY <- min(parcelasCoord$lat) 
  
  ## Ordenamos las parcelas por tipo
  parcelasCoord<-parcelasCoord[order(parcelasCoord$Tipo),]
  
  ## Representacion mapa
  
  ## Ayuda en: https://luisdva.github.io/rstats/mapassf/
  ## mapa de Europa
  Europa <- ne_countries(continent = "Europe", returnclass = "sf", scale = "medium")
  ## filtrar por país
  #Espanna <- Europa %>% filter(sovereignt=="Spain")
  Espanna<-ne_states(country = "spain", returnclass = "sf")
  
  # figura 
  colores<-c("Monoespecificas"="grey","Pluriespecificas"="blue","Plurimodales"="green")
  mapa_CCAA <- 
    ggplot()+
    geom_sf(data=Espanna)+
    geom_point(data = parcelasCoord, aes(x=lng,y=lat,color=Tipo),pch=16)+
    coord_sf(xlim = c(minCoordX, maxCoordX), 
             ylim = c(minCoordY, maxCoordY))+
    scale_color_manual(values = colores)+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(mapa_CCAA)
}



################################################################################
##
##           Mapa de la representación de las parcelas de cada CCAA
##
################################################################################

################################################################################
##                                IFN2
################################################################################
parcelas<-prepararDatos(2)

# Andalucia
provincias<-unlist(cProvinciasCCAA[1])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Aragon  
provincias<-unlist(cProvinciasCCAA[2])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Asturias
provincias<-unlist(cProvinciasCCAA[3])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Baleares
# provincias<-unlist(cProvinciasCCAA[4])
# datCCAA<-CCAA(provincias)
# representacionMapa(datCCAA)
# Canarias
provincias<-unlist(cProvinciasCCAA[5])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cantabria     
provincias<-unlist(cProvinciasCCAA[6])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-León 
provincias<-unlist(cProvinciasCCAA[7])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-La Mancha
provincias<-unlist(cProvinciasCCAA[8])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cataluña
provincias<-unlist(cProvinciasCCAA[9])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# C. Valenciana
provincias<-unlist(cProvinciasCCAA[10])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Extremadura
provincias<-unlist(cProvinciasCCAA[11])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Galicia
provincias<-unlist(cProvinciasCCAA[12])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Madrid
provincias<-unlist(cProvinciasCCAA[13])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Murcia 
provincias<-unlist(cProvinciasCCAA[14])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Navarra
provincias<-unlist(cProvinciasCCAA[15])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# País Vasco
provincias<-unlist(cProvinciasCCAA[16])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# La Rioja 
provincias<-unlist(cProvinciasCCAA[17])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)

################################################################################
##                                 IFN3
################################################################################
parcelas<-prepararDatos(3)

# Andalucia
provincias<-unlist(cProvinciasCCAA[1])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Aragon  
provincias<-unlist(cProvinciasCCAA[2])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Asturias
provincias<-unlist(cProvinciasCCAA[3])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Baleares
provincias<-unlist(cProvinciasCCAA[4])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Canarias
provincias<-unlist(cProvinciasCCAA[5])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cantabria     
provincias<-unlist(cProvinciasCCAA[6])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-León 
provincias<-unlist(cProvinciasCCAA[7])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-La Mancha
provincias<-unlist(cProvinciasCCAA[8])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cataluña
provincias<-unlist(cProvinciasCCAA[9])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# C. Valenciana
provincias<-unlist(cProvinciasCCAA[10])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Extremadura
provincias<-unlist(cProvinciasCCAA[11])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Galicia
provincias<-unlist(cProvinciasCCAA[12])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Madrid
provincias<-unlist(cProvinciasCCAA[13])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Murcia 
provincias<-unlist(cProvinciasCCAA[14])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Navarra
provincias<-unlist(cProvinciasCCAA[15])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# País Vasco
provincias<-unlist(cProvinciasCCAA[16])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# La Rioja 
provincias<-unlist(cProvinciasCCAA[17])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)

################################################################################
##                                 IFN4
################################################################################
parcelas<-prepararDatos(4)

# Andalucia
# provincias<-unlist(cProvinciasCCAA[1])
# datCCAA<-CCAA(provincias)
# representacionMapa(datCCAA)
# Aragon  
# provincias<-unlist(cProvinciasCCAA[2])
# datCCAA<-CCAA(provincias)
# representacionMapa(datCCAA)
# Asturias
provincias<-unlist(cProvinciasCCAA[3])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Baleares
provincias<-unlist(cProvinciasCCAA[4])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Canarias
provincias<-unlist(cProvinciasCCAA[5])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cantabria     
provincias<-unlist(cProvinciasCCAA[6])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-León 
provincias<-unlist(cProvinciasCCAA[7])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Cast-La Mancha
# provincias<-unlist(cProvinciasCCAA[8])
# datCCAA<-CCAA(provincias)
# representacionMapa(datCCAA)
# Cataluña
provincias<-unlist(cProvinciasCCAA[9])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# C. Valenciana
# provincias<-unlist(cProvinciasCCAA[10])
# datCCAA<-CCAA(provincias)
# representacionMapa(datCCAA)
# Extremadura
provincias<-unlist(cProvinciasCCAA[11])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Galicia
provincias<-unlist(cProvinciasCCAA[12])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Madrid
provincias<-unlist(cProvinciasCCAA[13])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Murcia 
provincias<-unlist(cProvinciasCCAA[14])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# Navarra
provincias<-unlist(cProvinciasCCAA[15])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# País Vasco
provincias<-unlist(cProvinciasCCAA[16])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)
# La Rioja 
provincias<-unlist(cProvinciasCCAA[17])
datCCAA<-CCAA(provincias)
representacionMapa(datCCAA)










