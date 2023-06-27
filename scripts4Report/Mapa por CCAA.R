################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
##    
##    File name: NFICalcFinal
##    Purpose: Read result of script "NFICalcFinal.r" which select plurispecific and 
##             multistratificated monoespecific plots from  Spanish NFI plots and 
##             make maps with them
##    
################################################################################
rm(list=ls())
## load library
## carga de paquetes necesarios
library(tidyverse)
library(sp)
library(ggplot2)
library(rnaturalearth)
library(tmap)
library(leaflet)

## work folder
getwd()
## change to data folder
## setwd("/home/cristobal/github/COMFOR.NFI/data/")
## setwd("/home/cristobal/Downloads")
## setwd("C:/Users/Irene/Documents/INF COMFOR/")
## setwd("~/comfor/")
## setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
dir()
ls()

################################################################################
#Seleccion de la ccaa
################################################################################
#Nombre de ccaa
ccaa="castillaLeon"

cProvincias<-c()
if(ccaa=="andalucia"){
  cProvincias<-c("4","11","14","18","21","23","29","41")
}else if(ccaa=="aragon"){
  cProvincias<-c("22","44","50")
}else if(ccaa=="asturias"){
  cProvincias<-c("33")
}else if(ccaa=="baleares"){
  cProvincias<-c("7")
}else if(ccaa=="canarias"){
  cProvincias<-c("35","38")
}else if(ccaa=="castillaLeon"){
  cProvincias<-c("5","9","24","34","37","40","42","47","49")
}else if(ccaa=="castillaLaMancha"){
  cProvincias<-c("2","13","16","19","45")
}else if(ccaa=="cataluña"){
  cProvincias<-c("8","17","25","43")
}else if(ccaa=="comunidadValenciana"){
  cProvincias<-c("3","12","46")
}else if(ccaa=="extremadura"){
  cProvincias<-c("6","10")
}else if(ccaa=="galicia"){
  cProvincias<-c("15","27","32","36")
}else if(ccaa=="madrid"){
  cProvincias<-c("28")
}else if(ccaa=="murcia"){
  cProvincias<-c("30")
}else if(ccaa=="navarra"){
  cProvincias<-c("31")
}else if(ccaa=="paisVasco"){
  cProvincias<-c("1","48","20")
}else if(ccaa=="laRioja"){
  cProvincias<-c("26")
}else if(ccaa=="ceuta"){
  cProvincias<-c("51")
}else if(ccaa=="melilla"){
  cProvincias<-c("52")
}


################################################################################
#Obtención únicamente de los datos de cada comunidad
################################################################################
datosCCAA<-function(cProvincias,datos){
  #Obtenemos el origen a partir de el plotID
  provincia<-c()
  for(i in 1:dim(datos)[1]){
    partes<-strsplit(datos$PlotID[i],"\\.")
    provincia<-c(provincia,partes[[1]][1])
  }
  datos$Origen<-provincia
  #Nos quedamos solo con las provincias pertenecientes a la ccaa que queremos representar
  dat<-c()
  for(i in 1:length(cProvincias)){
    dat<-rbind(dat,datos[datos$Origen==cProvincias[i],]) 
  }
  return(dat)
}

dir(path='../data')
dir_edition <- '../data/of_if3'
resultMD <- read.csv(paste0(dir_edition,'_resultHeightPlurimodal.csv'), row.names = 1)
resultMD<-datosCCAA(cProvincias,resultMD)

resultMDiamF<-read.csv(paste0(dir_edition,'_resultDiamPlurimodal.csv'), row.names = 1)
resultMDiamF<-datosCCAA(cProvincias,resultMDiamF)

plotsPluriSP<-read.csv(paste0(dir_edition,"_plotsPluriSP.csv"),row.names=1)
plotsPluriSP<-datosCCAA(cProvincias,plotsPluriSP)

plotsMonoSP <- read.csv(paste0(dir_edition,'_plotsMonoSP.csv'), row.names = 1)
plotsMonoSP <- datosCCAA(cProvincias,plotsMonoSP)

################################################################################
################################################################################
#        data read    /     Leemos los datos
################################################################################

## Plot coordinates from 3rd NFI edition
PlotCoord <- read.csv("../data/pcdatosmap.csv")
PlotCoord$PlotID <- paste0(PlotCoord$Provincia,".",PlotCoord$Estadillo)
names(PlotCoord)[c(7,8)]<-c('lng','lat') ## change coordinate label to use leaflet pkg
tail(PlotCoord); head(PlotCoord)

## Monospecific plots
plotsMonoSP$prov<-plotsMonoSP$Origen
provEst<-c()
for(i in 1:dim(plotsMonoSP)[1]){
   partes<-strsplit(plotsMonoSP$PlotID[i],split = "\\.")[[1]]
   resultado<-paste0(partes[1],".",partes[2])
   provEst<-c(provEst,resultado)
}
plotsMonoSP$PlotID0<-provEst
tail(plotsMonoSP); table(plotsMonoSP$prov); dim(table(plotsMonoSP$prov) )

## Pluriespecific plots
plotsPluriSP$prov<-plotsPluriSP$Origen
provEst<-c()
for(i in 1:dim(plotsPluriSP)[1]){
  partes<-strsplit(plotsPluriSP$PlotID[i],split = "\\.")[[1]]
  resultado<-paste0(partes[1],".",partes[2])
  provEst<-c(provEst,resultado)
}
plotsPluriSP$PlotID0<-provEst
tail(plotsPluriSP); table(plotsPluriSP$prov); dim(table(plotsPluriSP$prov) ) ## pluriespecific plots by province / Numero de parcelas mixtas que hay en cada provincia (hay alguna provincia sin datos)
 

## Combination of both dataframes
resultMModal <- merge( resultMD, resultMDiamF, by='PlotID', all=T)
resultMModal$prov <- with(
  resultMModal, as.numeric( substr( PlotID, 1, 1+str_locate(PlotID, '.') ) ) )
str(resultMModal); head(resultMModal, 20); table(resultMModal$prov) 

## Select plot plurimodal for both variables / Seleccionamos los estadillos que coinciden en ambas variables
positionPluri <- which( ( as.numeric(resultMModal$Pvalor.x) < 0.05 ) & ( as.numeric(resultMModal$Pvalor.y) < 0.05 ) )
plotsPluriMod <- resultMModal[ positionPluri, ]

provEst<-c()
for(i in 1:dim(plotsPluriMod)[1]){
  partes<-strsplit(plotsPluriMod$PlotID[i],split = "\\.")[[1]]
  resultado<-paste0(partes[1],".",partes[2])
  provEst<-c(provEst,resultado)
}
plotsPluriMod$PlotID0<-provEst

head(plotsPluriMod); tail(plotsPluriMod); dim(plotsPluriMod); any(is.na(resultMModal$Pvalue.y))

## Select complementary to plurimodal inside the original dataframe 
plotsMonoMd <- resultMModal[ -positionPluri, ]
dim(plotsMonoMd); dim(resultMModal)

## plurimodal plots by province / Numero de especies que se estudian en cada provincia (hay alguna provincia sin datos)
table(plotsPluriMod$prov) 
table(plotsMonoMd$prov) 



################################################################################
#                               MAPS
################################################################################

ls()

## PARCELAS MIXTAS
PlotsCoord_mix  <- PlotCoord[PlotCoord$PlotID %in% plotsPluriSP$PlotID0,]
head(PlotsCoord_mix)

## PARCELAS MONOESPECÍFICAS plurimodales
PlotsCoord_plmo <- PlotCoord[PlotCoord$PlotID %in% plotsPluriMod$PlotID0, ]
head(PlotsCoord_plmo)


## rest of the plots
PlotID_restofplots <- plotsMonoSP[ !(plotsMonoSP$PlotID0 %in% plotsPluriMod$PlotID0) ,
                                   'PlotID0']
PlotsCoord_mono <- PlotCoord[PlotCoord$PlotID %in% PlotID_restofplots, ]
head(PlotsCoord_mono)

names(PlotCoord)
summary(PlotCoord$lat)
summary(PlotCoord$lng)

#Mapa de España
spain <- ne_states(country = "spain",returnclass = "sf")

## all plots with complex forest
spain %>%
  ggplot()+
  geom_sf()+
  geom_point(data = PlotsCoord_mix, color="blue",aes(x=lng,y=lat,group=NULL),pch=2)+
  geom_point(data = PlotsCoord_plmo, color="red",aes(x=lng,y=lat,group=NULL),pch=1)

## only plot pluriespecific
spain %>%
  ggplot()+
  geom_sf()+
  geom_point(data = PlotsCoord_mix, color="blue",aes(x=lng,y=lat,group=NULL),pch=2)

## only monoespecific pluriestrata
spain %>%
  ggplot()+
  geom_sf()+
  geom_point(data = PlotsCoord_plmo, color="red",aes(x=lng,y=lat,group=NULL),pch=1)


## Interactive map / Mapa interactivo
## In this map appears all NFI plots, not only complex forest plots, to allow comparison
leaflet() %>%
  addTiles() %>%
  setView(lng = -3.7, lat = 40.4, zoom = 5) %>%
  addCircles(data = PlotsCoord_mono, radius = 10, color = "grey") %>%
  addCircles(data = PlotsCoord_plmo, radius = 10, color = "green") %>%
  addCircles(data = PlotsCoord_mix, radius = 10, color = "blue")



q()
n

