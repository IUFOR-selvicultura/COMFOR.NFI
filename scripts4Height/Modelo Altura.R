################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Depuracion datos modelos 
##    Purpose: Depurar y completar los datos para obtener las ecuaciones de los
##             posteriores modelos
##    
################################################################################
rm(list=ls())
library(dplyr)
library(ggplot2)

#Se necesitan los datos del IFN2, IFN3 y IFN4 y la mezcla entre ellos

################################################################################
#                               IFN2
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
combi4172_2<-read.csv("arbolesCombi4172C.csv")
combi4172_2$PlotID0<-with(combi4172_2, paste0(as.character(combi4172_2$Origen), ".", as.character(combi4172_2$Estadillo) ) )

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## numOrden
combi4172_2$ArbolID<-with(combi4172_2,paste0(as.character(combi4172_2$Origen),".",as.character(combi4172_2$Estadillo),".",as.character(combi4172_2$NumOrden)))

## Eliminamos columnas que no aportan información
combi4172_2<-combi4172_2[,-c(1,2)]

## Necesitamos tener en cuenta el año de toma de los datos de esa especie para 
## hallar el incremento anual para que tenga una mayor precisión
## Tenemos los datos de los árboles en las parcelas de la combinacion IFN2-IFN3

################################################################################
#                           DATOS DE PARCELAS
################################################################################
setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID %in% combi4172_2$PlotID )
parcelas2<- as.data.frame( parcelasPorcentage[ posicion, ])
## El identificador sin espacios para que tenga el mismo formato que el del IFN3
parcelas2$PlotID0<-gsub(" ","",parcelas2$PlotID)
## Eliminamos las columnas innecesarias
parcelas2<-parcelas2[,-c(1,6,7)]


################################################################################
#     Añadimos variables que serán de interés para los modelos de regresión
################################################################################
## Tener en cuenta que g => mm^2    y      altura => m

## ((1))) Otra variable explicativa que interesa es la que se obtiene del producto de G 
## por la altura que es una aproximación al volumen
combi4172_2$ProxVol<-with(combi4172_2,Altura*(g/1000000))

################################################################################
#          Añadimos variables necesarias para las parcelas
################################################################################
## ((2)) Dg
parcelas2$Dg<-with(parcelas2,sqrt((4*G)/(N*pi))*100)
## ((3)) SDI
parcelas2$SDI<-with(parcelas2,N*(Dg/25)^(1.605))


################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################
## Eliminamos las parcelas con menos de 100 arboles, ya que no van a ser
## representativas
parcelas2<-parcelas2[-which(parcelas2$N<100),]

## ((4)) Calculamos el BAL
##a--Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
combi4172_2<-combi4172_2[order(combi4172_2$PlotID0,combi4172_2$dbh,decreasing = T),]
##b--BAL 
BALTotal<-c()
for(i in 1:length(unique(combi4172_2$PlotID0))){
  dat<-combi4172_2[combi4172_2$PlotID0==unique(combi4172_2$PlotID0)[i],]
  aux<-cumsum(c(0,dat$g_ha[-length(dat$g_ha)]))
  BALTotal<-c(BALTotal,aux)
}
combi4172_2$BALTotal<-BALTotal

## ((5)) Calculamos la altura dominante
alturaDominante<-c()
for(i in 1:length(unique(combi4172_2$PlotID0))){
  dat<-combi4172_2[combi4172_2$PlotID0==unique(combi4172_2$PlotID0)[i],]
  dat$H<-with(dat,(Altura*expan))
  dat$HCumsum<-cumsum(dat$H)
  dat$ExpanCumsum<-cumsum(dat$expan)
  ecPos<-which(dat$ExpanCumsum>100)[1]
  ecVal<-dat[ecPos,]$ExpanCumsum
  mas100<-ecVal-100
  dat$Ho<-with(dat,ifelse((HCumsum<HCumsum[ecPos]),HCumsum,0))
  dat$Ho[ecPos]<-dat$Altura[ecPos]*(dat$expan[ecPos]-mas100)+dat$Ho[ecPos-1]
  dat$AlturaDominante<-max(dat$Ho)/100
  #sum(dat$H)/sum(dat$expan.x)
  altDom<-dat$AlturaDominante
  alturaDominante<-c(alturaDominante,altDom)
}
combi4172_2$AlturaDominante<-alturaDominante

alturaDiam<-merge(combi4172_2, parcelas2)

## ((6)) Indice Hart BecKing
alturaDiam$HartB<-with(alturaDiam,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDominante)))

## ((7)) Indice Hart 
alturaDiam$Hart<-with(alturaDiam,(10000/(sqrt(N)*AlturaDominante)))

################################################################################
#                    Reescalamos las variables
################################################################################
## Pasamos de mm^2 a cm^2
alturaDiam$g<-alturaDiam$g/100

################################################################################
#
################################################################################
write.csv(alturaDiam,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Altura/alturaDiam.csv")



