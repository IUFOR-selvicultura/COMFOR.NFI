################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Test Modelo Altura
##    Purpose: Modelo para obtener la ecuación de relacion altura-diámetro
##             
##    
################################################################################
rm(list=ls())

library(dplyr)
library(ggplot2)

#Se necesitan los datos del IFN3

################################################################################
#                               IFN3
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/IFN3/España")
combi4172_3<-read.csv("arbolesCombi4172.csv")

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## numOrden
combi4172_3$ArbolID<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo),".",as.character(combi4172_3$NumOrden)))
combi4172_3$PlotID0<-with(combi4172_3, paste0(as.character(combi4172_3$Origen), ".", as.character(combi4172_3$Estadillo) ) )

## Eliminamos columnas que no aportan información
#columnasEliminar<-c("X","DRed","ParEsp","Agente","Import","Elemento","Compara")
#combi4172_3<-select(combi4172_3,-one_of(columnasEliminar))

## Necesitamos tener en cuenta el año de toma de los datos de esa especie para 
## hallar el incremento anual para que tenga una mayor precisión
## Tenemos los datos de los árboles en las parcelas de la combinacion IFN2-IFN3

################################################################################
#                           DATOS DE PARCELAS
################################################################################
setwd("C:/Users/Irene/Documents/INF COMFOR/IFN3/España")
parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")

plotID0<-c()
## Añadimos el plotID0 a partir del plotID
for(i in 1:dim(parcelasPorcentage)[1]){
  partes <- unlist(strsplit(parcelasPorcentage$PlotID[i], "\\."))
  numero <- paste0(partes[1],".",partes[2])
  plotID0<-c(plotID0,numero)
}
parcelasPorcentage$PlotID0 <-plotID0


## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID0 %in% combi4172_3$PlotID0)
parcelas3<- as.data.frame( parcelasPorcentage[ posicion, ])
## Eliminamos las columnas innecesarias
parcelas3<-parcelas3[,-c(1,6,7)]


################################################################################
#     Añadimos variables que serán de interés para los modelos de regresión
################################################################################
## Tener en cuenta que g => mm^2    y      altura => m

## ((1))) Otra variable explicativa que interesa es la que se obtiene del producto de G 
## por la altura que es una aproximación al volumen
combi4172_3$ProxVol<-with(combi4172_3,Altura*(g/1000000))

################################################################################
#          Añadimos variables necesarias para las parcelas
################################################################################
## ((2)) Dg
parcelas3$Dg<-with(parcelas3,sqrt((4*G)/(N*pi))*100)
## ((3)) SDI
parcelas3$SDI<-with(parcelas3,N*(Dg/25)^(1.605))


################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################
## Eliminamos las parcelas con menos de 100 arboles, ya que no van a ser
## representativas
parcelas3<-parcelas3[-which(parcelas3$N<100),]

## ((4)) Calculamos el BAL
##a--Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
combi4172_3<-combi4172_3[order(combi4172_3$PlotID0,combi4172_3$dbh,decreasing = T),]
##b--BAL 
BALTotal<-c()
for(i in 1:length(unique(combi4172_3$PlotID0))){
  dat<-combi4172_3[combi4172_3$PlotID0==unique(combi4172_3$PlotID0)[i],]
  aux<-cumsum(c(0,dat$g_ha[-length(dat$g_ha)]))
  BALTotal<-c(BALTotal,aux)
}
combi4172_3$BALTotal<-BALTotal

## ((5)) Calculamos la altura dominante
alturaDominante<-c()
for(i in 1:length(unique(combi4172_3$PlotID0))){
  dat<-combi4172_3[combi4172_3$PlotID0==unique(combi4172_3$PlotID0)[i],]
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
combi4172_3$AlturaDominante<-alturaDominante

alturaDiam<-merge(combi4172_3, parcelas3)

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

## Variables de arboles
arboles<-alturaDiam[c(2,3,8,11,14,16,17,18,19,20,21,22,23,24,25)]
arboles$INV_ID<-"ifn3"
arboles$TREE_ID<-paste0(arboles$INV_ID,".",arboles$PlotID0,".",arboles$NumOrden)

## Variables de parcelas
parcelas<-alturaDiam[c(2,3,26,27,28,29,30,31,32,33)]
parcelas$INV_ID<-"ifn3"
