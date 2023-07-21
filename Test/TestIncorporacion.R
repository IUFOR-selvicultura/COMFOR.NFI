################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Test Modelo Incorporación
##    Purpose: Modelo para obtener la ecuación de incorporación
##             
##    
################################################################################
rm(list=ls())
library(dplyr)
library(ggplot2)

################################################################################
#                               IFN3
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/IFN3/España")
arboles_3<-read.csv("arboles.csv")

## Eliminamos las parcelas que contienen 3E
arboles_3<-subset(arboles_3,!grepl("\\.3E$",PlotID))

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## OrdenIfn2

## Eliminamos las variables que no nos hacen falta
arboles_3<-arboles_3[,-c(1)]

## Añadimos identificador
arboles_3$PlotID0<-with(arboles_3,paste0(as.character(arboles_3$Origen),".",as.character(arboles_3$Estadillo)))

combi4172_3<-read.csv("arbolesCombi4172.csv")
combi4172_3$PlotID0<-with(combi4172_3, paste0(as.character(combi4172_3$Origen), ".", as.character(combi4172_3$Estadillo) ) )

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## numOrden
combi4172_3$ArbolID<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo),".",as.character(combi4172_3$NumOrden)))

## Eliminamos columnas que no aportan información
combi4172_3<-combi4172_3[,-c(1,2)]


## Datos de las parcelas para estudiar como es su comportamiento
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
#                               IFN4
################################################################################
setwd("C:/Users/Irene/Documents/INF COMFOR/IFN4/España")
arboles_4<-read.csv("arboles.csv")

## Eliminamos las variables que no nos hacen falta
arboles_4<-arboles_4[,-c(1)]

## Eliminamos las parcelas que contienen 3E
arboles_4<-subset(arboles_4,!grepl("\\.3E$",PlotID))

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## OrdenIfn2

## Añadimos identificador
arboles_4$PlotID0<-with(arboles_4,paste0(as.character(arboles_4$Origen),".",as.character(arboles_4$Estadillo)))


################################################################################
#
#                                 DATOS
#
################################################################################
## Solo necesitamos los datos del IFN3, ya que con ello sabemos si se ha 
## incorporado o no fijándonos en el Orden
arboles_4$Incorporacion<-with(arboles_4,ifelse((OrdenIf4!=0 & OrdenIf3==0) | OrdenIf4==999,1,0))

pos<-which((arboles_4$PlotID0)%in% (parcelas3$PlotID0))
arboles_4S<-arboles_4[pos,]

## Numero de incorporaciones por parcela
IncorporacionP<-c()
for(i in 1:length(unique(arboles_4S$PlotID0))){
  dat<-arboles_4S[arboles_4S$PlotID0==unique(arboles_4S$PlotID0)[i],]
  if(sum(dat$Incorporacion)>0){
    IncorporacionP<-c(IncorporacionP,sum(dat$Incorporacion*dat$expan))
  }else{
    IncorporacionP<-c(IncorporacionP,0)
  }
}

IncorporacionP<-round(IncorporacionP,0)

## Creamos un nuevo data frame con parcelas y el id de incorporacion
incorporacionParcelas<-data.frame(PlotID0=unique(arboles_4S$PlotID0),Incorporacion=IncorporacionP)


################################################################################
#          Añadimos variables necesarias para las parcelas
################################################################################
## ((1)) Dg
parcelas3$Dg<-with(parcelas3,sqrt((4*G)/(N*pi))*100)
## ((2)) SDI
parcelas3$SDI<-with(parcelas3,N*(Dg/25)^(1.605))

## Índice identificador para poder unir los porcentajes por especies a este data frame
incorporacionT0<-merge(incorporacionParcelas,parcelas3,by="PlotID0")

##Seleccionamos los indices de incorporacionT0 que esten en IFN2, en el combi
posicionI<-which(combi4172_3$PlotID0 %in% incorporacionT0$PlotID0)
arboles3<-combi4172_3[posicionI,]

## ((3)) Calculamos la altura dominante
alturaDominante<-c()
plots<-c()
for(i in 1:length(unique(arboles3$PlotID0))){
  dat<-arboles3[arboles3$PlotID0==unique(arboles3$PlotID0)[i],]
  plots<-c(plots,unique(arboles3$PlotID0)[i])
  if(dat$expan[1]>100){
    altDom<-dat$Altura[1]
    alturaDominante<-c(alturaDominante,altDom)
  }else{
    dat$H<-with(dat,(Altura*expan))
    dat$HCumsum<-cumsum(dat$H)
    dat$ExpanCumsum<-cumsum(dat$expan)
    if(max(dat$ExpanCumsum)<100){
      altDom<-mean(dat$Altura)
      alturaDominante<-c(alturaDominante,altDom)
    }else{
      ecPos<-which(dat$ExpanCumsum>100)[1]
      ecVal<-dat[ecPos,]$ExpanCumsum
      mas100<-ecVal-100
      dat$Ho<-with(dat,ifelse((HCumsum<HCumsum[ecPos]),HCumsum,0))
      dat$Ho[ecPos]<-dat$Altura[ecPos]*(dat$expan[ecPos]-mas100)+dat$Ho[ecPos-1]
      altDom<-max(dat$Ho)/100
      alturaDominante<-c(alturaDominante,altDom)
    }
  }
}
datosAD<-data.frame(PlotID0=plots,AlturaDom=alturaDominante)

## Unimos los data frame
incorporacionT0<-merge(incorporacionT0,datosAD,by="PlotID0")

## ((4)) Indice Hart BecKing
incorporacionT0$HartB<-with(incorporacionT0,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDom)))

write.csv(incorporacionT0,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Incorporacion/TestincorporacionT0.csv")

################################################################################
#
#       División de los datos en datos de arboles y de parcelas
#
################################################################################

## Variables de arboles
arbolesIncor<-arboles3[c(4,5,8,13,15,19)]
arbolesIncor$INV_ID<-"ifn3"
arbolesIncor$TREE_ID<-paste0(arbolesIncor$INV_ID,".",arbolesIncor$PlotID0,".",arbolesIncor$NumOrden)

## Variables de parcelas
parcelasIncor<-incorporacionT0[c(1,4,5,6,7,8,11,12)]
parcelasIncor$INV_ID<-"ifn3"

