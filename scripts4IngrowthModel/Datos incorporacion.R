################################################################################
#
#                       OBTENCIÓN DE DATOS
#
################################################################################
rm(list=ls())
library(dplyr)
library(ggplot2)

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

## Además necesitamos la información de los plots
parcelas<-read.csv("of_plotsPluriSP.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelas$PlotID %in% combi4172_2$PlotID )
parcelas2<- as.data.frame( parcelas[ posicion, ])

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172<-merge(combi4172_2,parcelas2,by="PlotID")

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
arboles_3$PlotID2<-with(arboles_3,paste0(as.character(arboles_3$Origen),".",as.character(arboles_3$Estadillo)))

################################################################################



################################################################################
#
#             MODELOS DE INCORPORACION                         
#
################################################################################
## Nos interesas la información de las parcelas del IFN con los porcentajes
setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID %in% combi4172_2$PlotID )
parcelas2<- as.data.frame( parcelasPorcentage[ posicion, ])
## El identificador sin espacios para que tenga el mismo formato que el del IFN3
parcelas2$PlotID0<-gsub(" ","",parcelas2$PlotID)
## Eliminamos las columnas innecesarias
parcelas2<-parcelas2[,-c(1,6,7)]

pos<-which((arboles_3$PlotID2)%in% (parcelas2$PlotID0))
arboles_3S<-arboles_3[pos,]

## Solo necesitamos los datos del IFN3, ya que con ello sabemos si se ha 
## incorporado o no fijándonos en el Orden
arboles_3S$Incorporacion<-with(arboles_3S,ifelse((OrdenIf3!=0 & OrdenIf2==0) | OrdenIf3==999,1,0))

## Tomamos los datos de las parcelas del IFN2



#parcelas2 añadir variable incorporacion
## Numero de incorporaciones por parcela
IncorporacionP<-c()
for(i in 1:length(unique(arboles_3S$PlotID2))){
  dat<-arboles_3S[arboles_3S$PlotID2==unique(arboles_3S$PlotID2)[i],]
  if(sum(dat$Incorporacion)>0){
    IncorporacionP<-c(IncorporacionP,sum(dat$Incorporacion*dat$expan))
  }else{
    IncorporacionP<-c(IncorporacionP,0)
  }
}

IncorporacionP<-round(IncorporacionP,0)

## Creamos un nuevo data frame con parcelas y el id de incorporacion
incorporacionParcelas<-data.frame(PlotID0=unique(arboles_3S$PlotID2),Incorporacion=IncorporacionP)

################################################################################
#                   AÑADIMOS LAS VARIABLES NECESARIAS
################################################################################

################################################################################
#          Añadimos variables necesarias para las parcelas
################################################################################
## ((1)) Dg
parcelas2$Dg<-with(parcelas2,sqrt((4*G)/(N*pi))*100)
## ((2)) SDI
parcelas2$SDI<-with(parcelas2,N*(Dg/25)^(1.605))

## Índice identificador para poder unir los porcentajes por especies a este data frame
incorporacionT0<-merge(incorporacionParcelas,parcelas2,by="PlotID0")

##Seleccionamos los indices de incorporacionT0 que esten en IFN2, en el combi
posicionI<-which(combi4172_2$PlotID0 %in% incorporacionT0$PlotID0)
arboles2<-combi4172_2[posicionI,]

################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################

## ((3)) Calculamos la altura dominante
alturaDominante<-c()
plots<-c()
for(i in 1:length(unique(arboles2$PlotID0))){
  dat<-arboles2[arboles2$PlotID0==unique(arboles2$PlotID0)[i],]
  plots<-c(plots,unique(arboles2$PlotID0)[i])
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

################################################################################
## Guardamos el fichero de datos
write.csv(incorporacionT0,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Incorporacion/incorporacionT0.csv")
write.csv(arboles2,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Incorporacion/arboles2.csv")

