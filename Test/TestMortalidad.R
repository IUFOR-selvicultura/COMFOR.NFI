################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Test Modelo Mortalidad
##    Purpose: Modelo para obtener la ecuación de mortalidad
##             
##    
################################################################################
rm(list=ls())
library(dplyr)
library(ggplot2)
library(stringr)
library(rPraat)

################################################################################
#                               IFN3
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/IFN3/España")
combi4172_3<-read.csv("arbolesCombi4172.csv")
combi4172_3$PlotID0<-with(combi4172_3, paste0(as.character(combi4172_3$Origen), ".", as.character(combi4172_3$Estadillo) ) )

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## numOrden
combi4172_3$ArbolID<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo),".",as.character(combi4172_3$NumOrden)))

## Eliminamos columnas que no aportan información
combi4172_3<-combi4172_3[,-c(1,2)]


parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")
plotID0<-c()
## Añadimos el plotID0 a partir del plotID
for(i in 1:dim(parcelasPorcentage)[1]){
  partes <- unlist(strsplit(parcelasPorcentage$PlotID[i], "\\."))
  numero <- paste0(partes[1],".",partes[2])
  plotID0<-c(plotID0,numero)
}
parcelasPorcentage$PlotID0 <-plotID0

#parcelasPorcentage$PI0<-with(parcelasPorcentage,substr(PlotID,1,str_find(PlotID,'.')[2]))

## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID0 %in% combi4172_3$PlotID0)
parcelas3<- as.data.frame( parcelasPorcentage[ posicion, ])
## Eliminamos las columnas innecesarias
parcelas3<-parcelas3[,-c(1,6,7)]

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172<-merge(combi4172_3,parcelas3,by="PlotID")

arboles4172$ArbolID<-with(arboles4172,paste0(Origen,".",Estadillo,".",NumOrden))

################################################################################
#                               IFN4
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/IFN4/España")
combi4172_4<-read.csv("arbolesCombi4172Todos.csv")

## Eliminamos las parcelas que contienen 3E
combi4172_4<-subset(combi4172_4,!grepl("\\.3E$",PlotID))

## Eliminamos las variables que no nos hacen falta
combi4172_4<-combi4172_4[,-c(1,2,13)]

## Añadimos identificador
combi4172_4$PlotID0<-with(combi4172_4,paste0(as.character(combi4172_4$Origen),".",as.character(combi4172_4$Estadillo)))

## Además necesitamos la información de los plots
parcelas<-read.csv("of_plotsPluriSP.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelas$PlotID %in% combi4172_4$PlotID )
parcelas<- as.data.frame( parcelas[ posicion, ])

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172_4<-merge(combi4172_4,parcelas,by="PlotID")



################################################################################
#
#                     MODELO MORTALIDAD                      
#
################################################################################
## Eliminamos las variables innecesarias

## Indicamos si se han muerto los árboles o no
arboles4172_4$Mortalidad<-with(arboles4172_4,ifelse((OrdenIf3!=0 & OrdenIf4==0) | OrdenIf4==888,1,0))

## Extraemos los arboles que han muerto para obtener los datos del IFN2
arboles4172_4_muerto<-arboles4172_4[arboles4172_4$Mortalidad==1,]
arboles4172_4_muerto$IDArbol_IFN3<-with(arboles4172_4_muerto,paste0(PlotID0,".",NumOrden))
## Buscamos los datos de los árboles muertos en el IFN2
arbolesMuertos<-arboles4172[which(arboles4172$ArbolID %in% arboles4172_4_muerto$IDArbol_IFN3),]
arbolesMuertos$Mortalidad<-rep(1,dim(arbolesMuertos)[1])

## Extraemos los arboles que han muerto para obtener los datos del IFN2
arboles4172_4_vivo<-arboles4172_4[arboles4172_4$Mortalidad==0,]
arboles4172_4_vivo$IDArbol_IFN3<-with(arboles4172_4_vivo,paste0(PlotID0,".",NumOrden))

## Buscamos los datos de los árboles muertos en el IFN2
arbolesVivos<-arboles4172[which(arboles4172$ArbolID %in% arboles4172_4_vivo$IDArbol_IFN3),]
arbolesVivos$Mortalidad<-rep(0,dim(arbolesVivos)[1])

#Unimos los dos data frames
arbolesMortalidad<-rbind(arbolesVivos,arbolesMuertos)

mortalidadP<-c()
plots<-c()
for(i in 1:length(unique(arbolesMortalidad$PlotID0.x))){
  dat<-arbolesMortalidad[arbolesMortalidad$PlotID0.x==unique(arbolesMortalidad$PlotID0.x)[i],]
  plots<-c(plots,unique(arbolesMortalidad$PlotID0.x)[i])
  if(sum(dat$Mortalidad)>0){
    mortalidadP<-c(mortalidadP,sum(dat$Mortalidad*dat$expan))
  }else{
    mortalidadP<-c(mortalidadP,0)
  }
}
mortalidadP<-round(mortalidadP,0)

## Creamos un nuevo data frame con parcelas y el id de incorporacion
mortalidadParcelas<-data.frame(PlotID0=plots,Mortalidad=mortalidadP)


## Nos interesas la información de las parcelas del IFN con los porcentajes
posicion <- which( parcelasPorcentage$PlotID %in% combi4172_4$PlotID )
parcelas4<- as.data.frame( parcelasPorcentage[ posicion, ])
## Eliminamos las columnas innecesarias
parcelas4<-parcelas4[,-c(1,2)]


################################################################################
#                   AÑADIMOS LAS VARIABLES NECESARIAS
################################################################################
## ((1)) Dg
parcelas3$Dg<-with(parcelas3,sqrt((4*G)/(N*pi))*100)
## ((2)) SDI
parcelas3$SDI<-with(parcelas3,N*(Dg/25)^(1.605))

## Índice identificador para poder unir los porcentajes por especies a este data frame
mortalidadT0<-merge(mortalidadParcelas,parcelas3,by="PlotID0")


################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################
## ((3)) Calculamos la altura dominante
alturaDominante<-c()
plots<-c()
for(i in 1:length(unique(arbolesMortalidad$PlotID0.x))){
  dat<-arbolesMortalidad[arbolesMortalidad$PlotID0.x==unique(arbolesMortalidad$PlotID0.x)[i],]
  plots<-c(plots,unique(arbolesMortalidad$PlotID0.x)[i])
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

datosADM<-data.frame(PlotID0=plots,AlturaDom=alturaDominante)


## Unimos los data frame
mortalidadT0<-merge(mortalidadT0,datosADM,by="PlotID0")

## ((4)) Indice Hart BecKing
mortalidadT0$HartB<-with(mortalidadT0,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDom)))

################################################################################

## Variables de arboles
arboles<-arbolesMortalidad[c(4,5,13,15,19,22,24,25,27)]
arboles$INV_ID<-"ifn3"
arboles$TREE_ID<-paste0(arboles$INV_ID,".",arboles$PlotID0.x,".",arboles$NumOrden)

## Variables de parcelas
parcelas<-mortalidadT0[c(1,2,4,5,6,7,8,9,11,12)]
parcelas$INV_ID<-"ifn3"


