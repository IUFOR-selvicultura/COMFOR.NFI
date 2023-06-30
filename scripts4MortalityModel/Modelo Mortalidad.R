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
combi4172_3<-read.csv("arbolesCombi4172Todos.csv")

## Eliminamos las parcelas que contienen 3E
combi4172_3<-subset(combi4172_3,!grepl("\\.3E$",PlotID))

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## OrdenIfn2
combi4172_3$ArbolID<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo),".",as.character(combi4172_3$OrdenIf2)))

## Eliminamos las variables que no nos hacen falta
combi4172_3<-combi4172_3[,-c(1,2)]

## Añadimos identificador
combi4172_3$PlotID0<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo)))

## Además necesitamos la información de los plots
parcelas<-read.csv("of_plotsPluriSP.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelas$PlotID %in% combi4172_3$PlotID )
parcelas<- as.data.frame( parcelas[ posicion, ])

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172_3<-merge(combi4172_3,parcelas,by="PlotID")



################################################################################
#
#                     MODELO MORTALIDAD                         
#
################################################################################
## Eliminamos las variables innecesarias

## Indicamos si se han muerto los árboles o no
arboles4172_3$Mortalidad<-with(arboles4172_3,ifelse((OrdenIf2!=0 & OrdenIf3==0) | OrdenIf3==888,1,0))

## Extraemos los arboles que han muerto para obtener los datos del IFN2
arboles4172_3_muerto<-arboles4172_3[arboles4172_3$Mortalidad==1,]
arboles4172_3_muerto$IDArbol_IFN2<-with(arboles4172_3_muerto,paste0(PlotID0,".",NumOrden))
## Buscamos los datos de los árboles muertos en el IFN2
arbolesMuertos<-arboles4172[which(arboles4172$ArbolID %in% arboles4172_3_muerto$IDArbol_IFN2),]
arbolesMuertos$Mortalidad<-rep(1,dim(arbolesMuertos)[1])

## Seleccionamos únicamente las columnas de los arboles que nos interesan 
arbolesMuertos<-arbolesMuertos[c("PlotID","Especie","Altura","PlotID0","expan","ArbolID","Mortalidad")]

## Extraemos los arboles que han muerto para obtener los datos del IFN2
arboles4172_3_vivo<-arboles4172_3[arboles4172_3$Mortalidad==0,]
arboles4172_3_vivo$IDArbol_IFN2<-with(arboles4172_3_vivo,paste0(PlotID0,".",NumOrden))

## Buscamos los datos de los árboles muertos en el IFN2
arbolesVivos<-arboles4172[which(arboles4172$ArbolID %in% arboles4172_3_vivo$IDArbol_IFN2),]
arbolesVivos$Mortalidad<-rep(0,dim(arbolesVivos)[1])

## Seleccionamos únicamente las columnas de los arboles que nos interesan 
arbolesVivos<-arbolesVivos[c("PlotID","Especie","Altura","PlotID0","expan","ArbolID","Mortalidad")]

#Unimos los dos data frames
arbolesMortalidad<-rbind(arbolesVivos,arbolesMuertos)

mortalidadP<-c()
plots<-c()
for(i in 1:length(unique(arbolesMortalidad$PlotID0))){
  dat<-arbolesMortalidad[arbolesMortalidad$PlotID0==unique(arbolesMortalidad$PlotID0)[i],]
  plots<-c(plots,unique(arbolesMortalidad$PlotID0)[i])
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
setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID %in% combi4172_2$PlotID )
parcelas2<- as.data.frame( parcelasPorcentage[ posicion, ])
## El identificador sin espacios para que tenga el mismo formato que el del IFN3
parcelas2$PlotID0<-gsub(" ","",parcelas2$PlotID)
## Eliminamos las columnas innecesarias
parcelas2<-parcelas2[,-c(1,2)]


################################################################################
#                   AÑADIMOS LAS VARIABLES NECESARIAS
################################################################################
## ((1)) Dg
parcelas2$Dg<-with(parcelas2,sqrt((4*G)/(N*pi))*100)
## ((2)) SDI
parcelas2$SDI<-with(parcelas2,N*(Dg/25)^(1.605))

## Índice identificador para poder unir los porcentajes por especies a este data frame
mortalidadT0<-merge(mortalidadParcelas,parcelas2,by="PlotID0")


################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################
## ((3)) Calculamos la altura dominante
alturaDominante<-c()
plots<-c()
for(i in 1:length(unique(arbolesMortalidad$PlotID0))){
  dat<-arbolesMortalidad[arbolesMortalidad$PlotID0==unique(arbolesMortalidad$PlotID0)[i],]
  plots<-c(plots,unique(arbolesMortalidad$PlotID0)[i])
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
## Guardamos el fichero de datos
write.csv(mortalidadT0,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Mortalidad/mortalidadT0.csv")
write.csv(arbolesMortalidad,"C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Mortalidad/arbolesMortalidad.csv")


