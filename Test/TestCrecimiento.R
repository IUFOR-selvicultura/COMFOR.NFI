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
library(lubridate)

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
combi4172_3$PlotID2<-with(combi4172_3,paste0(as.character(combi4172_3$Origen),".",as.character(combi4172_3$Estadillo)))

## Además necesitamos la información de los plots
parcelas<-read.csv("of_plotsPluriSP.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelas$PlotID %in% combi4172_3$PlotID )
parcelas<- as.data.frame( parcelas[ posicion, ])

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172_3<-merge(combi4172_3,parcelas,by="PlotID")

################################################################################
#                                 IFN4
################################################################################
## Nos interesa tener los datos de los arboles que corresponden a las 
## parcelas de la combinacion de las especies 41+72
## Además nos interesa el archivo pcMayores para añadir variables que todavía no 
## habíamos tenido en cuenta y que para las regresiones ahora nos serán útiles

setwd("C:/Users/Irene/Documents/INF COMFOR/IFN4/España")
combi4172_4<-read.csv("arbolesCombi4172Todos.csv")

## Añadimos un indice de arbol, que estará formado por el origen, estadillo y 
## OrdenIfn3
combi4172_4$ArbolID<-with(combi4172_4,paste0(as.character(combi4172_4$Origen),".",as.character(combi4172_4$Estadillo),".",as.character(combi4172_4$OrdenIf3)))

combi4172_4<-combi4172_4[,-c(1,2)]
combi4172_4$PlotID2<-with(combi4172_4,paste0(as.character(combi4172_4$Origen),".",as.character(combi4172_4$Estadillo)))

################################################################################
#                             IFN3 + IFN4
################################################################################
## Vamos a hacer un merge de los dos INF a traves del indice del árbol
mezcla3_4<-merge(arboles4172_3,combi4172_4,by="ArbolID",all=TRUE)

## Añadimos variables binarias que nos van a interesar para la regresión
## Crecimiento en diametro
mezcla3_4$CrecDiam<-with(mezcla3_4,ifelse(dbh.x<dbh.y,1,0))
## Creciemiento en altura
mezcla3_4$CrecAlt<-with(mezcla3_4,ifelse(Altura.x<Altura.y,1,0))
## Incorporación
## Si el indice en OrdenIF2 es 0 y en OrdenIF3 es 1, esto nos indica que un árbol
## que antes no existía ahora si existe
## 1 => Indica que se ha incorporado una especie
## 0 => Se ha mantenido la especie que había respecto el IFN2
mezcla3_4$Incorporacion<-with(mezcla3_4,ifelse(OrdenIf4!=0 & OrdenIf3.y==0,1,0))
## Mortalidad
## 1 => Indica que se ha muerto una especie
## 0 => Indica que sigue el mismo arbol
mezcla3_4$Mortalidad<-with(mezcla3_4,ifelse(is.na(Incorporacion),1,0))

################################################################################


################################################################################
#
#             MODELOS DE REGRESIÓN EN IFN2+IFN3                         
#
################################################################################

## Dataframe sin NA's
crecimiento<-na.omit(mezcla3_4)

## Necesitamos tener en cuenta el año de toma de los datos de esa especie para 
## hallar el incremento anual para que tenga una mayor precisión

################################################################################
## IFN3
plots3<-read.csv("C:/Users/Irene/Documents/INF COMFOR/IFN3/España/pcparcelas.csv")
## Añadimos el indice 
plots3$PlotID0<-with(plots3,paste0(as.character(plots3$Origen),".",as.character(plots3$Estadillo)))
## Nos quedamos únicamente con los que coinciden el indice
posicion<-which(plots3$PlotID %in% crecimiento$PlotID2.x)
plots3<-as.data.frame(plots3[posicion,])
plots3<-plots3[order(plots3$PlotID),]
plots3<-plots3[,c("Ano","PlotID0")]
plots3<-distinct(plots3) #Hay filas repetidas con los mismos datos

#Lo añadimos al data frame crecimiento
crecimiento<-merge(crecimiento,plots3,by.x="PlotID2.x",by.y="PlotID0",all=TRUE)
crecimiento$Ano<-as.numeric(crecimiento$Ano)

################################################################################
## IFN4
setwd("C:/Users/Irene/Documents/INF COMFOR/IFN4/España")
load("if4.Rdata")
plots4<-plots.if4
## Añadimos el indice 
plots4$PlotID0<-with(plots4,paste0(as.character(plots4$Provincia),".",as.character(plots4$Estadillo)))
## Añadimos el año
plots4$Ano4<-year(plots4$FechaIni)

## Nos quedamos únicamente con los que coinciden el indice
posicion<-which(plots4$PlotID0 %in% crecimiento$PlotID2.x)
plots4<-as.data.frame(plots4[posicion,])
plots4<-plots4[order(plots4$PlotID),]
plots4<-plots4[,c("Ano4","PlotID0")]
plots4<-distinct(plots4) #Hay filas repetidas con los mismos datos

#Lo añadimos al data frame crecimiento
crecimiento<-merge(crecimiento,plots4,by.x="PlotID2.x",by.y="PlotID0",all=TRUE)
crecimiento$Ano4<-as.numeric(crecimiento$Ano4)



crecimiento$DiffAno<-with(crecimiento,(crecimiento$Ano4)-crecimiento$Ano)

################################################################################
#     Añadimos variables que serán de interés para los modelos de regresión
################################################################################
## Tener en cuenta que g => mm^2    y      altura => m

## ((1))) Otra variable explicativa que interesa es la que se obtiene del producto de G 
## por la altura que es una aproximación al volumen
crecimiento$ProxVol<-with(crecimiento,Altura.x*(g.x/1000000))


## ((2)) Nos intera los porcentajes para cada una de las especies, en el IFN3
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
posicion <- which( parcelasPorcentage$PlotID0 %in% crecimiento$PlotID2.x)
parcelas3<- as.data.frame( parcelasPorcentage[ posicion, ])
## Eliminamos las columnas innecesarias
parcelasPorcentage<-parcelas3[,-c(1,6,7)]

parcelasPorcentage$EspecieID<-with(parcelasPorcentage,paste0(as.character(PlotID),".",as.character(Especie)))

################################################################################
#          Añadimos variables necesarias para las parcelas
################################################################################
## ((3)) Dg
parcelasPorcentage$Dg<-with(parcelasPorcentage,sqrt((4*G)/(N*pi))*100)
## ((4)) SDI
parcelasPorcentage$SDI<-with(parcelasPorcentage,N*(Dg/25)^(1.605))

## Índice identificador para poder unir los porcentajes por especies a este data frame
crecimiento$EspecieID<-with(crecimiento,paste0(as.character(PlotID.x),".",as.character(Especie.x)))

crecimientoT<-left_join(crecimiento,parcelasPorcentage,by="EspecieID")

################################################################################
#         Añadimos variables necesarias  para el conjunto total de datos
################################################################################
## Eliminamos las parcelas con menos de 100 arboles, ya que no van a ser
## representativas
crecimientoT<-crecimientoT[-which(crecimientoT$N.x<100),]

## ((5)) Calculamos el BAL
##a--Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
crecimientoTO<-crecimientoT[order(crecimientoT$PlotID.x,crecimientoT$dbh.x,decreasing = T),]
##b--BAL 
BALTotal<-c()
for(i in 1:length(unique(crecimientoTO$PlotID.x))){
  dat<-crecimientoTO[crecimientoTO$PlotID.x==unique(crecimientoTO$PlotID.x)[i],]
  aux<-cumsum(c(0,dat$g_ha.x[-length(dat$g_ha.x)]))
  BALTotal<-c(BALTotal,aux)
}
crecimientoTO$BALTotal<-BALTotal

## ((6)) Calculamos la altura dominante
alturaDominante<-c()
for(i in 1:length(unique(crecimientoTO$PlotID.x))){
  dat<-crecimientoTO[crecimientoTO$PlotID.x==unique(crecimientoTO$PlotID.x)[i],]
  dat$H<-with(dat,(Altura.x*expan.x))
  dat$HCumsum<-cumsum(dat$H)
  dat$ExpanCumsum<-cumsum(dat$expan.x)
  ecPos<-which(dat$ExpanCumsum>100)[1]
  ecVal<-dat[ecPos,]$ExpanCumsum
  mas100<-ecVal-100
  dat$Ho<-with(dat,ifelse((HCumsum<HCumsum[ecPos]),HCumsum,0))
  dat$Ho[ecPos]<-dat$Altura.x[ecPos]*(dat$expan.x[ecPos]-mas100)+dat$Ho[ecPos-1]
  dat$AlturaDominante<-max(dat$Ho)/100
  #sum(dat$H)/sum(dat$expan.x)
  altDom<-dat$AlturaDominante
  alturaDominante<-c(alturaDominante,altDom)
}
crecimientoTO$AlturaDominante<-alturaDominante

## ((3)) Indice Hart BecKing
crecimientoTO$HartB<-with(crecimientoTO,(10000/(sqrt((sqrt(3)*N.x)/2)*AlturaDominante)))

## ((4)) Indice Hart 
crecimientoTO$Hart<-with(crecimientoTO,(10000/(sqrt(N.x)*AlturaDominante)))

################################################################################
#                    Reescalamos las variables
################################################################################
## Pasamos de mm^2 a cm^2
crecimientoTO$g.x<-crecimientoTO$g.x/100
crecimientoTO$g.y<-crecimientoTO$g.y/100


################################################################################
#                          Variable respuesta
################################################################################
## Variación de crecimiento
delta<-(crecimientoTO$g.y)-(crecimientoTO$g.x)

## Por lo general se tiene que entre inventario e inventario pasan 10 años
## pero en realidad no es así, y por ello debemos de tener en cuenta el 
## numero de años
crecimientoTO$vRes<-(crecimientoTO$g.x)+delta*(10/crecimientoTO$DiffAno)

## Guardamos el fichero de datos
write.csv(crecimientoTO,"C:/Users/Irene/Documents/INF COMFOR/Regresion/crecimientoTO3_4.csv")

## Variables de arboles
arboles<-crecimientoTO[c(1,2,6,7,15,17,18,22,23,53,69)]
arboles$INV_ID<-"ifn3"
arboles$TREE_ID<-paste0(arboles$INV_ID,".",arboles$PlotID2.x,".",arboles$NumOrden)

## Variables de parcelas
parcelas<-crecimientoTO[c(1,7,22,23,53,60,61,66,67)]
parcelas$INV_ID<-"ifn3"


