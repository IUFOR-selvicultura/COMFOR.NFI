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

## Además necesitamos la información de los plots
parcelas<-read.csv("of_plotsPluriSP.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelas$PlotID %in% combi4172_2$PlotID )
parcelas<- as.data.frame( parcelas[ posicion, ])

## Juntamos los dos dataframe: El de parcelas + El de arbolescombi
arboles4172<-merge(combi4172_2,parcelas,by="PlotID")

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
#                             IFN2 + IFN3
################################################################################

## Vamos a hacer un merge de los dos INF a traves del indice del árbol
mezcla2_3<-merge(arboles4172,arboles4172_3,by="ArbolID",all=TRUE)

## Añadimos variables binarias que nos van a interesar para la regresión
## Crecimiento en diametro
mezcla2_3$CrecDiam<-with(mezcla2_3,ifelse(dbh.x<dbh.y,1,0))
## Creciemiento en altura
mezcla2_3$CrecAlt<-with(mezcla2_3,ifelse(Altura.x<Altura.y,1,0))
## Incorporación
## Si el indice en OrdenIF2 es 0 y en OrdenIF3 es 1, esto nos indica que un árbol
## que antes no existía ahora si existe
## 1 => Indica que se ha incorporado una especie
## 0 => Se ha mantenido la especie que había respecto el IFN2
mezcla2_3$Incorporacion<-with(mezcla2_3,ifelse((OrdenIf3!=0 & OrdenIf2==0) | OrdenIf3==999,1,0))
## Mortalidad
## 1 => Indica que se ha muerto una especie
## 0 => Indica que sigue el mismo arbol
mezcla2_3$Mortalidad<-with(mezcla2_3,ifelse(is.na(Incorporacion) | OrdenIf3==888,1,0))

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
crecimiento<-na.omit(mezcla2_3)

## Necesitamos tener en cuenta el año de toma de los datos de esa especie para 
## hallar el incremento anual para que tenga una mayor precisión

################################################################################
## IFN2
plots2<-read.csv("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/if_plot2.csv")
## Leemos los datos para las comunidades que no aparecen en estos datos
plots31<-read.csv("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo/plot2-31.csv")
plots33<-read.csv("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo/plot2-33.csv")
plots39<-read.csv("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo/plot2-39.csv")
plots2<-rbind(plots2,plots31,plots33,plots39)
## Añadimos el indice 
plots2$PlotID0<-with(plots2,paste0(as.character(plots2$PROVINCIA),".",as.character(plots2$ESTADILLO)))
plots2$PlotID<-with(plots2,paste(as.character(plots2$PROVINCIA),".",as.character(plots2$ESTADILLO)))
## Nos quedamos únicamente con los que coinciden el indice
posicion<-which(plots2$PlotID %in% crecimiento$PlotID.x)
plots<-as.data.frame(plots2[posicion,])
plots<-plots[order(plots$PlotID),]
plots$ANO<-as.numeric(paste0(19,plots$ANO))
plots<-plots[,c("ANO","PlotID","PlotID0")]

#Lo añadimos al data frame crecimiento
crecimiento<-merge(crecimiento,plots,all=TRUE,by.x="PlotID.x",by.y="PlotID")

################################################################################
## IFN3
plots3<-read.csv("C:/Users/Irene/Documents/INF COMFOR/IFN3/España/pcparcelas.csv")
## Añadimos el indice 
plots3$PlotID0<-with(plots3,paste0(as.character(plots3$Origen),".",as.character(plots3$Estadillo)))
plots3$PlotID<-with(plots3,paste(as.character(plots3$Origen),".",as.character(plots3$Estadillo)))
## Nos quedamos únicamente con los que coinciden el indice
posicion<-which(plots3$PlotID %in% crecimiento$PlotID.x)
plots3<-as.data.frame(plots3[posicion,])
plots3<-plots3[order(plots3$PlotID),]
plots3<-plots3[,c("Ano","PlotID","PlotID0")]
plots3<-distinct(plots3) #Hay filas repetidas con los mismos datos

#Lo añadimos al data frame crecimiento
crecimiento<-merge(crecimiento,plots3,by.x="PlotID.x",by.y="PlotID",all=TRUE)
crecimiento$Ano<-as.numeric(crecimiento$Ano)

head(crecimiento)

crecimiento$DiffAno<-with(crecimiento,(crecimiento$Ano)-crecimiento$ANO)

################################################################################
#     Añadimos variables que serán de interés para los modelos de regresión
################################################################################
## Tener en cuenta que g => mm^2    y      altura => m

## ((1))) Otra variable explicativa que interesa es la que se obtiene del producto de G 
## por la altura que es una aproximación al volumen
crecimiento$ProxVol<-with(crecimiento,Altura.x*(g.x/1000000))


## ((2)) Nos intera los porcentajes para cada una de las especies, en el IFN2
setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
parcelasPorcentage<-read.csv("parcelasPorcentajes.csv")
## Nos quedamos solo con las que corresponden a las parcelas mixtas 41+72
posicion <- which( parcelasPorcentage$PlotID %in% crecimiento$PlotID.x)
parcelasPorcentage<- as.data.frame( parcelasPorcentage[ posicion, ])
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
write.csv(crecimientoTO,"C:/Users/Irene/Documents/INF COMFOR/Regresion/crecimientoTO.csv")


