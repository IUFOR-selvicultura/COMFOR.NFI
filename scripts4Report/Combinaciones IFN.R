################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
## 
################################################################################
rm(list=ls())
ls()

## Leemos los datos
## #setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
## #setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
## setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
getwd()
od_calculus <- '../data/'
dir(path=od_calculus)
datos<-read.csv(paste0(od_calculus,"of_if4_plotsPluriSP_sps.csv"))

d_nfi <- '../../../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
dir(path=d_nfi) ## comprobamos que efectivamente esta el fichero que deseamos cargar
load(paste0(d_nfi,'if2.RData')) ## cargamos el fichero RData que contiene la edición con la que queremos trabajar
arboles <- tree.if2 ## asignamos el dataframe cargado, tree.if# a arboles (en este caso la segunda edición)

## Las diferentes parcelas que tenemos 
plots<-unique(datos$PlotID)

## Recogemos en una lista cada uno de los grupos de especies correspondientes a cada parcela
grupos<-list()
for(i in 1:length(plots)){
  grupos[[i]]<-datos[datos$PlotID==plots[i],]$Especie
}
head(grupos)

## Hacemos un recuento del numero de veces que aparece cada uno de los grupos  
combinaciones<- table(sapply(grupos, paste, collapse = ","))

## Ordenamos por el numero de repeticiones
combinaciones<-sort(combinaciones);combinaciones

## Realmente nos van a interesar las combinaciones que aparecen varias veces
combinaciones[combinaciones>10]

write.table(combinaciones,"combinaciones.if2.txt")

## Buscamos los indices de las parcelas que coinciden con la especie deseada, 
## es decir, en este caso queremos la combinación del 21 con el 43, para poder 
## obtener los datos que correponsen y así hallar la ecuación de crecimiento 
## que deseamos
listaTotal<-list("parcelas","especies")
for(i in 1:length(plots)){
  a<-unique(datos[datos$PlotID==plots[i],]$PlotID)
  b<-(datos[datos$PlotID==plots[i],]$Especie)
  listaTotal[[i]]<-list("parcelas"=a,"especies"=(b))
}
head(listaTotal)



################################################################################
#                               41+72
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(41,72),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.41.72.csv')

## Buscamos los valores de los parametros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi4172 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi4172); tail(arbolesCombi4172); str(arbolesCombi4172)
write.csv(arbolesCombi4172,"arbolesCombi4172Todos.csv")




################################################################################
#                               21+71
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(21,71),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.21.71.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2171 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2171); tail(arbolesCombi2171); str(arbolesCombi2171)
write.csv(arbolesCombi2171,"arbolesCombi2171.csv")

################################################################################
#                               21+43
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(21,43),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.21.43.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2143 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2143); tail(arbolesCombi2143); str(arbolesCombi2143)
write.csv(arbolesCombi2143,"arbolesCombi2143.csv")

################################################################################
#                               21+25
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(21,25),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.21.25.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2125 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2125); tail(arbolesCombi2125); str(arbolesCombi2125)
write.csv(arbolesCombi2125,"arbolesCombi2125.csv")


################################################################################
#                               21+26
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(21,26),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.21.26.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2126 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2126); tail(arbolesCombi2126); str(arbolesCombi2126)
write.csv(arbolesCombi2126,"arbolesCombi2126.csv")

################################################################################
#                               26+23
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(23,26),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.23.26.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2623 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2623); tail(arbolesCombi2623); str(arbolesCombi2623)
write.csv(arbolesCombi2623,"arbolesCombi2623.csv")

################################################################################
#                               26+24
################################################################################
idplot<-c()
cont<-0
comb<-paste(c(24,26),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
write.csv(idplot, 'plots.24.26.csv')

## Buscamos los valores de los paramtros para las parcelas que contienen esa 
## combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi2624 <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi2624); tail(arbolesCombi2624); str(arbolesCombi2624)
write.csv(arbolesCombi2624,"arbolesCombi2624.csv")
