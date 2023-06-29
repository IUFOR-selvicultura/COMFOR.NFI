################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
## 
################################################################################

rm(list=ls())
if_ed <- 'if4' ## elegimos la edición del inventario con la que trabajar
ls()
## Leemos los datos
## #setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
## #setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
## setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
getwd()
od_calculus <- '../data/'
dir(path=od_calculus)
datos<-read.csv(paste0(od_calculus,"of_",if_ed,"_plotsPluriSP_sps.csv"))
d_nfi <- '../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
dir(path=d_nfi) ## comprobamos que efectivamente esta el fichero que deseamos cargar
load(paste0(d_nfi,if_ed,'.RData')) ## cargamos el fichero RData que contiene la edición con la que queremos trabajar
switch(if_ed,
       'if2'={arboles <- trees.if2},
       'if3'={arboles <- trees.if3},
       'if4'={arboles <- trees.if4} ) ## asignamos el dataframe cargado, tree.if# a arboles (en este caso la segunda edición)
head(arboles)
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
file_name <- paste0(od_calculus, "combinaciones.", if_ed, ".txt")
write.table(combinaciones, file_name)
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

##### Mixtures #################################################################
#     41+72
#     21+71
#     21+43
#     21+25
#     21+26
#     23+26
#     24+26
################################################################################

especie_1<-24
especie_2<-26
combinacion<-paste0(especie_1,'.',especie_2)
idplot<-c()
cont<-0
comb<-paste(c(especie_1,especie_2),collapse=" ")
for(i in 1:length(listaTotal)){
  if(paste(listaTotal[[i]]$especies,collapse=" ")==comb){
    id<-listaTotal[[i]]$parcelas
    idplot<-c(idplot,id)
    cont=cont+1
  }
}
cont
idplot
file_idplot <- paste0('plots.',combinacion,'.',if_ed,'.csv')
write.csv(idplot, file_idplot)
## Buscamos los valores de los parametros para las parcelas que contienen esa combinacion
posicion <- which( arboles$PlotID %in% idplot )
arbolesCombi <- as.data.frame( arboles[ posicion, ])
head(arbolesCombi); tail(arbolesCombi); str(arbolesCombi)
file_arboles <- paste0("arboles.",combinacion,'.',if_ed,'.csv')
write.csv(arbolesCombi,file_arboles)
