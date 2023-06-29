################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
## 
################################################################################

rm(list=ls())

## #setwd("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
## #setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
## setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")

getwd()
od_calculus <- '../data/'
dir(path=od_calculus)

d_nfi <- '../../../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
d_nfi <- '../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
dir(path=d_nfi) ## comprobamos que efectivamente esta el fichero que deseamos cargar

if_ed <- 'if4' ## elegimos la edición del inventario con la que trabajar

ediciones <- c('if2', 'if3', 'if4')

for (j in 1:length(ediciones)) {
    if_ed <- ediciones[j]

    ## Leemos los datos

    datos<-read.csv(paste0(od_calculus,"of_",if_ed,"_plotsPluriSP_sps.csv"))


    load(paste0(d_nfi,if_ed,'.RData')) ## cargamos el fichero RData que contiene la edición con la que queremos trabajar

    switch(if_ed,
       'if2'={arboles <- trees.if2},
       'if3'={arboles <- trees.if3},
       'if4'={arboles <- trees.if4} ) ## asignamos el dataframe cargado, tree.if# a arboles (en este caso la segunda edición)
    head(arboles)

    switch(if_ed,
       'if2'={parcelas <- plots.if2},
       'if3'={parcelas <- plots.if3},
       'if4'={parcelas <- plots.if4} ) ## asignamos el dataframe cargado, tree.if# a arboles (en este caso la segunda edición)

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

    ## Buscamos los indices de las parcelas que coinciden con la especie deseada, es decir, en este caso queremos la combinación del 21 con el 43, para poder obtener los datos que correponsen y así hallar la ecuación de crecimiento que deseamos
    
    listaTotal<-list("parcelas","especies")
    for(i in 1:length(plots)){
        a<-unique(datos[datos$PlotID==plots[i],]$PlotID)
        b<-(datos[datos$PlotID==plots[i],]$Especie)
        listaTotal[[i]]<-list("parcelas"=a,"especies"=(b))
    }
    head(listaTotal)

##### Mixtures #################################################################
mezclas <- list( c(41,72), c(21,71), c(21,43),c(21,25),c(21,26),c(23,26),c(24, 26) )
################################################################################
for (i in 1:length(mezclas)){
    especies<-mezclas[[i]]
    combinacion<-paste0(especies[1],'.',especies[2])
    idplot<-c()
    cont<-0
    comb<-paste(c(especies[1],especies[2]),collapse=" ")
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

    ## Buscamos los valores de los parametros para las parcelas que contienen esa combinacion
    posicion <- which( parcelas$PlotID %in% idplot )
    parcelasCombi <- as.data.frame( parcelas[ posicion, ])
    file_parcela <- paste0('parcelas.',combinacion,'.',if_ed,'.csv')
    write.csv(parcelasCombi,file_parcela)
    }
}
