################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
## 
################################################################################

rm(list=ls())

options(width=150)

setwd("./")

getwd()
od_calculus <- '../data/'
dir(path=od_calculus)
od_mixtures <- '../data/mixtures/'
dir(path=od_mixtures)

## d_nfi <- '../../../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
d_nfi <- '../../../NFI-data-raw/' ## directorio en el que tenemos descargados los RData del IFN
dir(path=d_nfi) ## comprobamos que efectivamente esta el fichero que deseamos cargar

##### Ediciones del IFN español  #############################################################
ediciones <- c('if2', 'if3', 'if4')

##### Mixtures ###############################################################################
mezclas <- list( ## c(41, 72),
                c(21, 71), c(21, 43), c(21, 25), c(21, 26), c(23, 26), c(24, 26) )

## se recorren las 3 ediciones del IFN español para seleccionar distintas combinaciones de especies
for (j in 1:length(ediciones)) {
    if_ed <- ediciones[j]

    ## Leemos los datos de parcelas 
    datos<-read.csv(paste0(od_calculus,"of_",if_ed,"_plotsPluriSP_sps.csv"))
    
    ## cargamos el fichero RData que contiene la edición con la que queremos trabajar
    load(paste0(d_nfi,if_ed,'.RData'))
    
    ## asignamos el dataframe cargado, tree.if# a arboles
    switch(if_ed,
       'if2'={arboles <- trees.if2},
       'if3'={arboles <- trees.if3},
       'if4'={arboles <- trees.if4} )
    
    ## asignamos el dataframe cargado, plots.if# a parcelas
    switch(if_ed,
       'if2'={parcelas <- plots.if2},
       'if3'={parcelas <- plots.if3},
       'if4'={parcelas <- plots.if4} ) 

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
    combinaciones<-sort(combinaciones)
    
    ## Realmente nos van a interesar las combinaciones que aparecen varias veces
    combinaciones[combinaciones>10]
    file_name <- paste0(od_mixtures, "combinaciones.", if_ed, ".txt")
    write.table(combinaciones, file_name)

    ## Buscamos los indices de las parcelas que coinciden con la combinación de especies deseada, para poder obtener los datos que correponden de la tabla de árboles y de parcelas
    listaTotal<-list("parcelas","especies")
    for(i in 1:length(plots)){
        a<-unique(datos[datos$PlotID==plots[i],]$PlotID)
        b<-(datos[datos$PlotID==plots[i],]$Especie)
        listaTotal[[i]]<-list("parcelas"=a,"especies"=(b))
    }
    head(listaTotal)

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
        if(cont>0){
            idplot
            file_idplot <- paste0(od_mixtures,'plots.',combinacion,'.',if_ed,'.csv')
            write.csv(idplot, file_idplot)

            ## Buscamos los valores de los parametros para las parcelas que contienen esa combinacion
            posicion <- which( arboles$PlotID %in% idplot )
            arbolesCombi <- as.data.frame( arboles[ posicion, ])
            ## head(arbolesCombi); tail(arbolesCombi); str(arbolesCombi)
            file_arboles <- paste0(od_mixtures,"arboles.",combinacion,'.',if_ed,'.csv')
            write.csv(arbolesCombi,file_arboles)
    
            ## Buscamos los valores de los parametros para las parcelas que contienen esa combinacion
            posicion <- which( parcelas$PlotID %in% idplot )
            parcelasCombi <- as.data.frame( parcelas[ posicion, ])
            file_parcela <- paste0(od_mixtures,'parcelas.',combinacion,'.',if_ed,'.csv')
            write.csv(parcelasCombi,file_parcela)
            }
        }
}

ls()

plots.if2.in.if3 <- which( plots.if2$PlotID0 %in% plots.if3$PlotID0 )
plots.if3.in.if2 <- which( plots.if3$PlotID0 %in% plots.if2$PlotID0 )

## buscamos las parcelas comunes en el ifn2 y el ifn3 para cada combinación
head(plots.comb.if2.if3,30)
combinacion
for (i in 1:length(mezclas)){
    especies<-mezclas[[i]]       
    combinacion<-paste0(especies[1],'.',especies[2])
    file_idplot <- paste0(od_mixtures,'parcelas.',combinacion,'.if2.csv'); plots.comb.if2<-read.csv(file_idplot)[,c('PlotID0','CoorX', 'CoorY', 'ANO', 'FECHGR', 'FECHAVE') ]
    file_idplot <- paste0(od_mixtures,'parcelas.',combinacion,'.if3.csv'); plots.comb.if3<-read.csv(file_idplot)[,c('PlotID0','CoorX', 'CoorY', 'FechaIni', 'FechaFin') ]
    plots.comb.if2.if3<-merge(plots.comb.if2,plots.comb.if3,by='PlotID0')
    dim(plots.comb.if2);    dim(plots.comb.if3);    dim(plots.comb.if2.if3)
    file_parcela <- paste0(od_mixtures,'parcelas.',combinacion,'.if2.if3.csv')
    write.csv(parcelasCombi,file_parcela)
    
}
