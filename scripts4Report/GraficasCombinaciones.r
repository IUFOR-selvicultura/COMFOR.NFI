################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
##    
##    File name: NFICalcFinal
##    Purpose: Read result of script "NFICalcFinal.r" which select plurispecific and 
##             multistratificated monoespecific plots from  Spanish NFI plots and 
##             make maps with them
##    
################################################################################
rm(list=ls())
ls()
library(ggplot2)
library(plotly)
getwd()
dir()
dir <- '../data/'
## Leemos los datos
## setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/Combi")
combiIFN2<-read.table(paste0(dir,"combinaciones.if2.txt"))
combiIFN3<-read.table(paste0(dir,"combinaciones.if3.txt"))
combiIFN4<-read.table(paste0(dir,"combinaciones.if4.txt"))

## En realidad nos interesan las que tengan bastante representación, es decir, 
## que la combinacion no haya ocurrido por casualidad en una parcela, si no que
## sea una combinacion de especies recurrente
## El numero de repeticiones para considerarla repetitiva hemos considerado que
## sea 30
combiIFN2_30<-combiIFN2[combiIFN2$Freq>30,]
combiIFN3_30<-combiIFN3[combiIFN3$Freq>30,]
combiIFN4_30<-combiIFN4[combiIFN4$Freq>30,]

################################################################################
##
## Representamos graficamente las combinaciones mas comunes de cada uno de los 
## IFN por separado
##
################################################################################

graficoIndividual<-function(datos,IFN){
    title=paste0("Combinaciones mas frecuentes en el ",IFN)
    ggplot(data=datos, aes( y=Freq, x=Var1 )) +
        geom_bar(stat = "identity", color = "lightgreen") +
        theme_grey()
  }

###################################IFN2#########################################
graficoIndividual(combiIFN2_30,"IFN2")

###################################IFN3#########################################
graficoIndividual(combiIFN3_30,"IFN3")

###################################IFN4#########################################
graficoIndividual(combiIFN4_30,"IFN4")


################################################################################
##
## Representamos graficamente las combinaciones mas comunes 
##
################################################################################
combiIFN2_90<-combiIFN2[combiIFN2$Freq>90,]
combiIFN3_90<-combiIFN3[combiIFN3$Freq>90,]
combiIFN4_40<-combiIFN4[combiIFN4$Freq>40,]

combi<-rbind(combiIFN2_90,combiIFN3_90,combiIFN4_40)
combi$IFN<-c(rep('IFN2',dim(combiIFN2_90)[1]),rep('IFN3',dim(combiIFN3_90)[1]),rep('IFN4',dim(combiIFN4_40)[1]))

barCombi<-ggplot(combi,aes(x=Var1,y=Freq,fill=factor(IFN)))+
  geom_bar(stat="identity",position = "dodge")+
  #geom_text(aes(label = Cont), vjust = -0.5) +
  labs(title = "Combinaciones más frecuentes",
       x = "Combinaciones",
       y = "Repeticiones",
       fill="IFN") +
  theme_minimal()
barCombi

ggplotly(barCombi)
