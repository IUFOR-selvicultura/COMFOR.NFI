rm(list=ls())
ls()
library(ggplot2)
library(plotly)

## Leemos los datos
setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/Combi")
combiIFN2<-read.table("combinaciones2.txt")
combiIFN3<-read.table("combinaciones3.txt")
combiIFN4<-read.table("combinaciones4.txt")

## En realidad nos interesan las que tengan bastante representación, es decir, 
## que la combinacion no haya ocurrido por casualidad en una parcela, si no que
## sea una combinacion de especies recurrente
## El numero de repeticiones para considerarla repetitiva hemos considerado que
## sea 100
combiIFN2100<-combiIFN2[combiIFN2$Freq>100,]
combiIFN3100<-combiIFN3[combiIFN3$Freq>100,]
combiIFN4100<-combiIFN4[combiIFN4$Freq>100,]

################################################################################
##
## Representamos graficamente las combinaciones mas comunes de cada uno de los 
## IFN por separado
##
################################################################################

###################################IFN2#########################################
plot_ly(y=combiIFN2100$Freq,x=(combiIFN2100$Var1),type="bar",
        marker=list(color="lightgreen"))%>%
  layout(title="Combinaciones mas frecuentes IFN2")

###################################IFN3#########################################
plot_ly(y=combiIFN3100$Freq,x=(combiIFN3100$Var1),type="bar",
        marker=list(color="lightblue"))%>%
  layout(title="Combinaciones mas frecuentes IFN3")

###################################IFN4#########################################
plot_ly(y=combiIFN4100$Freq,x=(combiIFN4100$Var1),type="bar",
        marker=list(color="pink"))%>%
  layout(title="Combinaciones mas frecuentes IFN4")


################################################################################
##
## Representamos graficamente las combinaciones mas comunes 
##
################################################################################
combi<-rbind(combiIFN2100,combiIFN3100,combiIFN4100)
combi$IFN<-c(rep(2,dim(combiIFN2100)[1]),rep(3,dim(combiIFN3100)[1]),rep(4,dim(combiIFN4100)[1]))

barCombi<-ggplot(combi,aes(x=Var1,y=Freq,fill=factor(IFN)))+
  geom_bar(stat="identity",position = "dodge")+
  #geom_text(aes(label = Cont), vjust = -0.5) +
  labs(title = "Combinaciones más frecuentes",
       x = "Combinaciones",
       y = "Contador",
       fill="INF") +
  theme_minimal()
barCombi
ggplotly(barCombi)
