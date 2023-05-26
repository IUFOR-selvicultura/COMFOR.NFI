################################################################################
#
#                     EVOLUCION DEL IFN
#
################################################################################
library(ggplot2)
library(plotly)

################################################################################
#                         Funciones
################################################################################
numPlotsIF<-function(setwd,id){
  plotsMonoSP<-read.csv(paste0(setwd,'/of_plotsMonoSP.csv'), row.names = 1)
  plotsPluri<-read.csv(paste0(setwd,"/of_plotsPluriSP.csv"),row.names=1)
  resultMD<-read.csv(paste0(setwd,'/of_resultHeightPlurimodal.csv'), row.names = 1)
  resultMDiamF<-read.csv(paste0(setwd,'/of_resultDiamPlurimodal.csv'), row.names = 1)
  
  ## Monoespecificas
  nMono<-dim(plotsMonoSP)[1]
  
  ## Pluriespecifica
  nPluriEsp<-dim(plotsPluri)[1]
  
  ## Modalidad
  resultMModal <- merge( resultMD, resultMDiamF, by='PlotID', all=T)
  positionPluri <- which( ( as.numeric(resultMModal$Pvalor.x) < 0.05 ) & ( as.numeric(resultMModal$Pvalor.y) < 0.05 ) )
  plotsPluriMod <- resultMModal[ positionPluri, ]
  nPlurimod<-dim(plotsPluriMod)[1]
  
  tipo<-c("Monoespecifica","Plurimodal","Pluriespecifica")
  num<-c(nMono,nPlurimod,nPluriEsp)
  id<-rep(id,3)
  datosT<-data.frame(id,num,tipo)
  return(datosT)
}

numPlotsIFCCAA<-function(setwd,id){
  plotsMonoSP<-read.csv(paste0(setwd,'/of_plotsMonoSP.csv'), row.names = 1)
  plotsPluri<-read.csv(paste0(setwd,"/of_plotsPluriSP.csv"),row.names=1)
  resultMD<-read.csv(paste0(setwd,'/of_resultHeightPlurimodal.csv'), row.names = 1)
  resultMDiamF<-read.csv(paste0(setwd,'/of_resultDiamPlurimodal.csv'), row.names = 1)
  
  comunidades<-function(datos){
    origen<-c()
    for(i in 1:dim(datos)[1]){
      prov<-strsplit(datos$PlotID[i],split = "\\.")[[1]][1]
      prov<-trimws(prov)
      origen<-c(origen,prov)
    }
    datos$Origen<-origen
    
    datos$CCAA<-with(datos,ifelse(Origen=="4" | Origen=="11" | Origen=="14"| Origen=="18" | Origen=="21"| Origen=="23"| Origen=="29"| Origen=="41" ,"Andalucia",0))
    datos$CCAA<-with(datos,ifelse(Origen=="22" | Origen=="44" | Origen=="50","Aragon",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="33","Asturias",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="7","Baleares",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="35" | Origen=="38" ,"Canarias",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="39","Cantabria",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="5" | Origen=="9" | Origen=="24"| Origen=="34"| Origen=="37"| Origen=="23"| Origen=="40"| Origen=="42" | Origen=="47" | Origen=="49" ,"CastillaLeon",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="4" | Origen=="2" | Origen=="13"| Origen=="16"| Origen=="19"| Origen=="45","CastillaLaMancha",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="8" | Origen=="17" | Origen=="25"| Origen=="43" ,"Cataluña",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="3" | Origen=="12" | Origen=="46" ,"ComunidadValenciana",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="6" | Origen=="10" ,"Extremadura",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="15" | Origen=="27" | Origen=="32" | Origen=="36" ,"Galicia",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="28","Madrid",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="30","Murcia",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="31","Navarra",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="1" | Origen=="48" | Origen=="20","PaisVasco",CCAA))
    datos$CCAA<-with(datos,ifelse(Origen=="26","LaRioja",CCAA))
    
    return(datos)
  }
  
  tipo<-c("Monoespecifica","Pluriespecifica","Plurimodal")
  
  ## Monoespecificas
  plotsMonoSP<-comunidades(plotsMonoSP)
  nMonoCCAA<-as.data.frame(table(plotsMonoSP$CCAA))
  colnames(nMonoCCAA)<-c("CCAA","Cont")
  nMonoCCAA$Tipo<-tipo[1]
  
  ## Pluriespecifica
  plotsPluri<-comunidades(plotsPluri)
  nPluriCCAA<-as.data.frame(table(plotsPluri$CCAA))
  colnames(nPluriCCAA)<-c("CCAA","Cont")
  nPluriCCAA$Tipo<-tipo[2]

  ## Modalidad
  resultMModal <- merge( resultMD, resultMDiamF, by='PlotID', all=T)
  positionPluri <- which( ( as.numeric(resultMModal$Pvalor.x) < 0.05 ) & ( as.numeric(resultMModal$Pvalor.y) < 0.05 ) )
  plotsPluriMod <- resultMModal[ positionPluri, ]
  plotsPluriMod<-comunidades(plotsPluriMod)
  nPluriModCCAA<-as.data.frame(table(plotsPluriMod$CCAA))
  colnames(nPluriModCCAA)<-c("CCAA","Cont")
  nPluriModCCAA$Tipo<-tipo[3]
  
  datos<-rbind(nMonoCCAA,nPluriCCAA,nPluriModCCAA)
  datos$id<-rep(id,dim(datos)[1])
  datosCCAA<-datos
  return(datosCCAA)
}

####################################IFN2########################################
setwd2<-("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
datosT2<-numPlotsIF(setwd=setwd2,2);datosT2

####################################IFN3########################################
setwd3<-("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
datosT3<-numPlotsIF(setwd=setwd3,3);datosT3

####################################IFN4########################################
setwd4<-("C:/Users/Irene/Documents/INF COMFOR/IFN4/España/Outputs/Outputs")
datosT4<-numPlotsIF(setwd=setwd4,4);datosT4

################################BARRAS APLIADAS#################################
## Juntamos los datos
datosT<-rbind(datosT2,datosT3,datosT4)

todos<-ggplot(datosT,aes(x=tipo,y=num,fill=factor(id)))+
  geom_bar(stat="identity")
todos
ggplotly(todos)

################################################################################
#
#                            POR CCAA
#
################################################################################
####################################IFN2########################################
setwd2<-("C:/Users/Irene/Documents/INF COMFOR/General/General/comfor-NFI/of_spain/of_spain/completo")
datosCCAA2<-numPlotsIFCCAA(setwd=setwd2,2);datosCCAA2

####################################IFN3########################################
setwd3<-("C:/Users/Irene/Documents/INF COMFOR/Entrega/IFN3 Prueba")
datosCCAA3<-numPlotsIFCCAA(setwd=setwd3,3);datosCCAA3

####################################IFN4########################################
setwd4<-("C:/Users/Irene/Documents/INF COMFOR/IFN4/España/Outputs/Outputs")
datosCCAA4<-numPlotsIFCCAA(setwd=setwd4,4);datosCCAA4

###########################BARRAS MULTIPLES#####################################
datosCCAA<-rbind(datosCCAA2,datosCCAA3,datosCCAA4)

## MONOESPECIFICAS
datosCCAAMono<-datosCCAA[datosCCAA$Tipo=="Monoespecifica",]
barMono<-ggplot(datosCCAAMono,aes(x=CCAA,y=Cont,fill=factor(id)))+
  geom_bar(stat="identity",position = "dodge")+
  #geom_text(aes(label = Cont), vjust = -0.5) +
  labs(title = "Parcelas Monoespecificas",
       x = "Comunidad",
       y = "Contador",
       fill="INF") +
  theme_minimal()
barMono
ggplotly(barMono)

## PLURIESPECIFICAS
datosCCAAPluri<-datosCCAA[datosCCAA$Tipo=="Pluriespecifica",]
barPluri<-ggplot(datosCCAAPluri,aes(x=CCAA,y=Cont,fill=factor(id)))+
  geom_bar(stat="identity",position = "dodge")+
  #geom_text(aes(label = Cont), vjust = -0.5) +
  labs(title = "Parcelas Pluriespeficas",
       x = "Comunidad",
       y = "Numero",
       fill="INF") +
  theme_minimal()
barPluri
ggplotly(barPluri)

## PLURIMODALES
datosCCAAPluriMod<-datosCCAA[datosCCAA$Tipo=="Plurimodal",]
barPluriMod<-ggplot(datosCCAAPluriMod,aes(x=CCAA,y=Cont,fill=factor(id)))+
  geom_bar(stat="identity",position = "dodge")+
  #geom_text(aes(label = Cont), vjust = -0.5) +
  labs(title = "Parcelas Plurimodal",
       x = "Comunidad",
       y = "Numero",
       fill="INF") +
  theme_minimal()
barPluriMod
ggplotly(barPluriMod)
