################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK  - Cristobal ORDOÑEZ 
##    
##    File name: Mod incorporacion 72
##    Purpose: Modelo para obtener la ecuacion de incorporacion de la especie 72
##    
##    
################################################################################
rm(list=ls())

library(lme4)
library(lmerTest)
library(MuMIn)
library(MASS)
library(ggplot2)
library(DescTools)
library(MASS)
library(car)

setwd("C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Incorporacion")

## Leemos los datos
incorporacionTO<-read.csv("incorporacionT0.csv")
arboles2<-read.csv("arboles_3S.csv")

###############################################################################
#                     SEPARAMOS POR ESPECIES
# 
#                             ESPECIE 72
################################################################################
## Selecionamos los datos
incorporacion72<-incorporacionTO[incorporacionTO$Especie==72,]
arboles2_72<-arboles2[arboles2$Especie==72,]
arboles2_72$PlotID0<-with(arboles2_72,paste0(Origen,".",Estadillo))

## Calculamos la altura dominante por especie
## ((3)) Calculamos la altura dominante
alturaDominanteE<-c()
plots<-c()
for(i in 1:length(unique(arboles2_72$PlotID0))){
  dat<-arboles2_72[arboles2_72$PlotID0==unique(arboles2_72$PlotID0)[i],]
  plots<-c(plots,unique(arboles2_72$PlotID0)[i])
  if(dat$expan[1]>100){
    altDom<-dat$Altura[1]
    alturaDominanteE<-c(alturaDominanteE,altDom)
  }else{
    dat$H<-with(dat,(Altura*expan))
    dat$HCumsum<-cumsum(dat$H)
    dat$ExpanCumsum<-cumsum(dat$expan)
    if(max(dat$ExpanCumsum)<100){
      altDom<-mean(dat$Altura)
      alturaDominanteE<-c(alturaDominanteE,altDom)
    }else{
      ecPos<-which(dat$ExpanCumsum>100)[1]
      ecVal<-dat[ecPos,]$ExpanCumsum
      mas100<-ecVal-100
      dat$Ho<-with(dat,ifelse((HCumsum<HCumsum[ecPos]),HCumsum,0))
      dat$Ho[ecPos]<-dat$Altura[ecPos]*(dat$expan[ecPos]-mas100)+dat$Ho[ecPos-1]
      altDom<-max(dat$Ho)/100
      alturaDominanteE<-c(alturaDominanteE,altDom)
    }
  }
}
datosDA72<-data.frame(PlotID0=plots,AlturaDom72=alturaDominanteE)

## Unimos los 2 data frames
incorP_72<-merge(incorporacion72,datosDA72,by="PlotID0",all.y=TRUE)
incorP_72$HartB72<-with(incorP_72,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDom72)))

incorP_72<-na.omit(incorP_72)
################################################################################
#
#               MODELOS DE REGRESIÓN
#
################################################################################
## Ya tenemos las variables necesarias para el modelo ya que en este caso solo
## necesitamos las variables de parcelas
## Al tener pocas variables no será necesario realizar un ACP para la selección
## de variables
modelo1<-glm(Incorporacion~G+N+porcentajeG+porcentejeN+Dg+SDI+AlturaDom+HartB+AlturaDom72+HartB72,data=incorP_72,family = poisson())
summary(modelo1)
modelo2<-glm(Incorporacion~G+porcentajeG+porcentejeN+Dg+SDI+AlturaDom+HartB+AlturaDom72+HartB72,data=incorP_72,family = poisson())
summary(modelo2)
PseudoR2(modelo2, which = "McFadden")

## Factor de inflacion de la varianza
vif(modelo2)
## Tolerancia
tolerancia<-1/vif(modelo2);tolerancia

modelo3<-glm(Incorporacion~G+porcentajeG+porcentejeN+Dg+AlturaDom+HartB+AlturaDom72+HartB72,data=incorP_72,family = poisson())
summary(modelo3)
PseudoR2(modelo3, which = "McFadden")
vif(modelo3)
1/vif(modelo3)
