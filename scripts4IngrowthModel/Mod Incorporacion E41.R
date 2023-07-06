################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK  - Cristobal ORDOÑEZ 
##    
##    File name: Mod incorporacion 41
##    Purpose: Modelo para obtener la ecuacion de incorporacion de la especie 41
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
arboles2<-read.csv("arboles2.csv")

###############################################################################
#                     SEPARAMOS POR ESPECIES
# 
#                             ESPECIE 41
################################################################################
## Selecionamos los datos
incorporacion41<-incorporacionTO[incorporacionTO$Especie==41,]
arboles_41<-arboles2[arboles2$Especie==41,]
arboles_41$PlotID0<-gsub(" ","",arboles_41$PlotID)

## Calculamos la altura dominante por especie
## ((3)) Calculamos la altura dominante
alturaDominanteE<-c()
plots<-c()
for(i in 1:length(unique(arboles_41$PlotID0))){
  dat<-arboles_41[arboles_41$PlotID0==unique(arboles_41$PlotID0)[i],]
  plots<-c(plots,unique(arboles_41$PlotID0)[i])
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
datosDA41<-data.frame(PlotID0=plots,AlturaDom41=alturaDominanteE)

## Unimos los 2 data frames
incorP_41<-merge(incorporacion41,datosDA41,by="PlotID0",all.y=TRUE)
incorP_41$HartB41<-with(incorP_41,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDom41)))

incorP_41<-na.omit(incorP_41)

################################################################################
#
#               MODELOS DE REGRESIÓN
#
################################################################################
## Ya tenemos las variables necesarias para el modelo ya que en este caso solo
## necesitamos las variables de parcelas
## Al tener pocas variables no será necesario realizar un ACP para la selección
## de variables
modelo1<-glm(Incorporacion~G+N+porcentajeG+porcentejeN+Dg+SDI+AlturaDom+HartB+AlturaDom41+HartB41,data=incorP_41,family = poisson())
summary(modelo1)
PseudoR2(modelo1, which = "McFadden")

## Factor de inflacion de la varianza: Si es mayor de 10 indica que hay colinealidad
vif(modelo1)
## Tolerancia : Si es menor de 0.1 indica que hay colinealidad
tolerancia<-1/vif(modelo1);tolerancia

modelo2<-glm(Incorporacion~G+N+porcentajeG+porcentejeN+Dg+AlturaDom+HartB+AlturaDom41+HartB41,data=incorP_41,family = poisson())
summary(modelo2)

modelo3<-glm(Incorporacion~G+N+porcentajeG+porcentejeN+Dg+AlturaDom+AlturaDom41+HartB41,data=incorP_41,family = poisson())
summary(modelo3)
PseudoR2(modelo3, which = "McFadden")

vif(modelo3)
1/vif(modelo3)


################################################################################
#
#                      AGRUPAMOS DIAMETROS
#
################################################################################
asignar_valor <- function(diametro) {
  ifelse(diametro >= 75 & diametro < 125, 1,
         ifelse(diametro >= 125 & diametro < 175, 2,
                ifelse(diametro >= 175 & diametro < 225, 3, 4)))
}

arboles_41$gruposDiam<-asignar_valor(arboles_41$dbh)

table(arboles_41$gruposDiam)
