################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK  - Cristobal ORDOÑEZ 
##    
##    File name: Mod mortalidad 41
##    Purpose: Modelo para obtener la ecuacion de mortalidad de la especie 41
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

setwd("C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Mortalidad")

## Leemos los datos
mortalidadT0<-read.csv("mortalidadT0.csv")
arbolesMortalidad<-read.csv("arbolesMortalidad.csv")

###############################################################################
#                     SEPARAMOS POR ESPECIES
# 
#                             ESPECIE 41
################################################################################
## Selecionamos los datos
mortalidad41<-mortalidadT0[mortalidadT0$Especie==41,]
arbolesMortalidad_41<-arbolesMortalidad[arbolesMortalidad$Especie==41,]

## Calculamos la altura dominante por especie
## ((3)) Calculamos la altura dominante
alturaDominanteE<-c()
plots<-c()
for(i in 1:length(unique(arbolesMortalidad_41$PlotID0))){
  dat<-arbolesMortalidad_41[arbolesMortalidad_41$PlotID0==unique(arbolesMortalidad_41$PlotID0)[i],]
  plots<-c(plots,unique(arbolesMortalidad_41$PlotID0)[i])
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
mortP_41<-merge(mortalidad41,datosDA41,by="PlotID0",all.y=TRUE)
mortP_41$HartB41<-with(mortP_41,(10000/(sqrt((sqrt(3)*N)/2)*AlturaDom41)))

################################################################################
#
#               MODELOS DE REGRESIÓN
#
################################################################################
## Ya tenemos las variables necesarias para el modelo ya que en este caso solo
## necesitamos las variables de parcelas
## Al tener pocas variables no será necesario realizar un ACP para la selección
## de variables
modelo1<-glm(Mortalidad~G+N+porcentajeG+porcentejeN+Dg+SDI+AlturaDom+HartB+AlturaDom41+HartB41,data=mortP_41,family = poisson())
summary(modelo1)
modelo2<-glm(Mortalidad~G+N+porcentejeN+Dg+SDI+AlturaDom+HartB+HartB41,data=mortP_41,family = poisson())
summary(modelo2)
PseudoR2(modelo2, which = "McFadden")

## Factor de inflacion de la varianza
vif(modelo2) #No debe de ser superior a 10, ya que si lo es nos indica que tiene colinealidad

modelo3<-glm(Mortalidad~N+porcentejeN+Dg+SDI+AlturaDom+HartB+HartB41,data=mortP_41,family = poisson())
summary(modelo3)
PseudoR2(modelo3, which = "McFadden")
vif(modelo3)

modelo4<-glm(Mortalidad~N+porcentejeN+Dg+SDI+AlturaDom+HartB41,data=mortP_41,family = poisson())
summary(modelo4)
PseudoR2(modelo4, which = "McFadden")
vif(modelo4)

#MODELO PARA LA ESPECIE 41
coefficients(modelo4)

#Test de independencia
durbinWatsonTest(modelo4)


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

arbolesMortalidad_41$gruposDiam<-asignar_valor(arbolesMortalidad_41$dbh)

table(arbolesMortalidad_41$gruposDiam)

