################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK  - Cristobal ORDOÑEZ 
##    
##    File name: Mod crecimiento 72
##    Purpose: Modelo para obtener la ecuacion de crecimiento de la especie 72
##    
##    
################################################################################
rm(list=ls())

library(lme4)
library(lmerTest)
library(MuMIn)
library(MASS)
library(ggplot2)

setwd("C:/Users/Irene/Documents/INF COMFOR/Regresion")

## Leemos los datos
crecimientoTO<-read.csv("crecimientoTO.csv")

###############################################################################
#                     SEPARAMOS POR ESPECIES
# 
#                             ESPECIE 72
################################################################################
## Seleccionamos los datos
crecimiento72<-crecimientoTO[crecimientoTO$Especie.x==72,]

## ((1)) Calculamos el BAL
## Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
crecimientoTO72<-crecimiento72[order(crecimiento72$PlotID.x,crecimiento72$dbh.x,decreasing = T),]
## BAL 
BAL<-c()
for(i in 1:length(unique(crecimientoTO72$PlotID.x))){
  dat<-crecimientoTO72[crecimientoTO72$PlotID.x==unique(crecimientoTO72$PlotID.x)[i],]
  aux<-cumsum(c(0,dat$g_ha.x[-length(dat$g_ha.x)]))
  BAL<-c(BAL,aux)
}
crecimientoTO72$BAL<-BAL

head(crecimientoTO72);tail(crecimientoTO72)

################################################################################
#                   Análisis en componentes principales
################################################################################
## Realizamos un acp para elegir las variables que se consideran más 
## significativas para el modelo 

## Grupos de variables: Queremos que al menos haya una variable representando 
## cada grupo
tamaño72<-data.frame(crecimientoTO72$vRes,crecimientoTO72$Altura.x,crecimientoTO72$ProxVol,crecimientoTO72$perimetro.x,crecimientoTO72$dbh.x,crecimientoTO72$esbeltez.x) 
competencia72<-data.frame(crecimientoTO72$BAL,crecimientoTO72$BALTotal)
densidad72<-data.frame(crecimientoTO72$G.x,crecimientoTO72$N.x,crecimientoTO72$Dg,crecimientoTO72$HartB,crecimientoTO72$Hart,crecimientoTO72$AlturaDominante,crecimientoTO72$SDI)
especificidad72<-data.frame(crecimientoTO72$porcentajeG,crecimientoTO72$porcentejeN)
datos72<-cbind(tamaño72,competencia72,densidad72,especificidad72)
colnames(datos72)<-c("vRes","Altura.x","ProxVol","perimetro.x","dbh.x","esbeltez.x","BAL","BALTotal","G.x","N.x","Dg","HartB","Hart","AlturaDominante","SDI","porcentajeG","porcentajeN")

## Estandarizamos los datos
datos72<-scale(datos72)

## Realizamos el ACP para cada conjunto
acp72<-prcomp(datos72)

## Scores
scores<-as.data.frame(acp72$x) ; scores

## Autovectores
autovec<-acp72$rotation

## Autovalores
autoval<-acp72$sdev^2
autovalR<-round(autoval,6)
porcentaVar<-round((autoval/17)*100,4)
porcentaVarCum<-cumsum(porcentaVar)
tablaAutoval<-data.frame(autovalR,porcentaVar,porcentaVarCum)
tablaAutoval
## Aparentemente se puede ver como parece que necesitamos 5 componentes para obtener un 88% de variabilidad
## Comprobamos graficamente
screeplot(acp72)

# Función para crear un círculo
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

## Creación de data frame con correlaciones entre variables y PCs
correlaciones = as.data.frame(cor(datos72, acp72$x))
## Este data frame nos permite elegir las variables que son más representativas
## en cada una de las componentes, es decir, las que tienen más peso

################################################################################
## PC1 y PC2
# data frame con coordenadas de las flechas
arrows = data.frame(x1 = c(rep(0,ncol(datos72))), y1 = c(rep(0,ncol(datos72))), x2 = correlaciones$PC1, 
                    y2 = correlaciones$PC2)

# geom_path construye los círculos
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlaciones, aes(x = PC1, y = PC2, label = rownames(correlaciones))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "eje pc1", 
                                                                                                                           y = "eje pc2") + ggtitle("Círculo de correlaciones")
################################################################################
## PC3 y PC4   
# data frame con coordenadas de las flechas
arrows2 = data.frame(x1 = c(rep(0,ncol(datos72))), y1 = c(rep(0,ncol(datos72))), x2 = correlaciones$PC3, 
                     y2 = correlaciones$PC4)

# geom_path construye los círculos
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlaciones, aes(x = PC3, y = PC4, label = rownames(correlaciones))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "eje pc3",  y = "eje pc4") + ggtitle("Círculo de correlaciones")

################################################################################
## PC5 y PC1   
# data frame con coordenadas de las flechas
arrows2 = data.frame(x1 = c(rep(0,ncol(datos72))), y1 = c(rep(0,ncol(datos72))), x2 = correlaciones$PC5, 
                     y2 = correlaciones$PC1)

# geom_path construye los círculos
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlaciones, aes(x = PC5, y = PC1, label = rownames(correlaciones))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "eje pc5",  y = "eje pc1") + ggtitle("Círculo de correlaciones")



correlaciones
#PC1: vRes / ProxVol /  Perimetro / dbh.x / esbeltez / HartB 
#PC2: G.x / SDI
#PC3: BALTOTAL / BAL 
#PC4: porcentajeG
#PC5: Altura.x

################################################################################
## Depuramos la variable forma.
## Los valores de las formas 3 y 6 apenas nos interesan, por lo que las eliminamos
crecimientoTO72<-subset(crecimientoTO72,Forma.x!=3)
crecimientoTO72<-subset(crecimientoTO72,Forma.x!=6)
## Las formas 4 y 5 apenas son diferentes por lo que las agrupamos en una única
## categoria
crecimientoTO72$Forma.x<-with(crecimientoTO72,ifelse(Forma.x==4,5,Forma.x))
## Necesitamos que la variable forma sea un factor
crecimientoTO72$Forma.x<-as.factor(crecimientoTO72$Forma.x)
## Variable dummy para el modelo
crecimientoTO72$dummyF2<-with(crecimientoTO72,ifelse(crecimientoTO72$Forma.x==2,1,0))

## Por tanto, las variables que vamos a incluir en el modelo son 
## vRes / g.x / dbh.x / esbeltez.x / G.x / SDI / BALTOTAL / BAL / porcentajeG / Altura.x /dummyF2


################################################################################
#                       MODELO DE CRECIMIENTO
################################################################################
modL1_72<-lm(vRes~dbh.x+esbeltez.x+HartB+G.x+SDI+BAL+BALTotal+porcentajeG+Altura.x+dummyF2,data=crecimientoTO72)
summary(modL1_72)
## Seleccionamos las variables significativas
modL2_72<-lm(vRes~dbh.x+esbeltez.x+HartB+G.x+BAL+BALTotal+Altura.x+dummyF2,data=crecimientoTO72)
summary(modL2_72)
coef(modL2_72)

## Estudiamos la distribución de los residuales
resL_72<-rstudent(modL2_72)
ks.test(resL_72,"pnorm")
shapiro.test(resL_72)
qqnorm(resL_72)
qqline(resL_72)
## No se distribuye normalmente