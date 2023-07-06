################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Modelo Atura E72
##    Purpose: Modelo para obtener la ecuación de relacion altura-diámetro
##             para la especie 72
##    
################################################################################
rm(list=ls())

library(lme4)
library(lmerTest)
library(MuMIn)
library(MASS)
library(ggplot2)
library(car)

setwd("C:/Users/Irene/Documents/INF COMFOR/Regresion/Mod Altura")

## Leemos los datos
alturaDiam<-read.csv("alturaDiam.csv")

###############################################################################
#                     SEPARAMOS POR ESPECIES
# 
#                             ESPECIE 72
################################################################################
## Selecionamos los datos
alturaDiamE72<-alturaDiam[alturaDiam$Especie==72,]

## ((1)) Calculamos el BAL
## Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
alturaDiam72<-alturaDiamE72[order(alturaDiamE72$PlotID0,alturaDiamE72$dbh,decreasing = T),]
## BAL 
BAL<-c()
for(i in 1:length(unique(alturaDiam72$PlotID0))){
  dat<-alturaDiam72[alturaDiam72$PlotID0==unique(alturaDiam72$PlotID0)[i],]
  aux<-cumsum(c(0,dat$g_ha[-length(dat$g_ha)]))
  BAL<-c(BAL,aux)
}
## Añadimos la variable
alturaDiam72$BAL<-BAL

head(alturaDiam72);tail(alturaDiam72)

################################################################################
#                   Análisis en componentes principales
################################################################################
## Realizamos un acp para elegir las variables que se consideran más 
## significativas para el modelo 

## Grupos de variables: Queremos que al menos haya una variable representando 
## cada grupo
tamaño72<-data.frame(alturaDiam72$Altura,alturaDiam72$ProxVol,alturaDiam72$perimetro,alturaDiam72$dbh,alturaDiam72$esbeltez) 
competencia72<-data.frame(alturaDiam72$BAL,alturaDiam72$BALTotal)
densidad72<-data.frame(alturaDiam72$G,alturaDiam72$N,alturaDiam72$Dg,alturaDiam72$HartB,alturaDiam72$Hart,alturaDiam72$AlturaDominante,alturaDiam72$SDI)
especificidad72<-data.frame(alturaDiam72$porcentajeG,alturaDiam72$porcentejeN)
datos72<-cbind(tamaño72,competencia72,densidad72,especificidad72)
colnames(datos72)<-c("Altura","ProxVol","perimetro","dbh","esbeltez","BAL","BALTotal","G","N","Dg","HartB","Hart","AlturaDominante","SDI","porcentajeG","porcentajeN")

## Estandarizamos los datos
datos72<-scale(datos72)

## Realizamos el ACP para cada conjunto
acp72<-prcomp(datos72)

## Scores
scores=as.data.frame(acp72$x); scores

## Autovectores
autovec<-acp72$rotation

## Autovalores
autoval<-acp72$sdev^2
autovalR<-round(autoval,6)
porcentaVar<-round((autoval/16)*100,4)
porcentaVarCum<-cumsum(porcentaVar)
tablaAutoval<-data.frame(autovalR,porcentaVar,porcentaVarCum)
tablaAutoval
## Aparentemente se puede ver como parece que necesitamos 5 componentes para obtener un 84% de variabilidad
## Comprobamos graficamente
screeplot(acp72)


## Función para crear un círculo
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
correlaciones
#PC1: ProxVol /  perimetro / dbh / esbeltez / HartB / Dg / N
#PC2: SDI / G 
#PC3: BALTOTAL / BAL 
#PC4: porcentajeG / porcentajeN
#PC5: Altura / AlturaDominante

## Por tanto, las variables que vamos a incluir en el modelo son 
## Altura / dbh / esbeltez / HartB /Dg / N / G / SDI / AlturaDom /BALTOTAL / BAL / porcentajeN / porcentajeG 

################################################################################
## Depuramos la variable forma.
## Los valores de las formas 3 y 6 apenas nos interesan, por lo que las eliminamos
alturaDiam72<-subset(alturaDiam72,Forma!=3)
alturaDiam72<-subset(alturaDiam72,Forma!=6)
alturaDiam72<-subset(alturaDiam72,Forma!=1)
## Las formas 4 y 5 apenas son diferentes por lo que las agrupamos en una única
## categoria
alturaDiam72$Forma<-with(alturaDiam72,ifelse(Forma==4,5,Forma))
## Necesitamos que la variable forma sea un factor
alturaDiam72$Forma<-as.factor(alturaDiam72$Forma)
## Variable dummy para el modelo
alturaDiam72$dummyF2<-with(alturaDiam72,ifelse(alturaDiam72$Forma==2,1,0))


################################################################################
#                       MODELO ALTURA-DIAM
################################################################################
## TRANSFORMACION SQRT: Para que los residuales queden normales
modL1<-lm(Altura~dbh+esbeltez+HartB+Dg+N+G+SDI+AlturaDominante+BALTotal+BAL+porcentejeN+porcentajeG+dummyF2,data=alturaDiam72)
summary(modL1)
## Seleccionamos las variables significativas
modL2<-lm(Altura~dbh+esbeltez+Dg+N+SDI+AlturaDominante+BALTotal+BAL+porcentejeN+porcentajeG+dummyF2,data=alturaDiam72)
summary(modL2)

## Analisis de colinealidad
## Tolerarnaci => Tiene que estar por debajo de 0.1
## Def => 1/Tolerancia =  Infalcion de varianza. Si es mas de 10 quiere decir que la
## varianza esta inflada, es malo

## Factor de inflacion de la varianza
vif(modL2)

## Podemos ver como en el modelo propuesto hay algo de colinealidad 
## Vamos a formular un nuevo modelo en el que se evite esto
modL3<-lm(Altura~dbh+esbeltez+Dg+N+SDI+AlturaDominante+BAL+porcentejeN+porcentajeG+dummyF2,data=alturaDiam72)
summary(modL3)

## Factor de inflacion de la varianza
vif(modL3)

## Tolerancia
tolerancia<-1/vif(modL3);tolerancia

## Estudiamos la distribución de los residuales
res3<-rstudent(modL3)
ks.test(res3,"pnorm")
shapiro.test(res3)
qqnorm(res3)
qqline(res3)
par(mfrow=c(2,2))
plot(modL3)

## DCook
cooks.distance(modL3)
# Identificación de los puntos influyentes
umbral_cook <- 4/(nrow(alturaDiam72) - length(modL3$coefficients) - 1) # Umbral sugerido
puntos_influyentes_cook <- which(cooks.distance(modL3) > umbral_cook)

## Coeficiente de autocorrelacion
acf(residuals(modL3), lag.max = nrow(alturaDiam72)-1, plot = FALSE)$acf

## He probado realizando transformaciones a la variable respuesta para intenatar
## mejorar el modelo y que se asumiera normalidad, pero al no conseguirse, nos
## quedamos con el modelo más sencillo disponible. También he probado con los 
## modelos mixtos pero tampoco han mejorado
