################################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK - Cristobal ORDOÑEZ 
##    
##    File name: Modelo Atura E41
##    Purpose: Modelo para obtener la ecuación de relacion altura-diámetro
##             para la especie 41
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
#                             ESPECIE 41
################################################################################
## Selecionamos los datos
alturaDiamE41<-alturaDiam[alturaDiam$Especie==41,]

## ((1)) Calculamos el BAL
## Ordenamos el data frame, por el plotID y el dbh, en orden decreciente
alturaDiam41<-alturaDiamE41[order(alturaDiamE41$PlotID0,alturaDiamE41$dbh,decreasing = T),]
## BAL 
BAL<-c()
for(i in 1:length(unique(alturaDiam41$PlotID0))){
  dat<-alturaDiam41[alturaDiam41$PlotID0==unique(alturaDiam41$PlotID0)[i],]
  aux<-cumsum(c(0,dat$g_ha[-length(dat$g_ha)]))
  BAL<-c(BAL,aux)
}
## Añadimos la variable
alturaDiam41$BAL<-BAL

head(alturaDiam41);tail(alturaDiam41)

################################################################################
#                   Análisis en componentes principales
################################################################################
## Realizamos un acp para elegir las variables que se consideran más 
## significativas para el modelo 

## Grupos de variables: Queremos que al menos haya una variable representando 
## cada grupo
tamaño41<-data.frame(alturaDiam41$Altura,alturaDiam41$ProxVol,alturaDiam41$perimetro,alturaDiam41$dbh,alturaDiam41$esbeltez) 
competencia41<-data.frame(alturaDiam41$BAL,alturaDiam41$BALTotal)
densidad41<-data.frame(alturaDiam41$G,alturaDiam41$N,alturaDiam41$Dg,alturaDiam41$HartB,alturaDiam41$Hart,alturaDiam41$AlturaDominante,alturaDiam41$SDI)
especificidad41<-data.frame(alturaDiam41$porcentajeG,alturaDiam41$porcentejeN)
datos41<-cbind(tamaño41,competencia41,densidad41,especificidad41)
colnames(datos41)<-c("Altura","ProxVol","perimetro","dbh","esbeltez","BAL","BALTotal","G","N","Dg","HartB","Hart","AlturaDominante","SDI","porcentajeG","porcentajeN")

## Estandarizamos los datos
datos41<-scale(datos41)

## Realizamos el ACP para cada conjunto
acp41<-prcomp(datos41)

## Scores
scores=as.data.frame(acp41$x); scores

## Autovectores
autovec<-acp41$rotation

## Autovalores
autoval<-acp41$sdev^2
autovalR<-round(autoval,6)
porcentaVar<-round((autoval/16)*100,4)
porcentaVarCum<-cumsum(porcentaVar)
tablaAutoval<-data.frame(autovalR,porcentaVar,porcentaVarCum)
tablaAutoval
## Aparentemente se puede ver como parece que necesitamos 5 componentes para obtener un 84% de variabilidad
## Comprobamos graficamente
screeplot(acp41)


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
correlaciones = as.data.frame(cor(datos41, acp41$x))
## Este data frame nos permite elegir las variables que son más representativas
## en cada una de las componentes, es decir, las que tienen más peso

################################################################################
## PC1 y PC2
# data frame con coordenadas de las flechas
arrows = data.frame(x1 = c(rep(0,ncol(datos41))), y1 = c(rep(0,ncol(datos41))), x2 = correlaciones$PC1, 
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
arrows2 = data.frame(x1 = c(rep(0,ncol(datos41))), y1 = c(rep(0,ncol(datos41))), x2 = correlaciones$PC3, 
                     y2 = correlaciones$PC4)

# geom_path construye los círculos
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlaciones, aes(x = PC3, y = PC4, label = rownames(correlaciones))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "eje pc3",  y = "eje pc4") + ggtitle("Círculo de correlaciones")
correlaciones
#PC1: ProxVol /  perimetro / dbh / esbeltez / HartB / Dg
#PC2: Altura / SDI / G / AlturaDominante
#PC3: BALTOTAL / BAL / porcentajeN
#PC4: porcentajeG

## Por tanto, las variables que vamos a incluir en el modelo son 
## Altura / dbh / esbeltez / HartB /Dg / G / SDI / AlturaDom /BALTOTAL / BAL / porcentajeN / porcentajeG 

################################################################################
## Depuramos la variable forma.
## Los valores de las formas 3 y 6 apenas nos interesan, por lo que las eliminamos
alturaDiam41<-subset(alturaDiam41,Forma!=3)
alturaDiam41<-subset(alturaDiam41,Forma!=6)
## Las formas 4 y 5 apenas son diferentes por lo que las agrupamos en una única
## categoria
alturaDiam41$Forma<-with(alturaDiam41,ifelse(Forma==4,5,Forma))
## Necesitamos que la variable forma sea un factor
alturaDiam41$Forma<-as.factor(alturaDiam41$Forma)
## Variable dummy para el modelo
alturaDiam41$dummyF2<-with(alturaDiam41,ifelse(alturaDiam41$Forma==2,1,0))


################################################################################
#                       MODELO ALTURA-DIAM
################################################################################
## TRANSFORMACION SQRT: Para que los residuales queden normales
modL1<-lm(Altura~dbh+esbeltez+HartB+Dg+G+SDI+AlturaDominante+BALTotal+BAL+porcentejeN+porcentajeG+dummyF2,data=alturaDiam41)
summary(modL1)
## Seleccionamos las variables significativas
modL2<-lm(Altura~dbh+esbeltez+HartB+Dg+G+SDI+AlturaDominante+BALTotal+porcentejeN+porcentajeG+dummyF2,data=alturaDiam41)
summary(modL2)
coef(modL2)

## Estudiamos la distribución de los residuales
resLS<-rstudent(modL2)
ks.test(resLS,"pnorm")
shapiro.test(resLS)

qqnorm(resLS)
qqline(resLS)
par(mfrow=c(2,2))
plot(modL2)

## Aunque en shapiro test se rechace la normalidad, el test de Kolmogorov si
## la acepta ya que es un test menos rígio, pero igualmente válido

## Analisis de colinealidad
## Tolerarnaci => Tiene que estar por debajo de 0.1
## Def => 1/Tolerancia =  Infalcion de varianza. Si es mas de 10 quiere decir que la
## varianza esta inflada, es malo

## Factor de inflacion de la varianza
vif(modL2)

## Podemos ver como en el modelo propuesto hay colinealidad por tanto no se 
## acepta. Vamos a formular un nuevo modelo en el que se evite esto














mod<-lm((vRes)~dbh.x+esbeltez.x+Dg+(G.x)+SDI+BAL+BALTotal+porcentajeG+dummyF2,data=alturaDiam41)
stepAIC(mod)

mod1<-lm((vRes)~dbh.x+esbeltez.x+Dg+G.x+porcentajeG+dummyF2, data = alturaDiam41)
summary(mod1)

## Estudiamos la distribución de los residuales
res1<-rstudent(mod1)
ks.test(res1,"pnorm")
shapiro.test(res1)
qqnorm(res1)
qqline(res1)
par(mfrow=c(2,2))
plot(modL2S)

## Estudiamos la colinealidad
#Inflacion de la varianza
vif(mod1)
#Tolerancia
1/vif(mod1)
## Test de indep de Dubin-Watson
durbinWatsonTest(mod1)

















## Tolerancia
tolerancia<-1/vif(modL2);tolerancia

## Indice de condicion

## Proporcion de variación

## Test de indep de Bubin-Watson
durbinWatsonTest(modL2S)
## Tiene que ser mayor de 1.8 

## DCook
cooks.distance(modL2S)
# Identificación de los puntos influyentes
umbral_cook <- 4/(nrow(alturaDiam41) - length(modL2S$coefficients) - 1) # Umbral sugerido
puntos_influyentes_cook <- which(cooks.distance(modL2S) > umbral_cook)

## Residuales
residuals(modL2S)

## Coeficiente de autocorrelacion
acf(residuals(modL2S), lag.max = nrow(alturaDiam41)-1, plot = FALSE)$acf