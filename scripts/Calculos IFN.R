########################################################################
##    
##  MAIN AUTHOR - Irene ARROYO  
##    
##  CHECK & COMENTS - Cristobal ORDOÑEZ 
##    
##    File name: NFICalcFinal
##    Purpose: Read of Spanish NFI plots and selection of 
##             plurispecific and multistratificated
##             monoespecific plots 
##    
##    
########################################################################
rm(list=ls())

## load library
## carga de paquetes necesarios
library(dplyr)
library(tidyverse)
library(sm)
library(plotly)

## work folder
getwd()
## change to data folder
## setwd("/home/cristobal/github.R/COMFOR.NFI/data/")
## setwd("/home/cristobal/Downloads")
## setwd("C:/Users/Irene/Documents/INF COMFOR/")
## setwd("~/comfor/")
setwd("C:/Users/Irene/Documents/INF COMFOR/Entrega/")
dir()
ls()


##############################################################################
##
## 1 - data read        / rough data view
##     Lectura de datos / Estudio de las variables contenidas en el fichero arbol 
##
################################################################################
## read tree data from rData file
load('if3.RData')
str(trees.if3);str(plots.if3);str(plots.sp.if3)

##Tomamos los datos necesarios
arboles<-trees.if3
names(arboles)

if(is.null(arboles$Cla)){
  arboles$Cla<-0  
}else if(is.null(arboles$Subclase)){
  arboles$Subclase<-0
}else if(is.null(arboles$OrdenIFActual)){
  arboles$OrdenIfActual<-0
}else if(is.null(arboles$OrdenIFAnterior)){
  arboles$OrdenIfAnterior<-0
}

## PlotID identify each plot with province and form / Añadimos un indice, para saber cual nos interesa
arboles$PlotIDC<-with(arboles,paste0(PlotID,".",Cla,".",Subclase))

## Seleccion de variables que nos interesan
arboles<-arboles[,c("Estadillo","Cla","Subclase","NumOrden","Rumbo","Distanci",
                    "DRed", "Especie","Diametro1","Diametro2", "Altura","Calidad",
                    "Forma","Origen","OrdenIFAnterior","OrdenIFActual","PlotID")]


## Eliminamos las filas en las que o ya no este el arbol, o sea un arbol muerto
arboles<-arboles[arboles$OrdenIFActual!=0 & arboles$OrdenIFActual!=888 & !is.na(arboles$OrdenIFActual),  ]
arboles<-arboles[!is.na(arboles$Especie),]

################################################################################
#           rough data view
#           Estudio de las variables contenidas en el fichero arbol
################################################################################
str(arboles)

#Estadillo : (Plot id)
dim(table(arboles$PlotID)) #total de 62120 parcelas distintas para el estudio

#Diametro1 :
summary(arboles$Diametro1)

#Diametro2 : 
summary(arboles$Diametro2)

#Altura
summary(as.numeric(arboles$Altura))

#Especie : Dado como un número que equivale a una especie
#Nos permite ver el numero de árboles de cada especie que tenemos
table(arboles$Especie)
length(unique(arboles$Especie)) #Hay 160 especies distintas

################################################################################
#                     Cálculos de árbol individual
################################################################################
## Calculo del diametro normal promedio
arboles$dbh<-(arboles$Diametro1+arboles$Diametro2)/2
summary(arboles$dbh)

## Calculo del perimetro del arbol en mm
arboles$perimetro<-arboles$dbh*pi
summary(arboles$perimetro)

## Calculo del factor de expansion (arboles por ha): 
arboles$expan <- with(arboles, case_when(dbh < 75  ~ 0,
                                         dbh < 125 ~ 10000/(pi*(5^2)),
                                         dbh < 225 ~ 10000/(pi*(10^2)),
                                         dbh < 425 ~ 10000/(pi*(15^2)),
                                         TRUE      ~  10000/(pi*(25^2)) ) )
summary(arboles$expan)

## Calculo de la area basimetrica individual mm2
arboles$g<-with(arboles,Diametro1*Diametro2*pi/4)
summary(arboles$g)

## Calculo del area basimetrica m2 por hectarea 
arboles$g_ha <- arboles$g*arboles$expan/1000000
summary(arboles$g_ha)

## Calculo del bal (m²/ha)
## arboles<-arboles[order(arboles$PlotID, # ordeno árboles por provincia y estadillo
##                        arboles$dbh, # y por diámetro
##                        decreasing = TRUE), ] # de mayor a menor

## Calculo de la esbeltez, ratio altura diametro, adimensional 
arboles$esbeltez<-arboles$Altura*1000/arboles$dbh
summary(arboles$esbeltez)

write.csv(arboles,"arboles.csv")

################################################################################
#           Analisis previo de las variables
################################################################################
#Representar el % de area basimetrica para cada una de las especies 
plot_ly(arboles, labels = ~Especie, values = ~g, type = "pie",
        textinfo = "label+percent", insidetextorientation = "radial")
#Calidad 
plot_ly(y=table(arboles$Calidad),x=names(table(arboles$Calidad)),type="bar",
        marker=list(color="lightgreen"))

#Forma
plot_ly(y=table(arboles$Forma),x=names(table(arboles$Forma)),type="bar",
        marker=list(color="lightblue"))

#Origen
plot_ly(y=table(arboles$Origen),x=names(table(arboles$Origen)),type="bar",
        marker=list(color="pink"))

################################################################################
#        Calculo de variables de parcela, total y por especie 
################################################################################

## calculus of stand variables, basal area and tph
## Para todas la parcelas, sin tener en cuenta la especie, calculamos el area basal y la densidad, es decir, la G y N totales
#Para todas la parcelas, sin tener en cuenta la especie, calculamos el area 
#basal y la densidad, es decir, la G y N totales

parcelasOE <-     arboles %>%    group_by(PlotID) %>%    summarise(
  G = sum(g_ha, na.rm = TRUE),  # basal area (m2/ha)
  N = sum(expan, na.rm = TRUE)  # density (trees/ha)
)
head(parcelasOE); tail(parcelasOE)

## calculus of stand variables by plot and species, basal area and tph
## Repetimos el mismo procedimiento pero ahora teniendo en cuenta las especies, para así obtener el G para cada especie y posteriormente compararle con el total

parcelasOEE <-    arboles %>%    group_by(PlotID,Especie) %>%    summarise(
  G.e = sum(g_ha, na.rm = TRUE),  # basal area (m2/ha)
  N.e = sum(expan, na.rm = TRUE)  # density (trees/ha)
)
head(parcelasOEE); tail(parcelasOEE)

## Merging both dataframes, sumaries by plot and species with sumaries by plot
## Unimos ambos data frame, para tener en uno tanto la G total como la G correspondiente a cada especie
parcelasT<-merge(parcelasOE,parcelasOEE,by=c("PlotID"))

## Calculamos los porcentajes que representa cada especie en dicho estadillo
parcelasT$porcentajeG<-parcelasT$G.e/parcelasT$G
parcelasT$porcentejeN<-parcelasT$N.e/parcelasT$N
head(parcelasT); tail(parcelasT); dim(parcelasT)

write.csv(parcelasT,"parcelasPorcentajes.csv")

################################################################################
#                       PARCELAS MIXTAS
################################################################################
## Nos interesa encontrar los estadillos que tengas dos o más especies distintas (con al menos una representación del 10% del total)
parcelasP<-parcelasT[parcelasT$porcentajeG>0.15 & parcelasT$G>10,]
dim(parcelasP); head(parcelasP); tail(parcelasP)

## Nos intersan aquellas especies que tienen 2 o más especies
cont <- parcelasP %>% count(PlotID) %>% filter(n>1)
head(cont)
table(cont$n)
summary(cont$n)

##
parcelasPluriSP_sps <- as.data.frame( parcelasP[ which( parcelasP$PlotID %in% cont$PlotID ), ])
head(parcelasPluriSP_sps); tail(parcelasPluriSP_sps); str(parcelasPluriSP_sps)

## Output plot variables, total y por especies
write.csv(parcelasPluriSP_sps,'of_plotsPluriSP_sps.csv')

str(parcelasOE)
## plots: total
posPluriSP <- which( parcelasOE$PlotID %in% cont$PlotID )
parcelasPluriSP <- as.data.frame( parcelasOE[ posPluriSP, ])
head(parcelasPluriSP); tail(parcelasPluriSP); str(parcelasPluriSP)

## output file with total basal area and tph of multiespecific plots
write.csv(parcelasPluriSP,'of_plotsPluriSP.csv')

################################################################################
#                       PARCELAS MONOESPECÍFICAS
################################################################################

## Monospecific plots
parcelasMonoSP <- as.data.frame( parcelasOE[ -posPluriSP, ])
head(parcelasMonoSP); tail(parcelasMonoSP); str(parcelasMonoSP)

## output file with total basal area and tph of multiespecific plots
write.csv(parcelasMonoSP,'of_plotsMonoSP.csv')

## Comprobamos en dbh y en altura
##  * Normalidad
##  * Plurimodalidad

## Una parcela monoespecífica es aquella que tiene al menos un 90% de masa de esa especie y al menos 10m2/ha de área basimétrica
arboles1 <- arboles[ which( arboles$PlotID %in%
                              parcelasT[parcelasT$porcentajeG > 0.9 & parcelasT$G > 10, 'PlotID']), ]
dim(arboles1)

## Eliminamos aquellas parcelas que tengan menos de 5 arboles con distintas medidas
contA <- arboles1 %>% count(PlotID) %>% filter(n>5) #que tenga al menos 6 repeticiones
arbolesF <- arboles1[ which( arboles1$PlotID %in% contA$PlotID), ]
head(arbolesF); tail(arbolesF); dim(arbolesF)

## Removal of plots with less than 5 trees / Eliminamos aquellas parcelas que tengan menos de 5 arboles con distintas medidas
contA <- arboles1 %>% count(PlotID) %>% filter(n>5) #que tenga al menos 6 repeticiones
arbolesF <- arboles1[ which( arboles1$PlotID %in% contA$PlotID), ]
head(arbolesF); tail(arbolesF); dim(arbolesF)

########################################################################
##
##  4.1 - Variable == total height / Altura
##
########################################################################

########################################################################
##  4.1.1 - Test == NORMALIDAD
########################################################################

## Nos interesa dbh o ht, teniendo en cuenta expan, que será el peso
## Para comprobar la normalidad, haremos por ejemplo un test de normalidad
## Ho: f es normal.
## H1: f no es normal

## Vamos a comporbar si la distribución de altura (ht sigue normalidad)
## Tenemos que tener en cuenta de que el peso es expan, ya que signifca que con una carácteristicas determinadas hay tantos árboles iguales

## Generalizamos
posicion <- data.frame(arbolesF$PlotID)
posicionU <- unique(posicion)
tam <- dim(posicionU)[1]

resultadosN <- matrix(NA,nrow=tam,ncol=2) ## Matriz para alamacenar los datos
colnames(resultadosN) <- c("Pvalor", 'PlotID')

StartTime <- Sys.time()
for(i in 1:tam){
  d <- arbolesF[arbolesF$PlotID == posicionU$arbolesF.PlotID[i], ]
  d <- d[c("Altura","expan")]
  ## print(posicionU$arbolesF.Origen[i])
  ## print(posicionU$arbolesF.Estadillo[i])
  x <- d$Altura
  ## Comprobamos que no tengan una única altura ya que eso no nos dará ninguna distribución y será una única recta
  if(length(unique(x))==1){
    next
  }
  w <- d$expan
  f <- c(rep(x,w))
  ## Comprobamos el rango, es decir nos interesan solo aquellos que pueden tener distintas edades, y para ello nos quedaremos con que se cumpla que el rango es mayor que la mitad del máximo
  if(max(f)-min(f)<(max(f)/2)){
    next
  }
  ## Que no supere el límite de 5000 del shapiro test
  if(length(f)>4999){
    next
  }  
  resultadosN[i,] <- c(shapiro.test(f)$p.value,
                       ## posicionU$arbolesF.Origen[i],
                       ## posicionU$arbolesF.Estadillo[i],
                       posicionU$arbolesF.PlotID[i])
  
  print(paste('Plot # ',i, ' of ', tam, ' plots. ',
              ' - - Plot Id: ', resultadosN[i,2],
              ' - - PValue: ', resultadosN[i,1]) )
}
EndTime <- Sys.time()
print(paste('Time spend: start at ', StartTime, ' end at ', EndTime) )

## Nos interesan aquellos en los que se rechaze normalidad, es decir, aquellos cuyo pvalor sea <0.05 Primero eliminamos los NA que se han producido, es decir, aquellos que solo tienen una altura y aquellos con más de 5000 datos
resultadosND <- as.data.frame(resultadosN)
resultadosNF <- na.omit(resultadosND)
write.csv(resultadosNF,'of_plot.ht.normality.csv')

## Comprobamos a ver si hay algún valor que se distribuya normalmente, es decir, que su pvalor sea>0.05, de forma que no se rechaza la hipotesis nula
any(as.numeric(resultadosNF$Pvalor) > 0.05)
## No hay ninguno que sea normal, por lo que nos interesan todos


########################################################################
##
## 4.0.0 - PLURIMODALIDAD
##         Functions to test
##
########################################################################

## Test de modalidad
## h0: f es unimodal
## h1: f es plurimodal

## 1. Programamos una función que nos cuente el número de modas que tienen dichos datos. La usaremos para ver cuantas modas tiene la función estimada, ya que sm.density() nos proporciona la densidad de unos datos en unos puntos
nModas<-function(f){
  cont<-0
  crece<-TRUE
  n<-length(f)-1
  for(i in 1:n){
    if(crece){
      if(f[i]<=f[i+1]){
        crece<-TRUE
      }else{
        if(f[i]>f[i+1]){
          cont<-cont+1
          crece<-FALSE
        }
      }
    }else{
      if(f[i]<=f[i+1]){
        crece<-TRUE
      }
    }
  }
  return(cont)
}

## 2. Generamos muestras de Bootstrap suavizado a partir de la original usando un parametro de suavizado h. Repetimos el proceso n veces 
fBootstrap<-function(f,hobs){
  m<-length(f)
  y<-sample(f,m,replace=T)+rnorm(m)*hobs
  return(y)
}

## 3. Calculamos el estadistico test
h.unimodal <- function(datos){
  estimacion<-sm.density(datos, display = "none")
  h0<-estimacion$h
  m<-nModas(estimacion$estimate)
  if(m>1){
    m.vect<-c()
    while(length(m.vect[m.vect==1])==0){
      m.vect<-c()
      c<-1
      h.grid<-seq(h0, 1.1*h0, (0.1*h0)/40)
      for(i in h.grid){
        m.vect[c]<-nModas(sm.density(datos, h = i, display = "none")$estimate)
        c<-c+1
      }
      h0=1.1*h0
    }
  }else{
    m.vect <- c(1)
    while(m.vect[1]==1){
      m.vect<-c()
      c<-1
      h.grid<-seq(h0/1.1, h0, (h0-h0/1.1)/80)
      for(i in h.grid){
        m.vect[c] <- nModas(sm.density(datos, h = i, display = "none")$estimate)
        c <- c+1
      }
      h0 = h0/1.1
    }
  }
  return(min(h.grid[m.vect == 1]))
}

## 4.
unimodal.test <- function(data, iter = 20){
  hobs <- h.unimodal(data)
  pvalor <- 0
  for(i in 1:iter){
    xboot <- fBootstrap(data, hobs)
    fb <- sm.density(xboot, h = hobs, display = "none")$estimate
    if(nModas(fb) > 1){
      pvalor <- pvalor+1
    }
  }
  return(pvalor/iter)
}

########################################################################
##
## 4.1.2 - PLURIMODALIDAD
##         test for tree height distribution
##
########################################################################

## Los datos con los que tendremos que trabajar serán con los mismos que en normalidad, es decir, no nos interesan aquellos que solo tengan una única altura. También tendremos que eliminar el estadillo que se distribuye normalmente
arboles_filtrados <- arbolesF[arbolesF$PlotID %in% resultadosNF$PlotID, ]
dim(arboles_filtrados)

posicionM<-data.frame(arboles_filtrados$PlotID)
posicionUM<-unique(posicionM)
tam2<-dim(posicionUM)[1]

## Generalizamos
resultadosM <- matrix(NA,nrow=tam2,ncol=2)
colnames(resultadosM)<-c("Pvalor","PlotID")

StartTime <- Sys.time()
for(i in 1:tam2){
  d <- arboles_filtrados[arboles_filtrados$PlotID == posicionUM$arboles_filtrados.PlotID[i], ]
  d <- d[c("Altura","expan")]
  ## print(posicionUM$arboles_filtrados.Origen[i])
  ## print(posicionUM$arboles_filtrados.Estadillo[i])
  ## print(posicionUM$arboles_filtrados.PlotID[i])
  
  x<-d$Altura
  w<-d$expan
  f<-c(rep(x,w))
  sm.density(f)  
  resultadosM[i,] <- c(unimodal.test(f,100), ## posicionUM$arboles_filtrados.Origen[i], posicionUM$arboles_filtrados.Estadillo[i],
                       posicionUM$arboles_filtrados.PlotID[i] )
  
  print(paste('Plot # ',i, ' of ', tam2, ' plots. ',
              ' - - Plot Id: ',resultadosM[i,2],
              ' - - PValue: ', resultadosM[i,1] ) )
}
EndTime <- Sys.time()
print(paste('Time spend: start at ', StartTime, ' end at ', EndTime) )


resultadosMD<-as.data.frame(resultadosM)
head(resultadosMD);tail(resultadosMD)

write.csv(resultadosMD,"of_resultHeightPlurimodal.csv")

########################################################################
##
##  4.2 - Variable == dbh / diametro
##
########################################################################

########################################################################
##  4.2.1 - Test == NORMALIDAD
########################################################################

resultadosNDiam<-matrix(NA,nrow=tam,ncol=2)
colnames(resultadosNDiam) <- c("Pvalor", ## "Origen", "Estadillo",
                               'PlotID')

StartTime <- Sys.time()  
for(i in 1:tam){
  d <- arbolesF[arbolesF$PlotID == posicionU$arbolesF.PlotID[i], ]
  d <- d[c("dbh","expan")]
  ## print(posicionU$arbolesF.Origen[i])
  ## print(posicionU$arbolesF.Estadillo[i])
  ## print(posicionU$arbolesF.PlotID[i])
  x <- d$dbh
  #Comprbamos que haya mas de un único diametro
  if(length(unique(x))==1){
    next
  }
  w <- d$expan
  f <- c(rep(x,w))
  #Comprobamos el rango, es decir nos interesan solo aquellos que pueden tener distintas edades, 
  #y para ello nos quedaremos con que se cumpla que el rango es mayor que la mitad del máximo
  if(max(f)-min(f)<(max(f)/2)){
    next
  }
  
  #Comprobamos que no haya mas de 5000 diamteros
  if(length(f)>4999){
    next
  }  
  resultadosNDiam[i,] <- c(shapiro.test(f)$p.value,
                           ## posicionU$arbolesF.Origen[i],
                           ## posicionU$arbolesF.Estadillo[i],
                           posicionU$arbolesF.PlotID[i])
  print(paste('Plot # ',i,
              ' of ', tam, ' plots. ',
              ' - - Plot Id: ', resultadosNDiam[i,2],
              ' - - PValue: ', resultadosNDiam[i,1] ) )
}
EndTime <- Sys.time()
print(paste('Time spend: start at ', StartTime, ' end at ', EndTime) )

head(resultadosNDiam); tail(resultadosNDiam)

## Nos interesan aquellos en los que se rechaze normalidad, es decir, aquellos cuyo pvalor sea <0.05
## Primero eliminamos los NA que se han producido, es decir, aquellos que solo tienen una altura, aquellos con más de 5000 datos y aquellos que tiene poco rango de valores
resultadosNDiamD <- as.data.frame(resultadosNDiam)
resultadosNFDiam <- na.omit(resultadosNDiamD)
write.csv(resultadosNFDiam,'of_plot.dbh.normality.csv')

## Comprobamos a ver si hay algún valor que se distribuya normalmente, es decir, que su pvalor sea>0.05, de forma que no se rechaza la hipotesis nula
any(as.numeric(resultadosNFDiam$Pvalor) > 0.05) ## En este caso no hay ningún dato que se ditribuya normalmente

########################################################################
##  4.2.2 - Test == PLURIMODALIDAD
########################################################################

## Los datos con los que tendremos que trabajar serán con los mismos que en normalidad, es decir, no nos interesan aquellos que solo tengan un único diámetro
## También tendremos que eliminar el estadillo que se distribuye normalmente
arboles_filtradosDiam <- arbolesF[arbolesF$PlotID %in% resultadosNFDiam$PlotID, ]

posicionMD <- data.frame(arboles_filtradosDiam$PlotID)
posicionUMD <- unique(posicionMD)
tam4 <- dim(posicionUMD)[1]

#Generalizamos
resultadosMDiam <- matrix(NA,nrow=tam4,ncol=2)
colnames(resultadosM) <- c("Pvalor", ## "Origen", "Estadillo",
                           'PlotID')

StartTime <- Sys.time()              
for(i in 1:tam4){ 
  d <- arboles_filtradosDiam[arboles_filtradosDiam$PlotID == posicionUMD$arboles_filtradosDiam.PlotID[i], ]
  d <- d[c("dbh","expan")]
  ## print(posicionUMD$arboles_filtradosDiam.Origen[i])
  ## print(posicionUMD$arboles_filtradosDiam.Estadillo[i])
  ## print(posicionUMD$arboles_filtradosDiam.PlotID[i])
  x <- d$dbh
  w <- d$expan
  f <- c(rep(x,w))
  
  sm.density(f)
  
  resultadosMDiam[i,] <- c(unimodal.test(f,100),
                           ## posicionUMD$arboles_filtradosDiam.Origen[i],
                           ## posicionUMD$arboles_filtradosDiam.Estadillo[i],
                           posicionUMD$arboles_filtradosDiam.PlotID[i])  
  print(paste('Plot # ',i,
              ' of ', tam, ' plots. ',
              ' - - Plot Id: ', resultadosMDiam[i,2],
              ' - - PValue: ', resultadosMDiam[i,1] ) )
}
EndTime <- Sys.time()
print(paste('Time spend: start at ', StartTime, ' end at ', EndTime) )

## resultadosMDiam


resultadosMDiamF <- as.data.frame(resultadosMDiam)

colnames(resultadosMDiamF) <- c("Pvalor", ## "Origen", "Estadillo",
                                'PlotID')

write.csv(resultadosMDiamF,"of_resultDiamPlurimodal.csv")


## q()
## n



