#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                     Adapt plots already filtered to SIMANFOR                       ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 18/07/2023                              #
#                              Last modification: 18/07/2023                               #
#------------------------------------------------------------------------------------------#


#### Aim of the script ####

# That script is developed with the aim to use the 2nd, 3rd and 4rd Spanish National Forest
# Inventory (SFNI, IFN in spanish) to run simulations in SIMANFOR
# Resources:
# SFNI: https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/default.aspx
# SIMANFOR: https://www.simanfor.es/


#### Basic steps ####

# path
setwd('/media/aitor/Elements/aitor/iuFOR_trabajo/Proyectos/COMFOR_SUDOE/SIMANFOR_deliverable/')

# libraries
library(plyr)
library(dplyr)
library(stringr)
library(taRifx)
library(tidyverse)
library(raster)
library(oce) # utm2lonlat
library(sf)

# data

# PsylPnig data
trees_psylpnig_2 <- read.csv('data/0.raw/arboles.21.25.if2.csv')
trees_psylpnig_3 <- read.csv('data/0.raw/arboles.21.25.if3.csv')
trees_psylpnig_4 <- read.csv('data/0.raw/arboles.21.25.if4.csv')

plots_psylpnig_2 <- read.csv('data/0.raw/parcelas.21.25.if2.csv')
plots_psylpnig_3 <- read.csv('data/0.raw/parcelas.21.25.if3.csv')
plots_psylpnig_4 <- read.csv('data/0.raw/parcelas.21.25.if4.csv')

# PsylPpin data
trees_psylppin_2 <- read.csv('data/0.raw/arboles.21.26.if2.csv')
trees_psylppin_3 <- read.csv('data/0.raw/arboles.21.26.if3.csv')
trees_psylppin_4 <- read.csv('data/0.raw/arboles.21.26.if4.csv')

plots_psylppin_2 <- read.csv('data/0.raw/parcelas.21.26.if2.csv')
plots_psylppin_3 <- read.csv('data/0.raw/parcelas.21.26.if3.csv')
plots_psylppin_4 <- read.csv('data/0.raw/parcelas.21.26.if4.csv')

# PsylQpyr data
trees_psylqpyr_2 <- read.csv('data/0.raw/arboles.21.43.if2.csv')
trees_psylqpyr_3 <- read.csv('data/0.raw/arboles.21.43.if3.csv')
trees_psylqpyr_4 <- read.csv('data/0.raw/arboles.21.43.if4.csv')

plots_psylqpyr_2 <- read.csv('data/0.raw/parcelas.21.43.if2.csv')
plots_psylqpyr_3 <- read.csv('data/0.raw/parcelas.21.43.if3.csv')
plots_psylqpyr_4 <- read.csv('data/0.raw/parcelas.21.43.if4.csv')

# PsylFsyl data
trees_psylfsyl_2 <- read.csv('data/0.raw/arboles.21.71.if2.csv')
trees_psylfsyl_3 <- read.csv('data/0.raw/arboles.21.71.if3.csv')
trees_psylfsyl_4 <- read.csv('data/0.raw/arboles.21.71.if4.csv')

plots_psylfsyl_2 <- read.csv('data/0.raw/parcelas.21.71.if2.csv')
plots_psylfsyl_3 <- read.csv('data/0.raw/parcelas.21.71.if3.csv')
plots_psylfsyl_4 <- read.csv('data/0.raw/parcelas.21.71.if4.csv')

# Coordinates corrected
coord3 <- read.csv('data/0.raw/if3_coordLT.csv')


#### Manage IDs ####

### PsylPnig data

## Trees

# IDs IFN2
trees_psylpnig_2$INVENTORY_ID <- 'IFN2'
trees_psylpnig_2$PLOT_ID_short <- paste(trees_psylpnig_2$Provincia, trees_psylpnig_2$Estadillo, sep = '_')
trees_psylpnig_2$PLOT_ID <- paste(trees_psylpnig_2$Provincia, trees_psylpnig_2$Estadillo, sep = '_')
trees_psylpnig_2$TREE_ID_IFN2 <- paste(trees_psylpnig_2$PLOT_ID, trees_psylpnig_2$nArbol, sep = '_')
trees_psylpnig_2$TREE_ID <- paste(trees_psylpnig_2$INVENTORY_ID, trees_psylpnig_2$Provincia, trees_psylpnig_2$Estadillo, trees_psylpnig_2$nArbol, sep = '_')

# IDs IFN3
trees_psylpnig_3$INVENTORY_ID <- 'IFN3'
trees_psylpnig_3$PLOT_ID_short <- paste(trees_psylpnig_3$Provincia, trees_psylpnig_3$Estadillo, sep = '_')
trees_psylpnig_3$PLOT_ID <- paste(trees_psylpnig_3$Provincia, trees_psylpnig_3$Estadillo, trees_psylpnig_3$Cla, trees_psylpnig_3$Subclase, sep = '_')
trees_psylpnig_3$TREE_ID_IFN3 <- paste(trees_psylpnig_3$PLOT_ID, trees_psylpnig_3$nArbol, sep = '_')
trees_psylpnig_3$TREE_ID <- paste(trees_psylpnig_3$INVENTORY_ID, trees_psylpnig_3$PLOT_ID, trees_psylpnig_3$nArbol, sep = '_')

# IDs IFN4
trees_psylpnig_4$INVENTORY_ID <- 'IFN4'
trees_psylpnig_4$PLOT_ID_short <- paste(trees_psylpnig_4$Provincia, trees_psylpnig_4$Estadillo, sep = '_')
trees_psylpnig_4$PLOT_ID <- paste(trees_psylpnig_4$Provincia, trees_psylpnig_4$Estadillo, trees_psylpnig_4$Cla, trees_psylpnig_4$Subclase, sep = '_')
trees_psylpnig_4$TREE_ID_IFN4 <- paste(trees_psylpnig_4$PLOT_ID, trees_psylpnig_4$nArbol, sep = '_')
trees_psylpnig_4$TREE_ID <- paste(trees_psylpnig_4$INVENTORY_ID, trees_psylpnig_4$PLOT_ID, trees_psylpnig_4$nArbol, sep = '_')


## Plots

# IDs IFN2
plots_psylpnig_2$INVENTORY_ID <- 'IFN2'
plots_psylpnig_2$PLOT_ID_short <- paste(plots_psylpnig_2$Provincia, plots_psylpnig_2$Estadillo, sep = '_')
plots_psylpnig_2$PLOT_ID <- paste(plots_psylpnig_2$Provincia, plots_psylpnig_2$Estadillo, sep = '_')

# IDs IFN3
plots_psylpnig_3$INVENTORY_ID <- 'IFN3'
plots_psylpnig_3$PLOT_ID_short <- paste(plots_psylpnig_3$Provincia, plots_psylpnig_3$Estadillo, sep = '_')
plots_psylpnig_3$PLOT_ID <- paste(plots_psylpnig_3$Provincia, plots_psylpnig_3$Estadillo, plots_psylpnig_3$Cla, plots_psylpnig_3$Subclase, sep = '_')

# IDs IFN4
plots_psylpnig_4$INVENTORY_ID <- 'IFN4'
plots_psylpnig_4$PLOT_ID_short <- paste(plots_psylpnig_4$Provincia, plots_psylpnig_4$Estadillo, sep = '_')
plots_psylpnig_4$PLOT_ID <- paste(plots_psylpnig_4$Provincia, plots_psylpnig_4$Estadillo, plots_psylpnig_4$Cla, plots_psylpnig_4$Subclase, sep = '_')


### PsylPpin data

## Trees

# IDs IFN2
trees_psylppin_2$INVENTORY_ID <- 'IFN2'
trees_psylppin_2$PLOT_ID_short <- paste(trees_psylppin_2$Provincia, trees_psylppin_2$Estadillo, sep = '_')
trees_psylppin_2$PLOT_ID <- paste(trees_psylppin_2$Provincia, trees_psylppin_2$Estadillo, sep = '_')
trees_psylppin_2$TREE_ID_IFN2 <- paste(trees_psylppin_2$PLOT_ID, trees_psylppin_2$nArbol, sep = '_')
trees_psylppin_2$TREE_ID <- paste(trees_psylppin_2$INVENTORY_ID, trees_psylppin_2$Provincia, trees_psylppin_2$Estadillo, trees_psylppin_2$nArbol, sep = '_')

# IDs IFN3
trees_psylppin_3$INVENTORY_ID <- 'IFN3'
trees_psylppin_3$PLOT_ID_short <- paste(trees_psylppin_3$Provincia, trees_psylppin_3$Estadillo, sep = '_')
trees_psylppin_3$PLOT_ID <- paste(trees_psylppin_3$Provincia, trees_psylppin_3$Estadillo, trees_psylppin_3$Cla, trees_psylppin_3$Subclase, sep = '_')
trees_psylppin_3$TREE_ID_IFN3 <- paste(trees_psylppin_3$PLOT_ID, trees_psylppin_3$nArbol, sep = '_')
trees_psylppin_3$TREE_ID <- paste(trees_psylppin_3$INVENTORY_ID, trees_psylppin_3$PLOT_ID, trees_psylppin_3$nArbol, sep = '_')

# IDs IFN4
trees_psylppin_4$INVENTORY_ID <- 'IFN4'
trees_psylppin_4$PLOT_ID_short <- paste(trees_psylppin_4$Provincia, trees_psylppin_4$Estadillo, sep = '_')
trees_psylppin_4$PLOT_ID <- paste(trees_psylppin_4$Provincia, trees_psylppin_4$Estadillo, trees_psylppin_4$Cla, trees_psylppin_4$Subclase, sep = '_')
trees_psylppin_4$TREE_ID_IFN4 <- paste(trees_psylppin_4$PLOT_ID, trees_psylppin_4$nArbol, sep = '_')
trees_psylppin_4$TREE_ID <- paste(trees_psylppin_4$INVENTORY_ID, trees_psylppin_4$PLOT_ID, trees_psylppin_4$nArbol, sep = '_')


## Plots

# IDs IFN2
plots_psylppin_2$INVENTORY_ID <- 'IFN2'
plots_psylppin_2$PLOT_ID_short <- paste(plots_psylppin_2$Provincia, plots_psylppin_2$Estadillo, sep = '_')
plots_psylppin_2$PLOT_ID <- paste(plots_psylppin_2$Provincia, plots_psylppin_2$Estadillo, sep = '_')

# IDs IFN3
plots_psylppin_3$INVENTORY_ID <- 'IFN3'
plots_psylppin_3$PLOT_ID_short <- paste(plots_psylppin_3$Provincia, plots_psylppin_3$Estadillo, sep = '_')
plots_psylppin_3$PLOT_ID <- paste(plots_psylppin_3$Provincia, plots_psylppin_3$Estadillo, plots_psylppin_3$Cla, plots_psylppin_3$Subclase, sep = '_')

# IDs IFN4
plots_psylppin_4$INVENTORY_ID <- 'IFN4'
plots_psylppin_4$PLOT_ID_short <- paste(plots_psylppin_4$Provincia, plots_psylppin_4$Estadillo, sep = '_')
plots_psylppin_4$PLOT_ID <- paste(plots_psylppin_4$Provincia, plots_psylppin_4$Estadillo, plots_psylppin_4$Cla, plots_psylppin_4$Subclase, sep = '_')


### PsylQpyr data

## Trees

# IDs IFN2
trees_psylqpyr_2$INVENTORY_ID <- 'IFN2'
trees_psylqpyr_2$PLOT_ID_short <- paste(trees_psylqpyr_2$Provincia, trees_psylqpyr_2$Estadillo, sep = '_')
trees_psylqpyr_2$PLOT_ID <- paste(trees_psylqpyr_2$Provincia, trees_psylqpyr_2$Estadillo, sep = '_')
trees_psylqpyr_2$TREE_ID_IFN2 <- paste(trees_psylqpyr_2$PLOT_ID, trees_psylqpyr_2$nArbol, sep = '_')
trees_psylqpyr_2$TREE_ID <- paste(trees_psylqpyr_2$INVENTORY_ID, trees_psylqpyr_2$Provincia, trees_psylqpyr_2$Estadillo, trees_psylqpyr_2$nArbol, sep = '_')

# IDs IFN3
trees_psylqpyr_3$INVENTORY_ID <- 'IFN3'
trees_psylqpyr_3$PLOT_ID_short <- paste(trees_psylqpyr_3$Provincia, trees_psylqpyr_3$Estadillo, sep = '_')
trees_psylqpyr_3$PLOT_ID <- paste(trees_psylqpyr_3$Provincia, trees_psylqpyr_3$Estadillo, trees_psylqpyr_3$Cla, trees_psylqpyr_3$Subclase, sep = '_')
trees_psylqpyr_3$TREE_ID_IFN3 <- paste(trees_psylqpyr_3$PLOT_ID, trees_psylqpyr_3$nArbol, sep = '_')
trees_psylqpyr_3$TREE_ID <- paste(trees_psylqpyr_3$INVENTORY_ID, trees_psylqpyr_3$PLOT_ID, trees_psylqpyr_3$nArbol, sep = '_')

# IDs IFN4
trees_psylqpyr_4$INVENTORY_ID <- 'IFN4'
trees_psylqpyr_4$PLOT_ID_short <- paste(trees_psylqpyr_4$Provincia, trees_psylqpyr_4$Estadillo, sep = '_')
trees_psylqpyr_4$PLOT_ID <- paste(trees_psylqpyr_4$Provincia, trees_psylqpyr_4$Estadillo, trees_psylqpyr_4$Cla, trees_psylqpyr_4$Subclase, sep = '_')
trees_psylqpyr_4$TREE_ID_IFN4 <- paste(trees_psylqpyr_4$PLOT_ID, trees_psylqpyr_4$nArbol, sep = '_')
trees_psylqpyr_4$TREE_ID <- paste(trees_psylqpyr_4$INVENTORY_ID, trees_psylqpyr_4$PLOT_ID, trees_psylqpyr_4$nArbol, sep = '_')


## Plots

# IDs IFN2
plots_psylqpyr_2$INVENTORY_ID <- 'IFN2'
plots_psylqpyr_2$PLOT_ID_short <- paste(plots_psylqpyr_2$Provincia, plots_psylqpyr_2$Estadillo, sep = '_')
plots_psylqpyr_2$PLOT_ID <- paste(plots_psylqpyr_2$Provincia, plots_psylqpyr_2$Estadillo, sep = '_')

# IDs IFN3
plots_psylqpyr_3$INVENTORY_ID <- 'IFN3'
plots_psylqpyr_3$PLOT_ID_short <- paste(plots_psylqpyr_3$Provincia, plots_psylqpyr_3$Estadillo, sep = '_')
plots_psylqpyr_3$PLOT_ID <- paste(plots_psylqpyr_3$Provincia, plots_psylqpyr_3$Estadillo, plots_psylqpyr_3$Cla, plots_psylqpyr_3$Subclase, sep = '_')

# IDs IFN4
plots_psylqpyr_4$INVENTORY_ID <- 'IFN4'
plots_psylqpyr_4$PLOT_ID_short <- paste(plots_psylqpyr_4$Provincia, plots_psylqpyr_4$Estadillo, sep = '_')
plots_psylqpyr_4$PLOT_ID <- paste(plots_psylqpyr_4$Provincia, plots_psylqpyr_4$Estadillo, plots_psylqpyr_4$Cla, plots_psylqpyr_4$Subclase, sep = '_')


### PsylFsyl data

## Trees

# IDs IFN2
trees_psylfsyl_2$INVENTORY_ID <- 'IFN2'
trees_psylfsyl_2$PLOT_ID_short <- paste(trees_psylfsyl_2$Provincia, trees_psylfsyl_2$Estadillo, sep = '_')
trees_psylfsyl_2$PLOT_ID <- paste(trees_psylfsyl_2$Provincia, trees_psylfsyl_2$Estadillo, sep = '_')
trees_psylfsyl_2$TREE_ID_IFN2 <- paste(trees_psylfsyl_2$PLOT_ID, trees_psylfsyl_2$nArbol, sep = '_')
trees_psylfsyl_2$TREE_ID <- paste(trees_psylfsyl_2$INVENTORY_ID, trees_psylfsyl_2$Provincia, trees_psylfsyl_2$Estadillo, trees_psylfsyl_2$nArbol, sep = '_')

# IDs IFN3
trees_psylfsyl_3$INVENTORY_ID <- 'IFN3'
trees_psylfsyl_3$PLOT_ID_short <- paste(trees_psylfsyl_3$Provincia, trees_psylfsyl_3$Estadillo, sep = '_')
trees_psylfsyl_3$PLOT_ID <- paste(trees_psylfsyl_3$Provincia, trees_psylfsyl_3$Estadillo, trees_psylfsyl_3$Cla, trees_psylfsyl_3$Subclase, sep = '_')
trees_psylfsyl_3$TREE_ID_IFN3 <- paste(trees_psylfsyl_3$PLOT_ID, trees_psylfsyl_3$nArbol, sep = '_')
trees_psylfsyl_3$TREE_ID <- paste(trees_psylfsyl_3$INVENTORY_ID, trees_psylfsyl_3$PLOT_ID, trees_psylfsyl_3$nArbol, sep = '_')

# IDs IFN4
trees_psylfsyl_4$INVENTORY_ID <- 'IFN4'
trees_psylfsyl_4$PLOT_ID_short <- paste(trees_psylfsyl_4$Provincia, trees_psylfsyl_4$Estadillo, sep = '_')
trees_psylfsyl_4$PLOT_ID <- paste(trees_psylfsyl_4$Provincia, trees_psylfsyl_4$Estadillo, trees_psylfsyl_4$Cla, trees_psylfsyl_4$Subclase, sep = '_')
trees_psylfsyl_4$TREE_ID_IFN4 <- paste(trees_psylfsyl_4$PLOT_ID, trees_psylfsyl_4$nArbol, sep = '_')
trees_psylfsyl_4$TREE_ID <- paste(trees_psylfsyl_4$INVENTORY_ID, trees_psylfsyl_4$PLOT_ID, trees_psylfsyl_4$nArbol, sep = '_')


## Plots

# IDs IFN2
plots_psylfsyl_2$INVENTORY_ID <- 'IFN2'
plots_psylfsyl_2$PLOT_ID_short <- paste(plots_psylfsyl_2$Provincia, plots_psylfsyl_2$Estadillo, sep = '_')
plots_psylfsyl_2$PLOT_ID <- paste(plots_psylfsyl_2$Provincia, plots_psylfsyl_2$Estadillo, sep = '_')

# IDs IFN3
plots_psylfsyl_3$INVENTORY_ID <- 'IFN3'
plots_psylfsyl_3$PLOT_ID_short <- paste(plots_psylfsyl_3$Provincia, plots_psylfsyl_3$Estadillo, sep = '_')
plots_psylfsyl_3$PLOT_ID <- paste(plots_psylfsyl_3$Provincia, plots_psylfsyl_3$Estadillo, plots_psylfsyl_3$Cla, plots_psylfsyl_3$Subclase, sep = '_')

# IDs IFN4
plots_psylfsyl_4$INVENTORY_ID <- 'IFN4'
plots_psylfsyl_4$PLOT_ID_short <- paste(plots_psylfsyl_4$Provincia, plots_psylfsyl_4$Estadillo, sep = '_')
plots_psylfsyl_4$PLOT_ID <- paste(plots_psylfsyl_4$Provincia, plots_psylfsyl_4$Estadillo, plots_psylfsyl_4$Cla, plots_psylfsyl_4$Subclase, sep = '_')


#### Merge all trees and plots ####

# trees by IFN
trees2 <- rbind(trees_psylfsyl_2, trees_psylpnig_2, trees_psylppin_2, trees_psylqpyr_2)
trees3 <- rbind(trees_psylfsyl_3, trees_psylpnig_3, trees_psylppin_3, trees_psylqpyr_3)
trees4 <- rbind(trees_psylfsyl_4, trees_psylpnig_4, trees_psylppin_4, trees_psylqpyr_4)

# plots by IFN
plots2 <- rbind(plots_psylfsyl_2, plots_psylpnig_2, plots_psylppin_2, plots_psylqpyr_2)
plots3 <- rbind(plots_psylfsyl_3, plots_psylpnig_3, plots_psylppin_3, plots_psylqpyr_3)
plots4 <- rbind(plots_psylfsyl_4, plots_psylpnig_4, plots_psylppin_4, plots_psylqpyr_4)


#### Calculate tree variables ####

# just variables that will be needed on SIMANFOR are calculated

# check type of variables
str(trees2)
str(trees3)
str(trees4)

# convert height to numeric on IFN2
trees2 <- trees2 %>% 
  mutate(Altura = str_replace(Altura, ",", "."))

trees2$Altura <- destring(trees2$Altura)

# dbh (cm)
trees2$dbh <- (trees2$Diametro1 + trees2$Diametro2)/20
trees3$dbh <- (trees3$Diametro1 + trees3$Diametro2)/20
trees4$dbh <- (trees4$Diametro1 + trees4$Diametro2)/20

# expansion factor
trees2$expan <- with(trees2, 
                     ifelse (dbh < 7.5, 0, 
                             ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                    ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                           ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                  10000/(pi*(25^2)))))))
trees3$expan <- with(trees3, 
                     ifelse (dbh < 7.5, 0, 
                             ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                    ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                           ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                  10000/(pi*(25^2)))))))
trees4$expan <- with(trees4, 
                     ifelse (dbh < 7.5, 0, 
                             ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                    ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                           ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                                  10000/(pi*(25^2)))))))

# basal area (cm2)
trees2$g <- ((pi)/4)*(trees2$dbh^2)
trees3$g <- ((pi)/4)*(trees3$dbh^2)
trees4$g <- ((pi)/4)*(trees4$dbh^2)

# slenderness
trees2$slenderness <- trees2$Altura*100/trees2$dbh
trees3$slenderness <- trees3$Altura*100/trees3$dbh
trees4$slenderness <- trees4$Altura*100/trees4$dbh

# dead (1) or alive (0) tree - clean dead trees to avoid mistakes (height = 0)
trees2$dead <- ifelse(trees2$Calidad == 6, 1, 0)
trees2 <- trees2[trees2$dead == 0, ]

trees3$dead <- ifelse(trees3$Calidad == 6, 1, 0)
trees3 <- trees3[trees3$dead == 0, ]

trees4$dead <- ifelse(trees4$Calidad == 6, 1, 0)
trees4 <- trees4[trees4$dead == 0, ]

# delete trees without IFN3 data (dead, harvested...)
trees2 <- trees2[!is.na(trees2$dbh), ]
trees2 <- trees2[trees2$dbh != 0, ]

trees3 <- trees3[!is.na(trees3$dbh), ]
trees3 <- trees3[trees3$dbh != 0, ]

trees4 <- trees4[!is.na(trees4$dbh), ]
trees4 <- trees4[trees4$dbh != 0, ]

# check variable types
str(trees2)
str(trees3)
str(trees4)


#### Calculate plot variables ####

# just variables that will be needed on SIMANFOR are calculated

# variables from trees
plots2_vars <- plyr::ddply(trees2, c('PLOT_ID'), summarise,
                           SUM_DBH = sum(dbh*expan, na.rm = TRUE),
                           SUM_H = sum(Altura*expan, na.rm = TRUE),
                           G = sum(g*expan/10000, na.rm = TRUE),
                           N = sum(expan, na.rm = TRUE),
                           Deadwood = sum(ifelse(1 %in% dead, 1, 0))
)

plots2_vars$DBHm <- plots2_vars$SUM_DBH/plots2_vars$N
plots2_vars$Hm <- plots2_vars$SUM_H/plots2_vars$N
plots2_vars$Dg <- with(plots2_vars, 200*(G/N/pi)^0.5, na.rm=TRUE)

plots3_vars <- plyr::ddply(trees3, c('PLOT_ID'), summarise,
                           SUM_DBH = sum(dbh*expan, na.rm = TRUE),
                           SUM_H = sum(Altura*expan, na.rm = TRUE),
                           G = sum(g*expan/10000, na.rm = TRUE),
                           N = sum(expan, na.rm = TRUE),
                           Deadwood = sum(ifelse(1 %in% dead, 1, 0))
)

plots3_vars$DBHm <- plots3_vars$SUM_DBH/plots3_vars$N
plots3_vars$Hm <- plots3_vars$SUM_H/plots3_vars$N
plots3_vars$Dg <- with(plots3_vars, 200*(G/N/pi)^0.5, na.rm=TRUE)

plots4_vars <- plyr::ddply(trees4, c('PLOT_ID'), summarise,
                           SUM_DBH = sum(dbh*expan, na.rm = TRUE),
                           SUM_H = sum(Altura*expan, na.rm = TRUE),
                           G = sum(g*expan/10000, na.rm = TRUE),
                           N = sum(expan, na.rm = TRUE),
                           Deadwood = sum(ifelse(1 %in% dead, 1, 0))
)

plots4_vars$DBHm <- plots4_vars$SUM_DBH/plots4_vars$N
plots4_vars$Hm <- plots4_vars$SUM_H/plots4_vars$N
plots4_vars$Dg <- with(plots4_vars, 200*(G/N/pi)^0.5, na.rm=TRUE)

# functions fo Ho and Do
source('../../IFN/IFN_functions.R')

# calculate Ho and join data
trees2$h <- trees2$Altura  # tmp
trees3$h <- trees3$Altura  # tmp
trees4$h <- trees4$Altura  # tmp

Ho_2 <- dominantHeight(trees2, 'PLOT_ID')
plots2_vars <- merge(plots2_vars, Ho_2, by = 'PLOT_ID')

Ho_3 <- dominantHeight(trees3, 'PLOT_ID')
plots3_vars <- merge(plots3_vars, Ho_3, by = 'PLOT_ID')

Ho_4 <- dominantHeight(trees4, 'PLOT_ID')
plots4_vars <- merge(plots4_vars, Ho_4, by = 'PLOT_ID')

trees2 <- dplyr::select(trees2, -h)  # tmp
trees3 <- dplyr::select(trees3, -h)  # tmp
trees4 <- dplyr::select(trees4, -h)  # tmp

# calculate Do and join datasets
Do_2 <- DiametroDominante(trees2, 'PLOT_ID')
plots2_vars <- merge(Do_2, plots2_vars, all = TRUE, by.x = 'IDs', by.y = 'PLOT_ID')

Do_3 <- DiametroDominante(trees3, 'PLOT_ID')
plots3_vars <- merge(Do_3, plots3_vars, all = TRUE, by.x = 'IDs', by.y = 'PLOT_ID')

Do_4 <- DiametroDominante(trees4, 'PLOT_ID')
plots4_vars <- merge(Do_4, plots4_vars, all = TRUE, by.x = 'IDs', by.y = 'PLOT_ID')

# join new data to the original 
plots2 <- merge(plots2, plots2_vars, all = TRUE, by.x = 'PLOT_ID', by.y = 'IDs')
plots3 <- merge(plots3, plots3_vars, all = TRUE, by.x = 'PLOT_ID', by.y = 'IDs')
plots4 <- merge(plots4, plots4_vars, all = TRUE, by.x = 'PLOT_ID', by.y = 'IDs')

# clean NAs
#plots4 <- plots4[!is.na(plots4$AGE), ]
#plots4 <- plots4[!is.na(plots4$N), ]
#plots4 <- plots4[!is.na(plots4$X_UTM), ]

# slenderness
plots2$slenderness <- plots2$Hm*100/plots2$DBHm  # esbeltez normal
plots2$dominant_slenderness <- plots2$Ho*100/plots2$Do  # esbeltez dominante

plots3$slenderness <- plots3$Hm*100/plots3$DBHm  # esbeltez normal
plots3$dominant_slenderness <- plots3$Ho*100/plots3$Do  # esbeltez dominante

plots4$slenderness <- plots4$Hm*100/plots4$DBHm  # esbeltez normal
plots4$dominant_slenderness <- plots4$Ho*100/plots4$Do  # esbeltez dominante

# SDI
valor_r <- -1.605 # valor editable dependiendo de la especie (consultar bibliografía)

plots2$SDI <- plots2$N*((25/plots2$Dg)**valor_r)
plots3$SDI <- plots3$N*((25/plots3$Dg)**valor_r)
plots4$SDI <- plots4$N*((25/plots4$Dg)**valor_r)

# Cálculo del Índice de Hart (S)
# Índice de Hart-Becking
plots2$S <- 10000/(plots2$Ho*sqrt(plots2$N)) 
plots3$S <- 10000/(plots3$Ho*sqrt(plots3$N)) 
plots4$S <- 10000/(plots4$Ho*sqrt(plots4$N))  
# Índice de Hart-Becking para masas al tresbolillo
plots2$S_staggered <- (10000/plots2$Ho)*sqrt(2/plots2$N*sqrt(3))  
plots3$S_staggered <- (10000/plots3$Ho)*sqrt(2/plots3$N*sqrt(3))  
plots4$S_staggered <- (10000/plots4$Ho)*sqrt(2/plots4$N*sqrt(3))  

# match trees and plots
#trees4 <- trees4[trees4$PLOT_ID %in% plots4$PLOT_ID, ]

# clean environment
rm(dominantHeight, DiametroDominante, valor_r, plots2_vars, Ho_2, Do_2, 
   plots3_vars, Ho_3, Do_3, plots4_vars, Ho_4, Do_4)


#### Variables by species ####

# We calculate variables by species to set the main stand species by G criteria

# Calculate G and N by species
plots2_vars_sp <- ddply(trees2, c('PLOT_ID', 'Especie'), summarise, 
                        G_sp = sum(g*expan/10000, na.rm = TRUE), 
                        N_sp = sum(expan, na.rm = TRUE)
)
plots3_vars_sp <- ddply(trees3, c('PLOT_ID', 'Especie'), summarise, 
                         G_sp = sum(g*expan/10000, na.rm = TRUE), 
                         N_sp = sum(expan, na.rm = TRUE)
)
plots4_vars_sp <- ddply(trees4, c('PLOT_ID', 'Especie'), summarise, 
                        G_sp = sum(g*expan/10000, na.rm = TRUE), 
                        N_sp = sum(expan, na.rm = TRUE)
)

# organize information by PLOT_ID and G
plots2_vars_sp <- plots2_vars_sp %>%
  arrange(PLOT_ID, -G_sp)
plots3_vars_sp <- plots3_vars_sp %>%
  arrange(PLOT_ID, -G_sp)
plots4_vars_sp <- plots4_vars_sp %>%
  arrange(PLOT_ID, -G_sp)

# N and G by species
plots_useful_IFN2 <- data.frame()
for (k in unique(plots2_vars_sp$PLOT_ID)){
  
  plots_k <- plots2_vars_sp[plots2_vars_sp$PLOT_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$Especie[1]
  plots_k$sp_2 <- plots_k$Especie[2] 
  plots_k$sp_3 <- plots_k$Especie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN2 <- rbind(plots_useful_IFN2, plots_k)
}

plots_useful_IFN3 <- data.frame()
for (k in unique(plots3_vars_sp$PLOT_ID)){
  
  plots_k <- plots3_vars_sp[plots3_vars_sp$PLOT_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$Especie[1]
  plots_k$sp_2 <- plots_k$Especie[2] 
  plots_k$sp_3 <- plots_k$Especie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN3 <- rbind(plots_useful_IFN3, plots_k)
}

plots_useful_IFN4 <- data.frame()
for (k in unique(plots4_vars_sp$PLOT_ID)){
  
  plots_k <- plots4_vars_sp[plots4_vars_sp$PLOT_ID %in% k,]
  
  plots_k$sp_1 <- plots_k$Especie[1]
  plots_k$sp_2 <- plots_k$Especie[2] 
  plots_k$sp_3 <- plots_k$Especie[3] 
  plots_k$G_sp_1 <- plots_k$G_sp[1]
  plots_k$G_sp_2 <- plots_k$G_sp[2]
  plots_k$G_sp_3 <- plots_k$G_sp[3]
  plots_k$N_sp_1 <- plots_k$N_sp[1]
  plots_k$N_sp_2 <- plots_k$N_sp[2]
  plots_k$N_sp_3 <- plots_k$N_sp[3]
  
  plots_useful_IFN4 <- rbind(plots_useful_IFN4, plots_k)
}

# delete duplicated data
plots_useful_IFN2 <- plots_useful_IFN2[!duplicated(plots_useful_IFN2$PLOT_ID), ]
plots_useful_IFN2 <- dplyr::select(plots_useful_IFN2, -c(Especie, N_sp, G_sp))

plots_useful_IFN3 <- plots_useful_IFN3[!duplicated(plots_useful_IFN3$PLOT_ID), ]
plots_useful_IFN3 <- dplyr::select(plots_useful_IFN3, -c(Especie, N_sp, G_sp))

plots_useful_IFN4 <- plots_useful_IFN4[!duplicated(plots_useful_IFN4$PLOT_ID), ]
plots_useful_IFN4 <- dplyr::select(plots_useful_IFN4, -c(Especie, N_sp, G_sp))

# merge information with original data
plots2 <- merge(plots_useful_IFN2, plots2, all = TRUE, by = 'PLOT_ID')
plots3 <- merge(plots_useful_IFN3, plots3, all = TRUE, by = 'PLOT_ID')
plots4 <- merge(plots_useful_IFN4, plots4, all = TRUE, by = 'PLOT_ID')

# clean environment
rm(plots_k, plots_useful_IFN2, plots_useful_IFN3, plots_useful_IFN4, 
   plots2_vars_sp, plots3_vars_sp, plots4_vars_sp, k)


#### Checkpoint ####

save.image('data/1.checkpoints/tree_and_plot_data.RData')
load('data/1.checkpoints/tree_and_plot_data.RData')


#### Get coordinates of each plot on the correct form ####

## before doing that, we should check the CRS of each plot...
# resource: https://search.r-project.org/CRAN/refmans/oce/html/utm2lonlat.html

# in that case, Cristóbal already changed the CRS -> coord3

# in that case, we just copy the coordinates from the coord3 file
# but first, I will manage this df
coord3 <- dplyr::rename(coord3, 
                        Longitude = CoorX.1,
                        Latitude = CoorY.1)

coord3 <- dplyr::select(coord3, -c(X, Provincia, Origen, optional, CoorX, 
                                   CoorY, huso))

# now, I join coordinates and
plots2 <- merge(plots2, coord3, by = 'PlotID0')
plots3 <- merge(plots3, coord3, by = 'PlotID0')
plots4 <- merge(plots4, coord3, by = 'PlotID0')

# and remove duplicated
plots2 <- plots2[!duplicated(plots2$PLOT_ID), ]
plots3 <- plots3[!duplicated(plots3$PLOT_ID), ]
plots4 <- plots4[!duplicated(plots4$PLOT_ID), ]

# preserve original data
points2 <- plots2
points3 <- plots3
points4 <- plots4

# create ID unique code (duplicate it) to use it on weather data
points2$ID <- points2$PLOT_ID
points3$ID <- points3$PLOT_ID
points4$ID <- points4$PLOT_ID

# calculate longitude and latitude
# lonlat <- utm2lonlat(easting = points$X_UTM,
#                      northing = points$Y_UTM,
#                      zone = points$Huso, 
#                      hemisphere = 'N',
#                      km = FALSE)

# add them to the previous dataset
# points$Longitude <- tibble(lonlat$longitude)
# points$Latitude <- tibble(lonlat$latitude)

# create a SpatialPointsDataFrame1
coordinates(points2) <- c("Longitude", "Latitude")
proj4string(points2) <- CRS("+proj=longlat +datum=WGS84")

coordinates(points3) <- c("Longitude", "Latitude")
proj4string(points3) <- CRS("+proj=longlat +datum=WGS84")

coordinates(points4) <- c("Longitude", "Latitude")
proj4string(points4) <- CRS("+proj=longlat +datum=WGS84")



#### Calculate variables related with climate ####

# 1. run function to get historical climate data
source('../../WorldClim/scripts/wc_historic_monthly_data.R')

# my periods of time
period2 <- c(1986:1996)
period3 <- c(1997:2007)
period4 <- c(2008:2023)

# run functions for month, year and period historical data
folder_path <- "../../WorldClim/historical_monthly_data/"

# get monthly data
wc_month2 <- wc_monthly_climate(wc_path = folder_path,
                               points = points2, 
                               data_period = period2)

wc_month3 <- wc_monthly_climate(wc_path = folder_path,
                               points = points3, 
                               data_period = period3)

wc_month4 <- wc_monthly_climate(wc_path = folder_path,
                               points = points4, 
                               data_period = period4)

# get yearly data
wc_year2 <- wc_annual_climate(df = wc_month2)
wc_year3 <- wc_annual_climate(df = wc_month3)
wc_year4 <- wc_annual_climate(df = wc_month4)

# get mean by period
wc_period2 <- wc_average_climate(df = wc_year2)
wc_period3 <- wc_average_climate(df = wc_year3)
wc_period4 <- wc_average_climate(df = wc_year4)


# 2. future climate data is not needed here


# 3. run function to estimate Martonne Aridity index
source('../../WorldClim/scripts/wc_climate_index.R')

# calculate martonne for the period of historical data
m_hist_period2 <- wc_martonne(df = wc_period2, 
                             prec = wc_period2$prec, 
                             tmean = wc_period2$tmean)

m_hist_period3 <- wc_martonne(df = wc_period3, 
                             prec = wc_period3$prec, 
                             tmean = wc_period3$tmean)

m_hist_period4 <- wc_martonne(df = wc_period4, 
                             prec = wc_period4$prec, 
                             tmean = wc_period4$tmean)

# merge data with the original one
plots2 <- merge(plots2, m_hist_period2, by.x = 'PLOT_ID', by.y = 'ID')
plots3 <- merge(plots3, m_hist_period3, by.x = 'PLOT_ID', by.y = 'ID')
plots4 <- merge(plots4, m_hist_period4, by.x = 'PLOT_ID', by.y = 'ID')

rm(m_hist_period2, m_hist_period3, m_hist_period4,
   points2, points3, points4, wc_month2, wc_month3, wc_month4, 
   wc_period2, wc_period3, wc_period4, wc_year2, wc_year3, wc_year4, 
   folder_path, period2, period3, period4, 
   m_per_periods, wc_annual_climate, wc_average_climate, 
   wc_martonne, wc_monthly_climate)


#### Checkpoint ####

save.image('data/1.checkpoints/after_climate_data.RData')
load('data/1.checkpoints/after_climate_data.RData')


#### Adapt names to SIMANFOR ####

# IFN2
trees2 <- rename(trees2, c(
  "species" = "Especie", 
  'dbh_1' = 'Diametro1',
  'dbh_2' = 'Diametro2',
  "bearing" = "Rumbo", 
  "distance" = "Distancia",  
  "height" = "Altura")
)

plots2 <- rename(plots2, c(
  "ID_SP1" = "sp_1",
  "ID_SP2" = "sp_2",
  "ID_SP3" = "sp_3",
  "dbh_mean" = "DBHm",
  "dg" = "Dg",
  "h_mean" = "Hm",
  'YEAR' = 'ANO',
  'MARTONNE' = 'M')
)

# IFN3
trees3 <- rename(trees3, c(
  "species" = "Especie", 
  'dbh_1' = 'Diametro1',
  'dbh_2' = 'Diametro2',
  "bearing" = "Rumbo", 
  "distance" = "Distancia",  
  "height" = "Altura")
)

plots3 <- rename(plots3, c(
  "ID_SP1" = "sp_1",
  "ID_SP2" = "sp_2",
  "ID_SP3" = "sp_3",
  "dbh_mean" = "DBHm",
  "dg" = "Dg",
  "h_mean" = "Hm",
  'YEAR' = 'Ano',
  'MARTONNE' = 'M')
)

# IFN4
trees4 <- rename(trees4, c(
  "species" = "Especie", 
  'dbh_1' = 'Diametro1',
  'dbh_2' = 'Diametro2',
  "bearing" = "Rumbo", 
  "distance" = "Distancia",  
  "height" = "Altura")
)

plots4 <- rename(plots4, c(
  "ID_SP1" = "sp_1",
  "ID_SP2" = "sp_2",
  "ID_SP3" = "sp_3",
  "dbh_mean" = "DBHm",
  "dg" = "Dg",
  "h_mean" = "Hm",
  'YEAR' = 'Ano',
  'MARTONNE' = 'M')
)


#### Filter species combinations ####

## select plots and trees on the filtered plots by Cristóbal

# IFN2
plots2_psylpnig <- plots2[plots2$PLOT_ID %in% plots_psylpnig_2$PLOT_ID, ]
trees2_psylpnig <- trees2[trees2$PLOT_ID %in% trees_psylpnig_2$PLOT_ID, ]

plots2_psylppin <- plots2[plots2$PLOT_ID %in% plots_psylppin_2$PLOT_ID, ]
trees2_psylppin <- trees2[trees2$PLOT_ID %in% trees_psylppin_2$PLOT_ID, ]

plots2_psylqpyr <- plots2[plots2$PLOT_ID %in% plots_psylqpyr_2$PLOT_ID, ]
trees2_psylqpyr <- trees2[trees2$PLOT_ID %in% trees_psylqpyr_2$PLOT_ID, ]

plots2_psylfsyl <- plots2[plots2$PLOT_ID %in% plots_psylfsyl_2$PLOT_ID, ]
trees2_psylfsyl <- trees2[trees2$PLOT_ID %in% trees_psylfsyl_2$PLOT_ID, ]

# IFN3
plots3_psylpnig <- plots3[plots3$PLOT_ID %in% plots_psylpnig_3$PLOT_ID, ]
trees3_psylpnig <- trees3[trees3$PLOT_ID %in% trees_psylpnig_3$PLOT_ID, ]

plots3_psylppin <- plots3[plots3$PLOT_ID %in% plots_psylppin_3$PLOT_ID, ]
trees3_psylppin <- trees3[trees3$PLOT_ID %in% trees_psylppin_3$PLOT_ID, ]

plots3_psylqpyr <- plots3[plots3$PLOT_ID %in% plots_psylqpyr_3$PLOT_ID, ]
trees3_psylqpyr <- trees3[trees3$PLOT_ID %in% trees_psylqpyr_3$PLOT_ID, ]

plots3_psylfsyl <- plots3[plots3$PLOT_ID %in% plots_psylfsyl_3$PLOT_ID, ]
trees3_psylfsyl <- trees3[trees3$PLOT_ID %in% trees_psylfsyl_3$PLOT_ID, ]

# IFN4
plots4_psylpnig <- plots4[plots4$PLOT_ID %in% plots_psylpnig_4$PLOT_ID, ]
trees4_psylpnig <- trees4[trees4$PLOT_ID %in% trees_psylpnig_4$PLOT_ID, ]

plots4_psylppin <- plots4[plots4$PLOT_ID %in% plots_psylppin_4$PLOT_ID, ]
trees4_psylppin <- trees4[trees4$PLOT_ID %in% trees_psylppin_4$PLOT_ID, ]

plots4_psylqpyr <- plots4[plots4$PLOT_ID %in% plots_psylqpyr_4$PLOT_ID, ]
trees4_psylqpyr <- trees4[trees4$PLOT_ID %in% trees_psylqpyr_4$PLOT_ID, ]

plots4_psylfsyl <- plots4[plots4$PLOT_ID %in% plots_psylfsyl_4$PLOT_ID, ]
trees4_psylfsyl <- trees4[trees4$PLOT_ID %in% trees_psylfsyl_4$PLOT_ID, ]


#### Save results ####

# IFN2
write.csv(trees2, 'data/2.to_SIMANFOR/all/CS_IFN2_all_mixtures_trees.csv')
write.csv(trees2_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN2_trees_psylfsyl.csv')
write.csv(trees2_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN2_trees_psylpnig.csv')
write.csv(trees2_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN2_trees_psylppin.csv')
write.csv(trees2_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN2_trees_psylqpyr.csv')

write.csv(plots2, 'data/2.to_SIMANFOR/all/CS_IFN2_all_mixtures_plots.csv')
write.csv(plots2_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN2_plots_psylfsyl.csv')
write.csv(plots2_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN2_plots_psylpnig.csv')
write.csv(plots2_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN2_plots_psylppin.csv')
write.csv(plots2_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN2_plots_psylqpyr.csv')

# IFN3
write.csv(trees3, 'data/2.to_SIMANFOR/all/CS_IFN3_all_mixtures_trees.csv')
write.csv(trees3_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN3_trees_psylfsyl.csv')
write.csv(trees3_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN3_trees_psylpnig.csv')
write.csv(trees3_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN3_trees_psylppin.csv')
write.csv(trees3_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN3_trees_psylqpyr.csv')

write.csv(plots3, 'data/2.to_SIMANFOR/all/CS_IFN3_all_mixtures_plots.csv')
write.csv(plots3_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN3_plots_psylfsyl.csv')
write.csv(plots3_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN3_plots_psylpnig.csv')
write.csv(plots3_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN3_plots_psylppin.csv')
write.csv(plots3_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN3_plots_psylqpyr.csv')

# IFN4
write.csv(trees4, 'data/2.to_SIMANFOR/all/CS_IFN4_all_mixtures_trees.csv')
write.csv(trees4_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN4_trees_psylfsyl.csv')
write.csv(trees4_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN4_trees_psylpnig.csv')
write.csv(trees4_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN4_trees_psylppin.csv')
write.csv(trees4_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN4_trees_psylqpyr.csv')

write.csv(plots4, 'data/2.to_SIMANFOR/all/CS_IFN4_all_mixtures_plots.csv')
write.csv(plots4_psylfsyl, 'data/2.to_SIMANFOR/PsylFsyl/CS_IFN4_plots_psylfsyl.csv')
write.csv(plots4_psylpnig, 'data/2.to_SIMANFOR/PsylPnig/CS_IFN4_plots_psylpnig.csv')
write.csv(plots4_psylppin, 'data/2.to_SIMANFOR/PsylPpin/CS_IFN4_plots_psylppin.csv')
write.csv(plots4_psylqpyr, 'data/2.to_SIMANFOR/PsylQpyr/CS_IFN4_plots_psylqpyr.csv')

# project
save.image('data/1.checkpoints/final_project.RData')
