## Script para cargar bases de datos del IFN4

rm(list=ls())

getwd()
dir()
ls()
load(file='../if4.RData')

ls()
str(trees.if4); str(plots.if4); str(maps.if4); str(plots.sp.if4)
