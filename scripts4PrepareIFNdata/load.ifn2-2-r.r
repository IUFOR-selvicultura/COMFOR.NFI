## Script para cargar las bases de datos del IFN2

rm(list=ls())

getwd()
dir()
ls()
load(file='../../../NFI-data-raw/if2.RData')

ls()
str(trees.if2)
str(plots.if2)
str(tipos.if2)

