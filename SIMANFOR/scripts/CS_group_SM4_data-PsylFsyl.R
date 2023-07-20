#------------------------------------------------------------------------------------------#
####                       Groupe SIMANFOR results on a single df                       ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/03/2022                              #
#                              Last modification: 20/07/2023                               #
#------------------------------------------------------------------------------------------#



#### Summary ####

# Extended explanation here: 
# https://github.com/simanfor/resultados/blob/main/analisis_resultados/analisis_resultados_SIMANFOR.R


#### Basic steps ####

# libraries
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# set directory
general_dir <- "/media/aitor/Elements1/aitor/iuFOR_trabajo/Repositorios/LINUX/simanfor-dask/simulator/output/COMFOR_SUDOE/PsylFsyl/"
setwd(general_dir)


#### Read SIMANFOR outputs (just plot information) ####

plots <- tibble()  # will contain plot data
directory <- list.dirs(path = ".")  # will contain folder names

# for each subfolder...
for (folder in directory){ 
  
  # each subfolder is stablished as main one
  specific_dir <- paste(general_dir, "substract", folder, sep = "")
  specific_dir <- gsub("substract.", "", specific_dir)
  setwd(specific_dir)
  
  # extract .xlsx files names
  files_list <- list.files(specific_dir, pattern="xlsx")
  
  # for each file...
  for (doc in files_list){
    
    # read plot data
    plot_data <- read_excel(doc, sheet = "Plots")
    
    # create a new column with its name                
    plot_data$File_name <- doc  
    
    # add information to plot df
    ifelse(length(plots) == 0, plots <- rbind(plot_data), plots <- rbind(plots, plot_data))
  }
}


#### Save results ####

write.csv(plots, '/media/aitor/Elements1/aitor/iuFOR_trabajo/Proyectos/COMFOR_SUDOE/SIMANFOR_deliverable/results/all_results_PsylFsyl.csv')
