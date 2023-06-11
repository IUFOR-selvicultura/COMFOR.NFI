# COMFOR SUDOE Project
<right>
<img src="./logos/COMFOR_LOGO_ERDF.jpg" width="400"/>
Format: ![Comfor-sudoe EU project](https://www.comfor-sudoe.eu/)
</right>
<left>
<img src="./logos/iuFOR.png" width="210"/>
<img src="./logos/uva.png" width="75"/>
</left>

funded by EU program.
---

This repository has been build to share and store the script prepared along COMFOR SUDOE Project in the framework of WP 1 () to analyse National Forest Inventory data and distinguis complex forest from non clomplex stands, i.e. plots which holds pluriespecific composition or with plurimodal composition in terms of total tree heigth and diameter at breast height.

## Build NFI data into RData format files

## Load of NFI databases

## Calculus for discriminate complex forest plots

### scripts

- NFICalcFinal.r
This file read tree data from Spanish NFI database and select plots with multispecific composition and plots with multiestratified composition at height and dbh levels

- NFIComplexForMap.r
This file use calculus from previous file to analyze and plot map of complex forest in Spain with NFI database




### input files:

- if_pcmayores2.csv
tree database file

- if_plot2.csv
plot dadabase file

- if_plot.coord.if3.csv
plot position information





### output files:

- of_plotsMonoSP.csv
list of plots with monospecific composition

- of_plotsPluriSP.csv, of_plotsPluriSP_sps.csv
list of plots with monospecific composition, the second with tph and basal area by species

- of_plot.ht.normality.csv, of_plot.dbh.normality.csv
result of normality test (Shapiro–Wilk) for diameter and height distributions

- of_resultHeightPlurimodal.csv
result of multimodality test for non-normal distributions for height

- of_resultDiamPlurimodal.csv
result of multimodality test for non-normal distributions for diameter  


## Maps of NFI plots by clases of complexity
