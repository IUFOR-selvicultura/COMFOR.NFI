# COMFOR SUDOE Project

---

<left>
<img src="./logos/COMFOR_LOGO_ERDF.jpg" width="466"/>
<img src="./logos/iuFOR.png" width="330"/>
<img src="./logos/uva.png" width="129"/>
<left>

funded by EU program.

---

This repository has been build to share and store the script prepared along COMFOR SUDOE Project in the framework of Work Pakage 1 (Análisis, caracterización y clasificación de los bosques
complejos del SUDOE) to analyse National Forest Inventory data and distinguis complex forest from non clomplex stands, i.e. plots which holds pluriespecific composition or with plurimodal composition in terms of total tree heigth and diameter at breast height.

The repository have 3 different folders containing scripts:

1. Scripts for preparing and loading data from Spanish National Forest Inventories
2. Scripts for selecting and calculating which plots belongs to each class
3. Scripts to make graphs and maps with the outputs
4. Scripts to build growth model for complex forest

---

## [Prepare Spanish NFI data into RData format files and load into R environment](./scripts4PrepareIFNdata/readme.md)

This folder include scripts used to prepare RData files of released editions of NFI, and to load them into an R environment.
The link to download RData files is also available.

---

## [Calculus for discriminate complex forest plots](./scripts4calculus/readme.md)

- NFICalcFinal.r

This file read tree data from Spanish NFI database and select plots with multispecific composition and plots with multiestratified composition at height and dbh levels

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

---


## [Maps and graphs of NFI plots by clases of complexity](./scripts4Report/readme.md)

This folder holds scripts aiming to make graphical representation of all calculus made previously, including maps at diferent levels and plots of evolution along different editions of NFI.

- NFIComplexForMap.r


---

## [build growth model for complex forest](./scripts4GrowthModel/readme.md)

With this scripts, a model growth of mixed plots including Chestnut and Oak has been prepared as an example.


---


[![COMFOR](./logos/COMFOR_LOGO_ERDF.jpg)](https://www.comfor-sudoe.eu/)
[![iuFOR](./logos/iuFOR.png)](https://iufor.uva.es)
[![UVa](./logos/uva.png)](https://uva.es)
