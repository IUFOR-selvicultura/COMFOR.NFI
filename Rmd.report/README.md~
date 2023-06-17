# report.go_sigca

R-Markdown report for "evaluación economica de plantaciones de Pinus pinaster en el Grupo Operativo SIGCA"

This repository has been made with rmarkdown, which is able to mix markdown and R, the first one for writing the report (with latex for generating pdf, or pandoc for many other output format files such html, word,...), and the later for executing R code and generate graph and show data. 

## Data used for economic analysis
Data are stored in folder simulations. Data came from a simulation based on stand growth model for *Pinus pinaster* and every silvicultural scenario is stored in sub-folders named M2, M4, M8 and MG2, and contain a vast amount of files by variables and Site Index.

For gathering all this files an script in linux is available, simul2eval.sh. This file store all simulations into "joint" subfolder in seven files: AB.csv, Dm.csv, Dq.csv, Hdom.csv, Itinerario.csv, VC.csvand VolAprov.csv

This files are the source for R and Rmd analysis.

## Packages and software

RStudio helps with R Markdown, allow menu shorcut to generate final output

Anycase you can use your prefered R front-end editor to generate your report. 

### libraries
Following libraries are needed:
* rmarkdown
* knitr
* pander

And some other are recommended:
* bookdown
* 

### File
Your report or book should be writen in an .Rdm file or in a colection of them
You can include configuration files in .css format () or .tex (latex)

### Rendering procces
The most important part is to render your file in order to get the output file, pdf or html or whatever...
With RStudio you can get output with menu
With all R front-end you can use "render" function from package "rmarkdown". For our report

```r
library(rmarkdown)
render( "report.4.Rmd", output_format = "pdf_document", output_file='./output/report.eval.6.pdf' )
```
Provided you store previous lines in a file, i.e. report.r you can run from command line:

```
R CMD BATCH report.r
```


## You can get help from lot of web-sites and online-books

### R Markdown

R Markdown Cookbook https://bookdown.org/yihui/rmarkdown-cookbook/
Yihui Xie, Christophe Dervieux, Emily Riederer
2020-08-21

bookdown: Authoring Books and Technical Documents with R Markdown https://bookdown.org/yihui/bookdown/
Yihui Xie
2020-08-17

papaja: Reproducible APA manuscripts with R Markdown https://crsh.github.io/papaja_man/
Frederik Aust & Marius Barth
2020-07-13

R Markdown Websites https://garrettgman.github.io/rmarkdown/rmarkdown_websites.html

### reports

Reports and academic writing: https://david-broska.netlify.app/post/academic-writing-with-rmarkdown/

Generación de informes con R: https://rubenfcasal.github.io/post/generaci%C3%B3n-de-informes-con-r/

Multi-reports: https://es.stackoverflow.com/questions/345279/se-puede-generar-m%C3%BAltiples-informes-en-rmarkdown

#### cross-reference images and tables... 
https://stackoverflow.com/questions/37116632/r-markdown-html-number-figures

### bookdown, a package to write books

BOOKDOWN https://www.bookdown.org/
Write HTML, PDF, ePub, and Kindle books with R Markdown




