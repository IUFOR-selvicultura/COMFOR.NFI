library(rmarkdown)
## library(pander); library(ggplot2); library(reshape2); library(ggforce); library(dplyr); library(viridis)
## library(RColorBrewer)
## library(ggsci)
## library(wesanderson)
## wes_palette(name, n, type = c("discrete", "continuous"))
## names(wes_palettes)


## dir()
## getwd()
## (rep <- dir(pattern='*.Rmd'))
## help(browseURL)
## browseURL(url = render( rep[1] ))
## help(render)
getwd()
setwd('../Rmd.report')
dir()

render( "report_docx.Rmd",
       # "report_docx_temp.Rmd",
       output_format = "word_document",
       # output_file='./config/template.docx'
       output_file='./output/COMFOR_D1_1_1_RegionalReport_ComplexForests.docx'
       )


q()
y

