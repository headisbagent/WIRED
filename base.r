library(data.table)
library(reshape2)
library(gdxtools)
library(ggplot2)
library(rgdal)
library(ggmap)
library(placement)
library(sf)
library(lubridate)
library(ggnewscale)
library(RColorBrewer)
library(gganimate)
library(transformr)
library(e1071)
library(dplyr)
library(bit64)
library(cowplot)
library(patchwork)

igdx(gams.executable.location)

base.dir <- getwd()

print.lst.status <- function(file) {
	lst.file <- readLines(file)
	print(grep('SOLVER STATUS',lst.file,value=TRUE))
	print(grep('MODEL STATUS',lst.file,value=TRUE))
}