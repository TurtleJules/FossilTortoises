#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(paleobioDB) #to load, visualize and process data from PDBD
#The following object is masked from 'package:dplyr':  select
library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution
library(paleoTS) # analyze paleontological time-series

#### Data basis ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")

### FosFarBase ####
ALL<-read.csv(choose.files(" "), sep=";", header=TRUE)

ALLCL <- ALL %>% 
  filter(CL != "-") %>%
  filter(!grepl("o.",gen_linien)) %>%
  filter(gen_linien != "IX" & gen_linien != "VIII" & gen_linien != "X") # 3 Linien > 10 samples rausfiltern


### Checklist ####

Check<-read.csv(choose.files(" "), sep=";", header=TRUE)

CheckCL <- Check %>% 
  filter(CL..cm. != "na") %>%
  filter(Family == "Testudinidae")

ALL<-read.csv(choose.files(" "), skip = 17,sep=",", header=TRUE) # read csv from line 18 (skip), separated with comma (NOT WORKING)

#try with paleobioDB instead of reading file
x=read.csv("PBDB_fossil_testudines.csv")

head(x)

