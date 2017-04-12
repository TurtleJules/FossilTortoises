#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(tidyr)
library(stringi)
library(paleobioDB) #to load, visualize and process data from PDBD
#The following object is masked from 'package:dplyr':  select
library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution
library(paleoTS) # analyze paleontological time-series

#### Data basis ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")

### FosFarBase ####
ALL<-read.csv(choose.files(" "), sep=";", header=TRUE)

Ref <- ALL %>%
  filter(CL == "-") %>%
  tidyr::separate(Reference, c("Ref1", "Ref2", "Ref3", "Ref4", "Ref5", "Ref6", "Ref7", "Ref8", "Ref9"), sep=" or ") %>%
  tidyr::gather(RefNr, Reference, Ref1, Ref2, Ref3, Ref4, Ref5, Ref6, Ref7, Ref8, Ref9, na.rm=TRUE) %>% 
  tidyr::separate(Reference, c("AuthorYear", "TitleJournal", "Rest", "Rest2"), sep=":") %>%
#  tidyr::separate(Ref2, c("Ref2", "Ref3"), sep=" or ") %>%
#  tidyr::separate(AuthorYear, c("Author", "Year"), sep=",") %>%
#  mutate(RefYear=Year)
  mutate(RefYear=stri_sub(as.vector(AuthorYear), from = -5, to = -1)) %>%
#  tidyr::separate(AuthorYear, c("Author", "Year"), "(?<=[a-z]) ?(?=[1-2])")
#  mutate(Year = str_sub(Ref$AuthorYear,-5, -1)) #%>%
  tidyr::unite(TitleJournal, TitleJournal, Rest, Rest2, sep=" ") %>%
  tidyr::separate(stri_reverse(TitleJournal), c("Title", "Journal", "rest", "rest2" ), sep="(?<=\\.)")

# TO DO: Journal in eigene Spalte kriegen! #######
#unique(Ref$RefNr[which(Ref$Locality == "Sandalja near Pula")])
#Ref[6,29] # [line, column]

AuthorYear <- as.vector(Ref$AuthorYear)
Year <- stri_sub(AuthorYear, from = -5, to = -1)

ALLCL <- ALL %>% 
  filter(CL != "-")


### Checklist ####

Check<-read.csv(choose.files(" "), sep=";", header=TRUE)

CheckCL <- Check %>% 
  filter(CL..cm. != "na") %>%
  filter(Family == "Testudinidae")

ALL<-read.csv(choose.files(" "), skip = 17,sep=",", header=TRUE) # read csv from line 18 (skip), separated with comma (NOT WORKING)

#try with paleobioDB instead of reading file
x=read.csv("PBDB_fossil_testudines.csv")

head(x)

