#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(tidyr)
#library(stringi) # process character strings
##library(paleobioDB) #to load, visualize and process data from PDBD
#The following object is masked from 'package:dplyr':  select
#library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution
library(paleoTS) # analyze paleontological time-series



#### Data basis - import data set from Excel ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

#####prepare for analysis (fix column names in .csv-file after converting table from excel####
colnames(tidyCL)[6] <- "MAmin"
colnames(tidyCL)[7] <- "Mamax"
colnames(tidyCL)[17] <- "CL"
colnames(tidyCL)[18] <- "PL"

# write code for: meanAge, meanCL, SampleSize, TimeBins

##### data exploration ####
str(tidyCL)
dim(tidyCL)
names(tidyCL)
attributes(tidyCL)
head(tidyCL)
summary(tidyCL)
table(tidyCL$Country)
hist(tidyCL$CL)
#pairs(tidyCL)

library(scatterplot3d)
scatterplot3d(tidyCL$CL, tidyCL$Latitude, tidyCL$Longitude)
library(rgl)
plot3d(tidyCL$CL, tidyCL$Latitude, tidyCL$Longitude)


statsCL <- tidyCL %>%
  filter(!is.na(CL)) %>%
  summarise(min = min(CL), max = max(CL), var(CL), mean= mean(CL), median= median(CL), n=n())#, skew(CL), kurtosi(CL)) n = n(), 

write.table(statsCL,file="StatsCL.txt", sep="\t", row.names = FALSE)

##### Map localities with CL information and sample size (ggplot) ####
#setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
#tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

Map <- tidyCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL) %>%
  group_by(Latitude) %>%
  mutate(count= n())


mapWorld <- borders("world", colour="azure3", fill="azure3") # create a layer of borders, run line at the beginning (before loading plotly)


mapCL <- Map %>%
  ggplot(aes(Longitude, Latitude)) +# borders("world", ylim = c(-60, 90)) +
  mapWorld +
  geom_point(aes(Longitude, Latitude,colour=CL, size=count))


mapCL


##### interactive plots with plotly, load only when needed (after creating the "world"-map-environment) ####
library(plotly) # interactive plots
library(tidyverse) # interactive plots

ggplotly(mapCL) #make map interactive


#### Scatterplot CL ~ Age ####

plot <- tidyCL %>%
  select(Country, Latitude, Longitude, MAmin, Mamax, Genus, Species, Taxon, CL) %>%
  mutate(Age= (MAmin+Mamax)/2) %>%
  ggplot(aes(Age, CL, colour=Genus)) + geom_point()

plot
ggplotly(plot)

##### paleoTS ######
TidyCL <- tidyCL %>%
  select(MAmin, Mamax, CL) %>%
  filter(CL != "NA") %>%
  mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(CL), vv=var(CL), nn=n()) #create means etc. for each time bin 

TidyCL[is.na(TidyCL)]<-0 #subset NAs with O for 

TidyCL

bins <- tidyCL %>%
  #  select(MAmin, Mamax, CL) %>%
  filter(CL != "NA") %>%
  mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
  group_by(tt)

bins


paleoTidyCL <-as.paleoTS(TidyCL$mm, TidyCL$vv, TidyCL$nn, TidyCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode")
paleoTidyCL
plot(paleoTidyCL)

fit3models(paleoTidyCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess##### play around with speciesgeocodeR 07.06.17 #######



##### Plot all data on map, disregarding availablity of CL-information ####

All<-read.csv("tortoises13-04.csv", sep=";", header=TRUE)

colnames(All)[6] <- "Mamin"
colnames(All)[7] <- "Mamax"

ALL <- All %>%
  select(Locality, Country, Latitude, Longitude, Mamin, Mamax, Epoch, Genus, Species, Taxon, CL) %>%
  mutate(Age= (Mamin+Mamax)/2) %>%   # create mean age
  group_by(Latitude) %>%
  mutate(count= n())

mapWorld <- borders("world", colour="azure3", fill="azure3") # create a layer of borders  

mapAll <- ALL %>%
  ggplot(aes(Longitude, Latitude)) + mapWorld +
  #geom_point(fill="red", colour="red", size=0.5) +
  geom_point(aes(Longitude, Latitude,colour=Age, size=count))

mapAll

ggplotly(mapAll) # check if plotly and tidyverse have been loaded




#############################################
### play around with speciesgeocodeR 7.7.17 ######
# library(speciesgeocodeR)
# 
# # tab-separated file: #SpeciesName Lat Long #additionalColumn
# Map <- tidyCL %>%
#   select(Genus, Taxon, Latitude, Longitude, Country, CL, PL)
# 
# write.table(Map, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/map.txt",  sep="\t", row.names = FALSE)
# 
# setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
# #map<-read.csv("map.csv", sep=";", header=TRUE)

#occurrences
###occ <- read.table(system.file("extdata","map.csv", package = "speciesgeocodeR"), row.names = NULL)


############


#paleobioDB: works only with version 1.1 v6 (https://paleobiodb.org/data1.1/occs/single_doc.html)
# PDBD 1.2 v2: https://paleobiodb.org/data1.2/specs_doc.html


turtles <- pbdb_occurrences (limit="all", base_name="Testudinidae",
                  interval="Neogene", vocab="pbdb", show=c("coords", "phylo", "ident"))
head (turtles)
turtles$taxon_name
unique (turtles$matched_name)

