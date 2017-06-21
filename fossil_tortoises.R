#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(tidyr)
library(paleoTS) # analyze paleontological time-series
mapWorld <- borders("world", colour="azure3", fill="azure3") # create a layer of borders, run line at the beginning (before loading plotly)
library(plotly) # interactive plots
library(tidyverse) # interactive plots
library(ape)
library(phytools)
# library(phangorn) # to make tree ultramteric
# library(picante)
# library(stringi) # process character strings
# library(paleobioDB) #to load, visualize and process data from PDBD
# The following object is masked from 'package:dplyr':  select
# library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution



#### Data basis - import data set from Excel ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

#####prepare for analysis (fix column names in .csv-file after converting table from excel####
colnames(tidyCL)[6] <- "MAmin"
colnames(tidyCL)[7] <- "Mamax"
colnames(tidyCL)[17] <- "CL"
colnames(tidyCL)[18] <- "PL"

# write code for: meanAge, meanCL, SampleSize, TimeBins
tidyCL <-  tidyCL %>%
  mutate(Age= (MAmin+Mamax)/2)


##### data exploration ####
str(tidyCL)
dim(tidyCL)
names(tidyCL)
attributes(tidyCL)
head(tidyCL)
summary(tidyCL)
table(tidyCL$Country)
hist(tidyCL$CL)
hist(tidyCL$Age)

#pairs(tidyCL)

#library(scatterplot3d)
#scatterplot3d(tidyCL$CL, tidyCL$Latitude, tidyCL$Longitude)
#library(rgl)
#plot3d(tidyCL$CL, tidyCL$Latitude, tidyCL$Longitude)

##### Statistics #####
statsCL <- tidyCL %>%
  filter(!is.na(CL)) %>%
  summarise(min = min(CL), max = max(CL), mean= mean(CL), median= median(CL), SD = sd(CL), variance = var(CL), n=n())#, skew(CL), kurtosi(CL)) n = n(), 

write.table(statsCL,file="StatsCL.txt", sep="\t", row.names = FALSE)

##### Map localities with CL information and sample size (ggplot) ####
#setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
#tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

Map <- tidyCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL) %>%
  group_by(Latitude) %>%
  mutate(count= n())


#mapWorld <- borders("world", colour="azure3", fill="azure3") # create a layer of borders, run line at the beginning (before loading plotly)


mapCL <- Map %>%
  ggplot(aes(Longitude, Latitude)) +# borders("world", ylim = c(-60, 90)) +
  mapWorld +
  geom_point(aes(Longitude, Latitude,colour=CL, size=count))


mapCL
ggplotly(mapCL) #make map interactive


#### Scatterplot CL ~ Age ####

CLAge <- tidyCL %>%
  select(Country, Latitude, Longitude, MAmin, Mamax, Genus, Species, Taxon, CL) %>%
  mutate(Age= (MAmin+Mamax)/2) %>%
  ggplot(aes(Age, CL, colour=Genus)) + geom_point()

CLAge
ggplotly(CLAge)

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

fit3models(paleoTidyCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess

### paleoTS for Pliocene/Pleistocene ####
unique(tidyCL$Epoch)

PleiPlioCL <- tidyCL %>%
  filter(Age < 10.000)

length(PleiPlioCL$CL)

PPCL <- PleiPlioCL %>%
  select(MAmin, Mamax, CL) %>%
  filter(CL != "NA") %>%
  mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(CL), vv=var(CL), nn=n()) #create means etc. for each time bin 

PPCL[is.na(PPCL)]<-0 #subset NAs with O for n=1


extant <- read.csv("MFN_testudinidae.csv", sep=";", header=TRUE)  # file: MFN_testudinidae.csv

ExTort <- extant %>%
  mutate(CL = SCL * 10) %>%
  dplyr::select(CL) %>%
  summarise(mm=mean(CL), nn=n(), vv=var(CL), tt=0) %>%
  select(mm, nn, vv, tt)

ExTort[is.na(ExTort)] <- 0 #subset NAs with O for n=1

sumTort <- read.csv("tortoises_summary.csv", sep=";", header=TRUE)  # file: MFN_testudinidae.csv

SumTort <- sumTort %>%
#  summarise(mm=sum(meanCLmm), nn=sum(n), vv=var(CL), tt=0)
  mutate(tt=(Mamin+Mamax)/2, vv=sdCLmm^2, nn=n, mm=meanCLmm) %>%
  dplyr::select(mm, nn, vv, tt)
  
meanALL <- sum(sumTort$meanCLmm)/length(sumTort$meanCLmm)
varALL <- var(sumTort$meanCLmm)
nALL <- sum(sumTort$n)


PPCL <- bind_rows(SumTort,ExTort, PPCL) # 


PPCL


paleoPPCL <-as.paleoTS(PPCL$mm, PPCL$vv, PPCL$nn, PPCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode")
paleoPPCL
plot(paleoPPCL)

fit3models(paleoPPCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess

# bins <- PleiPlioCL %>%
#   #  select(MAmin, Mamax, CL) %>%
#   filter(CL != "NA") %>%
#   mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
#   group_by(tt)
# 
# bins
#### Map PlioPleiCL-data ###

PPmap <- PleiPlioCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL, Age) %>%
  group_by(Latitude) %>%
  mutate(count= n()) %>%
  ggplot(aes(Longitude, Latitude)) + mapWorld +
  geom_point(aes(Longitude, Latitude,colour=CL, size=count))

PPmap

ggplotly(PPmap)






PPmap <- PleiPlioCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL, Age) %>%
  group_by(Latitude) %>%
  mutate(count= n()) %>%
  ggplot(aes(Longitude, Latitude)) + mapWorld +
  #geom_point(fill="red", colour="red", size=0.5) +
  geom_point(aes(Longitude, Latitude,colour=Age, size=count))

PPmap

ggplotly(PPmap)


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


########### PHYLOGENY ############
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/Tortoise_Analyses")
#read tree
tree<-read.nexus("tree.nex") #package ape
plot(tree)
tree2<-extract.clade(tree,findMRCA(tree,c("Manouria_impressa", "Indotestudo_forstenii"))) #package ape
plot(tree2)

#add fossils
targetNode<-findMRCA(tree2,c("Astrochelys_radiata","Aldabrachelys_grandidieri")) #gives common ancestor     #phytools
tree_fossil<-bind.tip(tree2,"Aldabrachelys_abrupta",where=targetNode,position=0,edge.length=24.85501) #phytools
#position is ma before the node, lenght is how long it lasted
#A. abrupta: position=0,edge.length=24.85501
#24.855759-0.00075 = edge.lentgh -> but can't be right, because A. abrupta lasted till Late Holocene
plot(tree_fossil)




## source for the following:http://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
# I think the ages of species and nodes is displayed, user targetNode to determine wich node you are dealing with
mytree <- tree_fossil

myvector<-data.frame(as.matrix(dist.nodes(mytree))
                     [,length(mytree$tip)+1],rownames=c(rbind(mytree$tip.label),c((length(mytree$tip)+1):max(length(dist.nodes(mytree)[,1])))))



################ Plot tree on timescale ###############
# library(strap)
# # source for the following: http://schmitzlab.info/phylo2.html
# #let's mulitply branches by 40 to cover more time! Try different factors!
# tree$edge.length <- 40*tree$edge.length
# #we also must specify the root time
# tree$root.time <- max(nodeHeights(tree))
# #now we can plot the tree against the geologic timescale
# geoscalePhylo(tree, cex.ts=0.6, cex.tip=0.6)
# #export as pdf

##till's code
# tree <- read.tree("tree.txt")
# plot(tree)
# 
# species <- read.table("species.txt")    #_tree
# 
# tree_sp <- tree$tip.label
# species_sp <- as.character(species$V1)
# 
# matched_tips <- na.omit(match(species_sp,tree_sp))
# free_tips <- tree_sp[-matched_tips]
# 
# Tree2 <- drop.tip(tree, free_tips)
# Tree2
# plot(Tree2)
# 
# library(phytools)
# writeNexus(tree, file="tree")
# 
# treetest <- read.nexus("tree")
# plot(treetest)

################### SpeciesGeoCodeR ##########################
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


############ PaleoDB #################
#paleobioDB: works only with version 1.1 v6 (https://paleobiodb.org/data1.1/occs/single_doc.html)
# PDBD 1.2 v2: https://paleobiodb.org/data1.2/specs_doc.html

# turtles <- pbdb_occurrences (limit="all", base_name="Testudinidae",
#                   interval="Neogene", vocab="pbdb", show=c("coords", "phylo", "ident"))
# head (turtles)
# turtles$taxon_name
# unique (turtles$matched_name)



