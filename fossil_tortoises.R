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
library(paleobioDB) #to load, visualize and process data from PDBD
# The following object is masked from 'package:dplyr':  select
# library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution

# library(RefManageR)
# biblio <- ReadZotero(user = 4138123, .params = list(collection = 'NPV27KD5'))


#### Data basis - import data set from Excel ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

#####prepare for analysis (fix column names in .csv-file after converting table from excel####
colnames(tidyCL)[6] <- "MAmin"
colnames(tidyCL)[7] <- "Mamax"
colnames(tidyCL)[17] <- "CL"
colnames(tidyCL)[18] <- "PL"
colnames(tidyCL)[21] <- "estimated"

tidyCL <-  tidyCL %>%
  mutate(Age= ((as.numeric(as.character(MAmin)))+(as.numeric(as.character(Mamax))))/2)

####### import extant data ####
extant <- read.csv("MFN_testudinidae.csv", sep=";", header=TRUE, dec=".", na.strings = "NA", stringsAsFactors=FALSE)  # file: MFN_testudinidae.csv

colnames(extant)[4] <- "SCL"
colnames(extant)[10] <- "PL"
colnames(extant)[11] <- "PLmid"


Extant <- extant %>%
  mutate(CL = SCL * 10, PL=PL*10, PLmid=PLmid*10) 


##### data exploration ####
str(tidyCL)
#dim(tidyCL)
#names(tidyCL)
#attributes(tidyCL)
#head(tidyCL)
summary(tidyCL)
table(tidyCL$Country)
hist(tidyCL$CL)
hist(tidyCL$PL)
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

statsPL <- tidyCL %>%
  filter(!is.na(PL)) %>%
  summarise(min = min(PL), max = max(PL), mean= mean(PL), median= median(PL), SD = sd(PL), variance = var(PL), n=n())#, skew(CL), kurtosi(CL)) n = n(), 


write.table(statsCL,file="StatsCL.txt", sep="\t", row.names = FALSE)


statsCLextant <- Extant %>%
  filter(!is.na(CL)) %>%
  summarise(min = min(CL), max = max(CL), mean= mean(CL), median= median(CL), SD = sd(CL), variance = var(CL), n=n())#, skew(CL), kurtosi(CL)) n = n(), 

statsPLextant <- Extant %>%
  filter(!is.na(PL)) %>%
  summarise(min = min(PL), max = max(PL), mean= mean(PL), median= median(PL), SD = sd(PL), variance = var(PL), n=n())#, skew(CL), kurtosi(CL)) n = n(), 

##########Extrapolate CL from Plastronlength########

CLPLtidy <- tidyCL %>%
  filter(!is.na(CL) & !is.na(PL)) %>%
  dplyr::select(Taxon, CL, PL, size, Age, Island, Continent) %>%
  mutate(ratio=CL/PL) %>%
  group_by(Taxon) %>% #to show ratios per Taxon, leave out to get a total ratio
  summarise(meanRatio=round(mean(ratio),2), sdRatio=round(sd(ratio),2), n=n(), min=min(ratio), max=max(ratio))

CLPLextant <- Extant %>%
  filter(!is.na(CL) & !is.na(PL)) %>%
  dplyr::select(Taxon=Species, CL, PL, PLmid, Island, Continent) %>%
  mutate(ratio=CL/PL, ratioMid=CL/PLmid)
  #group_by(Taxon) %>% #to show ratios per Taxon, leave out to get a total ratio
  #  summarise(meanRatio=round(mean(ratio),2), sdRatio=round(sd(ratio),2), n=n(), min=min(ratio), max=max(ratio))

Ratio <- CLPLextant %>%
  summarise(meanRatio=round(mean(ratio),2), sdRatio=round(sd(ratio),2), n=n(), min=min(ratio), max=max(ratio))

RatioSpecies <- CLPLextant %>%
  group_by(Taxon) %>% #to show ratios per Taxon, leave out to get a total ratio
  summarise(meanRatio=round(mean(ratio),2), sdRatio=round(sd(ratio),2), n=n(), min=min(ratio), max=max(ratio))


testRatio <- tidyCL %>%
  dplyr::select(Taxon, CL, PL, size, estimated, Age, Island, Continent) %>%
  mutate(extraCL = PL*Ratio$meanRatio) %>%
  dplyr::select(Taxon, CL, extraCL, PL, size, estimated, Age, Island, Continent)


testRatio$CL[is.na(testRatio$CL)] <- testRatio$extraCL[is.na(testRatio$CL)] # fill all CL NAs with extrapolated CLs

#write.table(testRatio,file="RatioCLPL.txt", sep="\t", row.names = FALSE)


##### Map localities with CL information and sample size (ggplot) ####
#setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
#tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

Map <- tidyCL %>%
  dplyr::select(Genus, Taxon, Latitude, Longitude, Country, CL, PL) %>%
  group_by(Latitude) %>%
  mutate(count= n())


#mapWorld <- borders("world", colour="azure3", fill="azure3") # create a layer of borders, run line at the beginning (before loading plotly)


mapCL <- Map %>%
  ggplot(aes(Longitude, Latitude)) +# borders("world", ylim = c(-60, 90)) +
  mapWorld +
  theme_classic() +
  geom_point(aes(Longitude, Latitude,colour=CL, size=count)) +
  scale_colour_gradientn(colors=c("orange", "red", "purple", "blue", "green"))
  


mapCL
ggplotly(mapCL) #make map interactive


#### Scatterplot CL ~ Age ####

CLAge <- tidyCL %>%
  select(Country, Latitude, Longitude, MAmin, Mamax, Genus, Species, Taxon, CL) %>%
  mutate(Age= (MAmin+Mamax)/2) %>%
  ggplot(aes(Age, CL, colour=Genus)) + geom_point() + theme_classic() 

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


paleoTidyCL <-as.paleoTS(TidyCL$mm, TidyCL$vv, TidyCL$nn, TidyCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Miocene, CL")
paleoTidyCL
plot(paleoTidyCL)

fit3models(paleoTidyCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess

### paleoTS for Pliocene/Pleistocene ####
#Bergmann$bin <- cut(Bergmann$lat, c(33, 38, 43, 48, 53, 58, 60))
age <- as.numeric(unique(testRatio$Age))
sort(age)

PleiPlio <- testRatio %>% #testRatio or tidyCL 
  filter(Age < 11.000)

PleiPlio$bin <- cut(PleiPlio$Age, c(0, 0.0117, 0.126, 0.781, 2.588, 3.6, 5.332, 11.608))

EpochBins <- as.vector(c("Holocene", "Upper Pleistocene", "Middle Pleistocene", "Lower Pleistocene", 
               "Upper Pliocene", "Lower Pliocene", "Upper Miocene"))

MeanBins <- as.vector(c((0+0.0117)/2, (0.0117+0.126)/2, (0.126+0.781)/2, (0.781+2.588)/2,
              (2.588+3.6)/2, (3.6+5.332)/2, (5.332+11.608)/2))

Bins <- PleiPlio %>%
  select(bin) %>%
  group_by(bin) %>%
  summarise(n=n())

bin <- as.vector(unique(Bins$bin))

BINS <- data.frame(bin, EpochBins, MeanBins) #



unique(tidyCL$Epoch)

PleiPlioCL <- PleiPlio %>%
  merge(BINS)

length(PleiPlioCL$CL)



PPCL <- PleiPlioCL %>%
  select(CL, MeanBins) %>%
  filter(CL != "NA") %>%
  mutate(tt= MeanBins) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(CL), vv=var(CL), nn=n()) #create means etc. for each time bin 

PPCL[is.na(PPCL)]<-0 #subset NAs with O for n=1



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
  
# meanALL <- sum(sumTort$meanCLmm)/length(sumTort$meanCLmm)
# varALL <- var(sumTort$meanCLmm)
# nALL <- sum(sumTort$n)

# combining already summarised data into one value per time bin
# source: https://www.researchgate.net/post/How_do_I_combine_mean_and_standard_deviation_of_two_groups

PPESCL <- bind_rows(SumTort,ExTort, PPCL)

#   group_by(tt) %>%
#   summarise(mm=sum(mm)/length(mm),  nn=sum(nn[which(tt==tt)]))  %>%
# #  group_by(mm) %>%
#   mutate(vv=var(mm)) %>%
#   select(mm, nn, vv, tt)


combiCL <- PPESCL %>%
  filter(tt==0) %>%   #maybe find a way to automatically filter double tt values...
  mutate(nx = nn*mm) %>%
  mutate(mmall=sum(nx)/sum(nn)) %>%
  mutate(SD=sqrt(nx), d=mm-mmall) %>%
  mutate(nsd=((nx^2+d^2)*nn)) %>%
  mutate(varall=sum(nsd)/sum(nn), n=sum(nn)) %>%
  dplyr::select(mm=mmall, vv=varall, nn=n, tt) %>%
  unique()



# Combi <- PPESCL %>%
#   filter(tt==0) %>%
# #  group_by(tt) %>%
#   summarise(mm=sum(nn*mm)/sum(nn),
#          vv=sum(((nn*mm)^2+(mm-(sum(nn*mm)/sum(nn)))^2)*nn)/sum(nn),  #figure out why it does not show the correct variance!!!
#          nn=sum(nn)
#          )

#write.table(combiCL,file="combiCL.txt", sep="\t", row.names = FALSE)


PPCL <- PPESCL %>%
  filter(tt !=0) %>%
  bind_rows(combiCL)%>%
  arrange(tt)


paleoPPCL <-as.paleoTS(PPCL$mm, PPCL$vv, PPCL$nn, PPCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Pliocene, CL")
paleoPPCL
plot(paleoPPCL)

fit3models(paleoPPCL, silent=FALSE, method="AD", pool=FALSE)   

# bins <- PleiPlioCL %>%
#   #  select(MAmin, Mamax, CL) %>%
#   filter(CL != "NA") %>%
#   mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
#   group_by(tt)
# 
# bins

#### with plastron lengths ####
PPPL <- PleiPlioCL %>%
  select(MAmin, Mamax, PL) %>%
  filter(PL != "NA") %>%
  mutate(tt= (MAmin+Mamax)/2) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(PL), vv=var(PL), nn=n()) #create means etc. for each time bin 

PPPL[is.na(PPPL)]<-0 #subset NAs with O for n=1


ExTortP <- extant %>%
  mutate(PL = PL * 10) %>%
  dplyr::select(PL) %>%
  summarise(mm=mean(PL), nn=n(), vv=var(PL), tt=0) %>%
  select(mm, nn, vv, tt)

ExTortP[is.na(ExTortP)] <- 0 #subset NAs with O for n=1


PPEPL <- bind_rows(ExTortP, PPPL) 

paleoPPPL <-as.paleoTS(PPEPL$mm, PPEPL$vv, PPEPL$nn, PPEPL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Pliocene, PL")
paleoPPPL
plot(paleoPPPL)

fit3models(paleoPPPL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess

###### paleoTS with extrapolated CLs #####

TR <- testRatio

TR$CL[is.na(TR$CL)] <- TR$extraCL[is.na(TR$CL)] # fill all CL NAs with extrapolated CLs

paleoTR <- TR %>%
  filter(Age < 10.000)

length(paleoTR$CL)

PTR <- paleoTR %>%
  select(Age, CL) %>%
  filter(CL != "NA") %>%
  mutate(tt= Age) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(CL), vv=var(CL), nn=n()) #create means etc. for each time bin 

PTR[is.na(PTR)]<-0 #subset NAs with O for n=1


ExTort <- extant %>%
  mutate(CL = SCL * 10) %>%
  dplyr::select(CL) %>%
  summarise(mm=mean(CL), nn=n(), vv=var(CL), tt=0) %>%
  select(mm, nn, vv, tt)

ExTort[is.na(ExTort)] <- 0 #subset NAs with O for n=1

#sumTort <- read.csv("tortoises_summary.csv", sep=";", header=TRUE)  # file: MFN_testudinidae.csv

SumTort <- sumTort %>%
  mutate(tt=(Mamin+Mamax)/2, vv=sdCLmm^2, nn=n, mm=meanCLmm) %>%
  dplyr::select(mm, nn, vv, tt)

PTRES <- bind_rows(SumTort,ExTort, PTR) 

combiCL <- PTRES %>%
  filter(tt==0) %>%   #maybe find a way to automatically filter double tt values...
  mutate(nx = nn*mm) %>%
  mutate(mmall=sum(nx)/sum(nn)) %>%
  mutate(SD=sqrt(nx), d=mm-mmall) %>%
  mutate(nsd=((nx^2+d^2)*nn)) %>%
  mutate(varall=sum(nsd)/sum(nn), n=sum(nn)) %>%
  dplyr::select(mm=mmall, vv=varall, nn=n, tt) %>%
  unique()


PPCL <- PTRES %>%
  filter(tt !=0) %>%
  bind_rows(combiCL)%>%
  arrange(tt)


paleoPPCL <-as.paleoTS(PPCL$mm, PPCL$vv, PPCL$nn, PPCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Pliocene, extrapolated CL")
paleoPPCL
plot(paleoPPCL)

fit3models(paleoPPCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess

#######Boxplots Island #########
names(TR)
names(extant)
names(sumTort)

TRI <- TR %>%
  dplyr::select(Taxon, CL, PL, Age, Island, Continent)

IslandEx <- extant %>%
  dplyr::select(Taxon=Species, SCL, PL,  Island, Continent) %>% # Age=0,  , n=1
  mutate(CL=SCL*10, PL=PL*10, Age=0) %>%#, n=1
  dplyr::select(Taxon, CL, PL,  Island, Continent, Age)
  
IslandSum <- sumTort %>%
  dplyr::select(Taxon=Species, CL=meanCLmm, Mamin, Mamax, Island, Continent, n) %>%  #PL=0, 
  mutate(Age=(Mamin+Mamax)/2, PL=0) %>%
  dplyr::select(Taxon, CL, PL, Age, Island, Continent, n)

Island <- bind_rows(TRI, IslandEx, IslandSum)

IslandBox <- Island %>%
  ggplot(aes(Island, CL)) + geom_boxplot() + geom_jitter(aes(colour=Age)) +
#  facet_grid(.~Age) +
  theme_classic() + # for white background 
  #stat_summary(fun.data = give.n, geom = "text") +
 # xlab("Sex") + ylab("Carapace length [mm]") + 
  theme(legend.background = element_rect(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)
        , strip.text.x = element_text(size = 12)) +
  scale_colour_brewer(palette="Set1")

IslandBox

IslandDot <- Island %>%
  ggplot(aes(Island, CL)) + geom_violin() + geom_jitter(aes(colour=Continent)) +
  theme_classic() + # for white background 
  #stat_summary(fun.data = give.n, geom = "text") +
  # xlab("Sex") + ylab("Carapace length [mm]") + 
  theme(legend.background = element_rect(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)
        , strip.text.x = element_text(size = 12)) +
  scale_colour_brewer(palette="Set1")

IslandDot

ContinentBox <- Island %>%
  ggplot(aes(Continent, CL)) + geom_boxplot() + geom_jitter(aes(colour=Island))+
  theme_classic() + # for white background 
  #stat_summary(fun.data = give.n, geom = "text") +
  # xlab("Sex") + ylab("Carapace length [mm]") + 
  theme(legend.background = element_rect(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA)
        , strip.text.x = element_text(size = 12)) +
  scale_colour_brewer(palette="Set1") 

ContinentBox

ContinentDot <- Island %>%
  ggplot(aes(Continent, CL)) + geom_violin()

ContinentDot


#############without Island species ###########
TR <- testRatio

TR$CL[is.na(TR$CL)] <- TR$extraCL[is.na(TR$CL)]

paleoTR <- TR %>%
  filter(Age < 10.000) %>%
  filter(Island=="n")

length(paleoTR$CL)

PTR <- paleoTR %>%
  select(Age, CL) %>%
  filter(CL != "NA") %>%
  mutate(tt= Age) %>% # create mean age
  group_by(tt) %>% #create time bins
  summarise(mm=mean(CL), vv=var(CL), nn=n()) #create means etc. for each time bin 

PTR[is.na(PTR)]<-0 #subset NAs with O for n=1


ExTort <- extant %>%
  mutate(CL = SCL * 10) %>%
  dplyr::select(CL) %>%
  summarise(mm=mean(CL), nn=n(), vv=var(CL), tt=0) %>%
  select(mm, nn, vv, tt)

ExTort[is.na(ExTort)] <- 0 #subset NAs with O for n=1

#sumTort <- read.csv("tortoises_summary.csv", sep=";", header=TRUE)  # file: MFN_testudinidae.csv

# SumTort <- sumTort %>%
#   filter(Island=="n") %>%
#   mutate(tt=(Mamin+Mamax)/2, vv=sdCLmm^2, nn=n, mm=meanCLmm) %>%
#   dplyr::select(mm, nn, vv, tt)

PTRES <- bind_rows(ExTort, PTR) #SumTort,

# combiCL <- PTRES %>%
#   filter(tt==0) %>%   #maybe find a way to automatically filter double tt values...
#   mutate(nx = nn*mm) %>%
#   mutate(mmall=sum(nx)/sum(nn)) %>%
#   mutate(SD=sqrt(nx), d=mm-mmall) %>%
#   mutate(nsd=((nx^2+d^2)*nn)) %>%
#   mutate(varall=sum(nsd)/sum(nn), n=sum(nn)) %>%
#   dplyr::select(mm=mmall, vv=varall, nn=n, tt) %>%
#   unique()


PPCL <- PTRES %>%
  filter(tt !=0) %>%
  bind_rows(combiCL)%>%
  arrange(tt)


paleoPPCL <-as.paleoTS(PPCL$mm, PPCL$vv, PPCL$nn, PPCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode, Pliocene, extrapolated CL")
paleoPPCL
plot(paleoPPCL)

fit3models(paleoPPCL, silent=FALSE, method="AD", pool=FALSE)  



#### Map PlioPleiCL-data ###

PPmap <- tidyCL %>%
  filter(Age < 11.000) %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL, Age) %>%
  group_by(Latitude) %>%
  mutate(count= n()) %>%
  ggplot(aes(Longitude, Latitude)) + mapWorld + theme_classic() +
  geom_point(aes(Longitude, Latitude,colour=CL, size=count)) +
  scale_colour_gradientn(colors=c("orange", "red", "purple", "blue", "green"))

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
axisPhylo()   # add time scale

writeNexus(tree2, file="tree_testudinidae.nex")

#add fossils
targetNode<-findMRCA(tree2,c("Astrochelys_radiata","Aldabrachelys_grandidieri")) #gives common ancestor 
targetNode<-findMRCA(tree2,c("Aldabrachelys_gigantea","Aldabrachelys_grandidieri")) #gives common ancestor     #phytools
tree_fossil<-bind.tip(tree2,"Aldabrachelys_abrupta†",where=targetNode,position=0,edge.length=32) #phytools
#position is ma before the node, lenght is how long it lasted
#A. abrupta: position=0,edge.length=24.85501
#34.855759-0.00075 = edge.lentgh -> but can't be right, because A. abrupta lasted till Late Holocene
plot(tree_fossil)
axisPhylo()

writeNexus(tree_fossil, file="tree_fossil.nex")

# ## source for the following:http://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
# # I think the ages of species and nodes is displayed, user targetNode to determine wich node you are dealing with
# mytree <- tree_fossil
# 
# myvector<-data.frame(as.matrix(dist.nodes(mytree))
#                      [,length(mytree$tip)+1],rownames=c(rbind(mytree$tip.label),c((length(mytree$tip)+1):max(length(dist.nodes(mytree)[,1])))))
# 


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



setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
pdbd <- pbdb_occurrences (limit="all", base_name="Testudinidae",
                  interval="Neogene", vocab="pbdb", show=c("coords", "phylo", "ident"))
head (pdbd)
pdbd$taxon_name
unique (pdbd$matched_name)


PDBD <- pdbd %>%
  dplyr::select(matched_name, taxon_name, early_interval, late_interval, early_age, late_age,
                lng, lat, reference_no) %>%
  mutate(mean_age= (early_age+late_age)/2) %>%
  dplyr::filter(mean_age < 14.000) #%>%

  





setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
PDBDRef<-read.csv("pdbd_references.csv", sep=",", header=TRUE)




###########################

All<-read.csv("tortoises13-04.csv", sep=";", header=TRUE)


allSp <- All %>%
  dplyr::select(Reference, Taxon) %>%
  rename(Species=Taxon)

extantSp <- extant %>%
  dplyr::select(Reference, Species)



veganAll <- allSp %>%
  # bind_rows(extantSp) %>%
  group_by(Reference, Species) %>%
  summarise(n=n()) %>%
  tidyr::spread(Species, n, fill=0)

veganAllEx <- allSp %>%
  bind_rows(extantSp) %>%
  group_by(Reference, Species) %>%
  summarise(n=n()) %>%
  tidyr::spread(Species, n, fill=0)

library(vegan)

#head(vegan)
veganAll=veganAll[,-1]
vegansp=specaccum(veganAll,method="rarefaction", permutations=1000)
veganAllEx=veganAllEx[,-1]
veganspAll=specaccum(veganAllEx,method="rarefaction", permutations=1000)

par(mfrow=c(2, 2))
plot(vegansp,xlab="Ind",ylab="Richness", xvar="individuals", ci.type="line", ci.lty=2, ci.col="grey", col="deepskyblue4", lwd=2)
plot(veganspAll,xlab="Ind",ylab="Richness", xvar="individuals", ci.type="line", ci.lty=2, ci.col="grey", col="deepskyblue4", lwd=2)


par(mfcol=c(2, 1)) # mfrow: side by side
plot(vegansp,xlab="Ind",ylab="Richness", xvar="individuals", ci.type="line", ci.lty=2, ci.col="grey", col="deepskyblue4", lwd=2)
plot(veganspAll,xlab="Ind",ylab="Richness", xvar="individuals", ci.type="line", ci.lty=2, ci.col="grey", col="deepskyblue4", lwd=2)

