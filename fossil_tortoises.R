#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(tidyr)
#library(stringi) # process character strings
##library(paleobioDB) #to load, visualize and process data from PDBD
#The following object is masked from 'package:dplyr':  select
library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution
library(paleoTS) # analyze paleontological time-series

#### Data basis ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")

### FosFarBase ####
ALL<-read.csv(choose.files(" "), sep=";", header=TRUE) # file: tortoises13-04.csv

# wie viele CLs verfügbar?
# 01.05.: 34/768


species <- unique(ALL$Taxon)


Ref <- ALL %>%
  filter(CL == "-") %>%
  group_by(Country, Locality) %>%
  tidyr::separate(Reference, c("Ref1", "Ref2", "Ref3", "Ref4", "Ref5", "Ref6", "Ref7", "Ref8", "Ref9"), sep=" or ") %>%
  tidyr::gather(RefNr, Reference, Ref1, Ref2, Ref3, Ref4, Ref5, Ref6, Ref7, Ref8, Ref9, na.rm=TRUE) %>% 
  tidyr::separate(Reference, c("AuthorYear", "TitleJournal", "Rest", "Rest2"), sep=":") %>%
  mutate(RefYear=stri_sub(as.vector(AuthorYear), from = -5, to = -1)) %>%
#  tidyr::separate(AuthorYear, c("Author", "Year"), "(?<=[a-z]) ?(?=[1-2])")
#  mutate(Year = str_sub(Ref$AuthorYear,-5, -1)) #%>%
  tidyr::unite(TitleJournal, TitleJournal, Rest, Rest2, sep=" ")

Lit <- Ref %>%
  group_by(Country, Locality) %>%
  select(Country, Locality, Latitude, Longitude, Epoch, Taxon, Author,comment, collection,
         CL, RefNr, AuthorYear, RefYear, TitleJournal, note)


write.csv(Lit, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/References.txt", fileEncoding = "UTF-16LE") #fileEncoding keeps column structure without havin to drop row names
#, sep="\t"

Referenc <- unique(Lit$TitleJournal)
Reference<- na.omit(Referenc)
write.table(Reference, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/Reference.txt",  sep="\t")

# Errors
# 1. Lit[367,14]  
# 1. Lit$TitleJournal[which(Lit$Locality == "Cava Monticino, near Brisigella, Emilia-Romana")]  --> needs to be fixed (same as above)
# 2. Lit$TitleJournal[which(Lit$Country == "Japan")]
# 3. Lit$TitleJournal[which(Lit$Country == "Oman")]

#new<-read.csv(choose.files(" "), header=T, sep=";")   #, header=TRUE
new<-read.delim("ref.csv", header=T, sep=";")

References <- Lit %>%
  merge(new, by = "TitleJournal", all.x= TRUE) %>%
  select(Country, Locality, Latitude, Longitude, Epoch, Taxon, Author,comment, collection,
         CL, RefNr, AuthorYear, RefYear, TitleJournal, Journal, note) %>%
  arrange(Country, Locality, Taxon, RefNr)


write.table(References, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/References_new2.txt",  sep="\t", row.names = FALSE)

# TO DO: Journal in eigene Spalte kriegen! #######
#unique(Ref$RefNr[which(Ref$Locality == "Sandalja near Pula")])
#Ref[6,29] # [line, column]



###### 17.5.17: organize references: which paper do I need from which journals??#####
references <- read.csv(choose.files(" "), sep=";", header = TRUE)  # file: REFERENCES.csv (17.5.17)

journal <- unique(references$Journal)
paper <- unique(references$TitleJournal)


Ref <- references %>%
  dplyr::select(Journal, TitleJournal, note, AuthorYear, RefYear, RefNr, CL, Country, Locality, Latitude, Longitude, Epoch, Taxon, Author, comment, collection) %>%
  filter(note == "not available") %>%
  tidyr::unite(Taxa, Taxon, Author, comment, collection, sep="_", remove=FALSE) %>%
  dplyr::select(Journal, TitleJournal, note, AuthorYear, RefYear, RefNr, CL, Country, Locality, Latitude, Longitude, Epoch, Taxa) %>%
  group_by(Journal, TitleJournal, AuthorYear, RefYear, RefNr)
  

write.table(Ref, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/References_22.5.txt",  sep="\t", row.names = FALSE)

#  tidyr::unite(eggs, EggNumber, gen_linien, sep="-", remove=FALSE)
### TO DO: gather everything starting with Locality to get actual number of missing papers! ####


######

ALLCL <- ALL %>% 
  filter(CL != "-")





### Checklist ####

Check<-read.csv(choose.files(" "), sep=";", header=TRUE)

CheckCL <- Check %>% 
  filter(CL..cm. != "na") %>%
  filter(Family == "Testudinidae") %>%
  dplyr::select(Taxon.Species, Age, CL..cm.)

age <- unique(CheckCL$Age)

# ALL<-read.csv(choose.files(" "), skip = 17,sep=",", header=TRUE) # read csv from line 18 (skip), separated with comma (NOT WORKING)

write.table(CheckCL, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/test26.5.txt",  sep="\t", row.names = FALSE)


# Test paleoTS ####
library(paleoTS)


test<-read.csv(choose.files(" "), sep=";", header=TRUE) # file: test26.5.csv
# tortoises_tidy.csv

test<-read.csv("test26.5.csv", sep=";", header=TRUE)

Test1 <- test %>%
  mutate(mm = CL_mean, vv=0, nn= n, tt=Age_mean) %>%
  dplyr::select(mm, vv, nn, tt)

Test2 <- test %>%
  group_by(Age_mean) %>%
  summarise(mm = mean(CL_mean), nn=n(), vv=var(CL_mean)) %>%
  mutate(tt=Age_mean) %>%
  dplyr::select(mm, vv, nn, tt)

# NA: column 2, rows 3, 10, 13, 14, 15
Test2[3,2] <- 0
Test2[10,2] <- 0
Test2[13,2] <- 0
Test2[14,2] <- 0
Test2[15,2] <- 0


paleoTest1 <-as.paleoTS(Test1$mm, Test1$vv, Test1$nn, Test1$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode")
paleoTest1
plot(paleoTest1)

paleoTest2 <-as.paleoTS(Test2$mm, Test2$vv, Test2$nn, Test2$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode")
paleoTest2
plot(paleoTest2)




fit3models(paleoTest2, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess
#a=fit3models(meg) 
a
str(a)
a$AICc[1]

#### try with tidy data 6.6.17 ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
tidyCL<-read.csv("tortoises_tidy.csv", sep=";", header=TRUE)

# write code for: meanAge, meanCL, SampleSize, TimeBins


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



library(paleoTS)
paleoTidyCL <-as.paleoTS(TidyCL$mm, TidyCL$vv, TidyCL$nn, TidyCL$tt, MM = NULL, genpars = NULL, label = "Testudinidae body size evolution mode")
paleoTidyCL
plot(paleoTidyCL)

fit3models(paleoTidyCL, silent=FALSE, method="AD", pool=FALSE)   #not working with Test1, because no variances/sample sizes available, I guess##### play around with speciesgeocodeR 07.06.17 #######



### play around with speciesgeocodeR 7.7.17 ######
library(speciesgeocodeR)

# tab-separated file: #SpeciesName Lat Long #additionalColumn
Map <- tidyCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL)

write.table(Map, "//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/map.txt",  sep="\t", row.names = FALSE)

setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")
#map<-read.csv("map.csv", sep=";", header=TRUE)

#occurrences
###occ <- read.table(system.file("extdata","map.csv", package = "speciesgeocodeR"), row.names = NULL)

#map with ggplot ####

#Using GGPLOT, plot the Base World Map



Map <- tidyCL %>%
  select(Genus, Taxon, Latitude, Longitude, Country, CL, PL) %>%
  group_by(Latitude) %>%
  mutate(count= n())

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

#count <- Map$Latitude %>%
#  group_by(Latitude)%>%
#  n()

mp <- Map %>%
  ggplot(aes( Longitude, Latitude,colour=CL)) + mapWorld +
  geom_point(aes(size=count))

mp
####





library(maps)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(x=Map$Longitude,y=Map$Latitude.y, col="red", pch=16)

#paleobioDB: works only with version 1.1 v6 (https://paleobiodb.org/data1.1/occs/single_doc.html)
# PDBD 1.2 v2: https://paleobiodb.org/data1.2/specs_doc.html


turtles <- pbdb_occurrences (limit="all", base_name="Testudinidae",
                  interval="Neogene", vocab="pbdb", show=c("coords", "phylo", "ident"))
head (turtles)
turtles$taxon_name
unique (turtles$matched_name)

