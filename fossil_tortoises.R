#### Packages ####
library(ggplot2) # for plots and graphs (cheat sheet available)
library(dplyr) # for organizing data (data wrangling cheat sheet available)
library(tidyr)
#library(stringi) # process character strings
library(paleobioDB) #to load, visualize and process data from PDBD
#The following object is masked from 'package:dplyr':  select
library(speciesgeocodeR) # categorization of species occurrences for biodiversity, biogeography, ecology and evolution
library(paleoTS) # analyze paleontological time-series

#### Data basis ####
setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA")

### FosFarBase ####
ALL<-read.csv(choose.files(" "), sep=";", header=TRUE) # file: tortoises13-04.csv

# wie viele CLs verfügbar?
# 01.05.: 34/768

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
  filter(Family == "Testudinidae")

# ALL<-read.csv(choose.files(" "), skip = 17,sep=",", header=TRUE) # read csv from line 18 (skip), separated with comma (NOT WORKING)




#try with paleobioDB instead of reading file

turtles <- pbdb_occurrences (limit="all", base_name="Testudinidae",
                  interval="Neogene", vocab="pbdb", show=c("coords", "phylo", "ident"))
head (turtles)
turtles$taxon_name
unique (turtles$matched_name)
