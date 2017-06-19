library(ape)
#library(geiger)
library(phytools)
#library(nlme)
#library(evomap)
#library(caper)
#library(adephylo)
library(phangorn)
#library(l1ou)
#library(geomorph)
#library(bayou)

setwd("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/Julia.Joos/Eigene Dateien/MA/Tortoise_Analyses")
#read tree
tree<-read.nexus("tree.nex") #package ape
plot(tree)
# get the matrix to make it ultrametric
matrix<-cophenetic.phylo(tree) #ape
tree<-upgma(matrix) #phangorn
#isolate clade you are interested in
tree2<-extract.clade(tree,findMRCA(tree,c("Manouria_impressa", "Indotestudo_forstenii"))) #ape

#"Pseudemys_texana", "Rhinoclemmys_nasuta"
#"Platysternon_megacephalum","Geoemyda_japonica"

plot(tree2)
#add fossils
targetNode<-findMRCA(tree2,c("Lamna_ditropis","Isurus_paucus")) #gives common ancestor     #phytools
tree2<-bind.tip(tree,"Carcharocles_megalodon",where=targetNode,position=0.002,edge.length=0.02) #phytools
#possition is ma before the node, lenght is how much it lasted

