library(ape)
library(geiger)
library(phytools)
library(nlme)
library(evomap)
library(caper)
library(adephylo)
library(phangorn)
library(l1ou)
library(geomorph)
library(bayou)


#read tree
tree<-read.nexus("ConsensusTree_Bayesian_SharkPhylogeny.nex")
# get the matrix to make it ultrametric
matrix<-cophenetic.phylo(tree)
tree<-upgma(matrix)
#isolate clade you are intersted in
tree2<-extract.clade(tree,findMRCA(tree,c("Galagoides_demidoff","Microcebus_murinus")))

#add fossils
targetNode<-findMRCA(tree2,c("Lamna_ditropis","Isurus_paucus")) #gives common ancestor     
tree2<-bind.tip(tree,"Carcharocles_megalodon",where=targetNode,position=0.002,edge.length=0.02)
#possition is ma before the node, lenght is how much did it lasted

