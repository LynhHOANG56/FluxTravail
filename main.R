############################ Deplacement Domicile - Travail  ############################
##############
# chargement des packages nécessaires
library(igraph)
library(maptools)
library(cartography)

# chargement des données 
load("dep.rdata")
depMig <- read.csv("Flux_Travail.csv", header=T,sep=",")

# extraction des coordonnées des centroides des departements
centres <- cbind(depGeom@data[,"CODE_DEPT"],
                 as.data.frame(coordinates(depGeom)))
names(centres)<-c("id","X","Y")

# le nombre d'arrivées
attract <- aggregate(depMig$fij, by = list(depMig$dest), sum)
names(attract) <- c("id", "nbarrivee")

# liste des départements à exclure
listExcl <- c("75", "92", "77", "78", "93", "94", "95", "97", "99") 
listExcl <- c("75", "97", "99")
listExcl <- c() # non
# exclusion de ces départements
mig <- depMig[!depMig$dest %in% listExcl & !depMig$orig %in% listExcl,]
# liste des départements à sélectionner 
listDep <- attract[order(attract$nbarrivee[!attract$id %in% listExcl], 
                         decreasing = T),][,"id"]
# sélection de ces départements
mig <- mig[mig$orig %in% listDep & mig$dest %in% listDep, ]

# transformation des flux sélectionnés en graph
g <- data.frame(mig[,c("orig", "dest")])
g <- as.matrix(g)
g <- graph.edgelist(g, directed=TRUE)

# création de la liste des noeuds (vertex)
vert <- data.frame(id = V(g)$name)

# affectation des positions des coordonnées des centroides aux noeuds
coords <- centres[match(vert$id, table = centres$id), ]
coords <- as.matrix(coords[,2:3])
g$layout <- coords

# ajout d'un attribut  aux noeuds
V(g)$weights <- attract[match(vert$id, table = attract$i), "nbarrivee"]

# ajout d'un attribut d'épaisseur aux liens (edge)
x <- get.edgelist(g)
E(g)$fij <- mig[match(x = paste(x[,1],x[,2],sep="_"), 
                      table = paste(mig$orig,mig$dest,sep="_")),"fij"]

##################################  
# Plot la carte de Flux
par(mar=c(0,0,0,0),oma = c(0,0,0,0) )
# Affichage de la carte des départements
plot(depGeom, col="khaki1", bg = "grey", border = "#ffffff00")
# ajout des flèches une par une
for (e in seq_len(ecount(g))) {
  graph2 = delete.edges(g, E(g)[(1:ecount(g))[-e]])
  plot(graph2, 
       vertex.size = (V(g)$weights)*0.12,
       edge.curved = 0.5,
       edge.width= if (E(graph2)$fij /2000 >8311/2000){8311/2700}
       else {E(graph2)$fij /3000},
       # nous limitons la taille des plus grosses flèches à 1.75 
       edge.arrow.size = 0.123,
       # couleur
       t <- E(graph2)$fij,
       edge.color = if(t<=8311) {"green4" 
       }  else {
         edge.color = if((t<=27179)&(t>8311)) {"tan1"}
         else {
           edge.color = if((t<=57292)&(t>28995)) {"blue"}
           else {
             edge.color = if((t<=93721)&(t>61744)) {"darkorchid2"}
             else {"red" }}}},
       vertex.label=NA, 
       add=TRUE, 
       rescale = F
  )
}

legend("topright", legend=c("200 à 8311 (841)", "8330 à 27179 (59)
", "28955 à 57292 (18)", "61744 à 93721 (7)", "105011 à 214750 (5)"),
       col=c("green4","tan1", "blue","darkorchid2","red"), 
       lwd=2,lty=1, cex=0.65)
mtext(text = " Flux domicile - lieu de travail 2016",side = 3,adj=0,cex=1.2, line = -2)

##################################  
# Plot la carte de Flux avec le nombre d’arrivées de chaque de département
# sauf l'Ile-de-France
listExcl <- c("75", "92", "77", "78", "93", "94", "95", "97", "99") 
par(mar=c(0,0,0,0),oma = c(0,0,0,0) )
# Affichage de la carte des départements
plot(depGeom, col="khaki1", bg = "grey", border = "#ffffff00")
# ajout des flèches une par une
for (e in seq_len(ecount(g))) {
  graph2 = delete.edges(g, E(g)[(1:ecount(g))[-e]])
  plot(graph2, 
       vertex.size = (V(g)$weights)*80,
       edge.curved = 0.5,
       edge.width= if (E(graph2)$fij /2000 >8311/2000){8311/2700}
       else {E(graph2)$fij /3000},
       # nous limitons la taille des plus grosses flèches à 1.75 
       edge.arrow.size = 0.123,
       # couleur
       t <- E(graph2)$fij,
       edge.color = if(t<=8311) {"green4" 
       }  else {
         edge.color = if((t<=27179)&(t>8311)) {"tan1"}
         else {
           edge.color = if((t<=57292)&(t>28995)) {"blue"}
           else {
             edge.color = if((t<=93721)&(t>61744)) {"darkorchid2"}
             else {"red" }}}},
       vertex.label=NA, 
       add=TRUE, 
       rescale = F
  )
}

legend("topright", legend=c("200 à 8311 (841)", "8330 à 27179 (59)
", "28955 à 57292 (18)", "61744 à 93721 (7)", "105011 à 214750 (5)"),
       col=c("green4","tan1", "blue","darkorchid2","red"), 
       lwd=2,lty=1, cex=0.65)
legendCirclesSymbols(var = c(min(E(g)$fij),max(E(g)$fij)),
                     title.txt = "Nombre d'arrivées", 
                     col = "orange", inches = 0.24)
mtext(text = " Flux domicile - lieu de travail 2016 (hors Ile de France)",
      side = 1,adj=0,cex=1.2, line = -2)                 


####################################################################################
############## Moyen de transports pour se rendre au travail ##############
library(cartography)
library(raster)

dt0 <- read.csv("moyen_transport.csv", header=T,sep=","); dim(dt0); head(dt0)
indD <- dt0$indD
### ############### Deux.roues
FranceDep <- getData(name="GADM", country="FRA", level=2)
concordance <- dt0[indD, "Deux.roues"]; concordance
FranceDep$Deux.roues<- concordance
echelle <- c(seq(350,735000,by=7341)); echelle
couleurs <- colorRampPalette(c('white', 'blue','green','yellow', 'red'))
spplot(FranceDep, "Deux.roues", col.regions=couleurs(101),  at=echelle,
       main=list(cex=1.2, label=
                   "Mode de transport utilisé pour se rendre au travail 2016 - Deux roues"))

### ############### Marche.à.pied
concordance <- dt0[indD, "Marche.à.pied"]; concordance
FranceDep$Marche.à.pied<- concordance
echelle <- c(seq(350,735000,by=7341)); echelle
couleurs <- colorRampPalette(c('white', 'blue','green','yellow', 'red'))
spplot(FranceDep, "Marche.à.pied", col.regions=couleurs(101),  at=echelle,
       main=list(cex=1.2, label=
                   "Mode de transport utilisé pour se rendre au travail 2016 - Marche à pied"))

### ############### Pas.de.transport
concordance <- dt0[indD, "Pas.de.transport"]; concordance
FranceDep$Pas.de.transport<- concordance
echelle <- c(seq(350,735000,by=7341)); echelle
couleurs <- colorRampPalette(c('white', 'blue','green','yellow', 'red'))
spplot(FranceDep, "Pas.de.transport", col.regions=couleurs(101),  at=echelle,
       main=list(cex=1.2, label=
                   "Mode de transport utilisé pour se rendre au travail 2016 - Pas de transport"))

### ############### Transports.en.commun
concordance <- dt0[indD, "Transports.en.commun"]; concordance
FranceDep$Transports.en.commun<- concordance
echelle <- c(seq(350,735000,by=7341)); echelle
couleurs <- colorRampPalette(c('white', 'blue','green','yellow', 'red'))
spplot(FranceDep, "Transports.en.commun", col.regions=couleurs(101),  at=echelle,
       main=list(cex=1.2, label=
                   "Mode de transport utilisé pour se rendre au travail 2016 - Transports en commun"))

### ############### Voiture..camion..fourgonnette
concordance <- dt0[indD, "Voiture..camion..fourgonnette"]; concordance
FranceDep$Voiture..camion..fourgonnette<- concordance
echelle <- c(seq(350,735000,by=7341)); echelle
couleurs <- colorRampPalette(c('white', 'blue','green','yellow', 'red'))
spplot(FranceDep, "Voiture..camion..fourgonnette", col.regions=couleurs(101),  at=echelle,
       main=list(cex=1.2, label=
                   "Mode de transport utilisé pour se rendre au travail 2016 - Voiture, camion, fourgonnette"))


### ############### Part.des.ménages.ayant.au.moins.1.voiture.2016
dt0$Part.des.ménages.ayant.au.moins.1.voiture.2016
concordance <- dt0[indD, "Part.des.ménages.ayant.au.moins.1.voiture.2016"]; concordance
FranceDep$Part.des.ménages.ayant.au.moins.1.voiture.2016<- concordance
echelle <- c(35,84,7,86, 88,90,92, 100 ); echelle
couleurs <- colorRampPalette(c('white','tomato'))
spplot(FranceDep, "Part.des.ménages.ayant.au.moins.1.voiture.2016",
       col.regions=couleurs(10),  at=echelle, main=list(cex=1.2, label=
                                                          "Part des ménages ayant au moins 1 voiture (%) 2016 "))
