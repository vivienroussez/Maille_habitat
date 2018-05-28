require(tidyverse)
require(spdep)
require(FactoMineR)
require(maptools)
#require(RWeka)  # Pour algorithme XMeans 

# install.packages("RWeka")
# WPM("install-package","XMeans") n'a pas fonctionné :(

rm(list = ls())
source(file='M:/GEOGRAPHIE/GEO2016/LibEtFondsCartes.R' )
source(file ="M:/INFORMATIQUE/Fonctions basiques.R" )

mapCom <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/COMMUNES_METRO.shp")

# géographie de F. Janvier, mais certains soucis sur les îles : on reprend la couche d'origine
# mapCom <- readShapePoly("Geo/Data/com_16.shp")
# mapCom@data$Codgeo <- mapCom@data$INSEE_COM # pour être raccord sur le code fait depuis le début
 
row.names(mapCom) <- as.character(mapCom@data$Codgeo)


mapCom@data$reg2016[mapCom@data$Codgeo %in% c("65476","65160","65422","65185","65292")] <- "75"
mapCom@data$reg2016[mapCom@data$Codgeo %in% c("84150","84138","84053","84097")] <- "84"
mapCom@data$reg[mapCom@data$Codgeo %in% c("65476","65160","65422","65185","65292")] <- "72"
mapCom@data$reg[mapCom@data$Codgeo %in% c("84150","84138","84053","84097")] <- "82"


# Création de la matrice de contiguité
contig_tot <- poly2nb(mapCom,row.names = mapCom@data$Codgeo,queen = T)
# On retire les communes-iles pour la suite (probleme dans traitement de la contiguite)
iles <- attr(contig_tot, "region.id")[which(card(contig_tot) == 0)]
sansile <- subset(mapCom, ! Codgeo %in% iles)
contig <- poly2nb(sansile, row.names = sansile@data$Codgeo, queen = T) 


save(contig_tot, contig, sansile, mapCom, file = "Geo/geo.RData")
#load("Base.RData")
