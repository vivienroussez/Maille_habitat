require(tidyverse)
require(cartogram)
require(sp)
require(maptools)
require(FactoMineR)

# setwd("E:/Maille habitat/")
rm(list = ls())
load("Zonages_inter.RData")
load("liste_zonages.RData") # provient de 3_agregation
load("bdd_indic.RData") # provient de 1_creation_bdd

choix <- "zone_inter_fact40_40000"  #### A changer pour analyser une autre version !!!
map <- lst_zon[[grep(choix,names(lst_zon))]]
ag <- liste_zonages[[grep(choix,names(liste_zonages))]]
map <- merge(map,ag,by.x=choix,by.y="zone")

# anamorphose ----
ana <- cartogram(map,"P14_LOG",itermax = 3) ## On fait un fonds anamorphosé en fonction du nombre de logements

save(ana, bdd_calcul_total, bdd_indic, zones_ile_f, liste_zonages, zonages, file = "Diffusion.RData")
rm(list = ls())
load("Diffusion.RData")
