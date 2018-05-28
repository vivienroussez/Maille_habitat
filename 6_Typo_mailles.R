require(tidyverse)
require(cartogram)
require(sp)
require(maptools)
require(FactoMineR)

# setwd("E:/Maille habitat/")

load("dat.RData")
choix <- "zone_inter_fact40_40000"  #### A changer pour analyser une autre version !!!
map <- lst_zon[[grep(choix,names(lst_zon))]]
ag <- liste_zonages[[grep(choix,names(liste_zonages))]]
map <- merge(map,ag,by.x=choix,by.y="zone")

#ana <- cartogram(map,"P14_LOG",itermax = 3) ## On fait un fonds anamorphosé en fonction du nombre de logements
don <- select(map@data,indic.menag.14,indic.rs.14,indic.vac.14,indic.prix_rev.12,
              indic.jeun.15,indic.transac.12,indic.soc.15,indic.suroc.15,indic.duroc.15,
              P14_LOG,P14_POP)
acp <- PCA(don,graph = F,ncp = 5,quanti.sup = c(10,11))
hc <- HCPC(acp,nb.clust = 6,consol = T,graph = F)
cl <- data.frame(zone=map[,choix],clust=hc$data.clust[,"clust"])

names(cl)[2] <- "typo_maille"
zonages <- merge(zonages,cl,by=choix,all.x=T)

save(liste_zonages,lst_zon,zonages,geo,lst_mailles,mapCom,file = "dat.RData")

