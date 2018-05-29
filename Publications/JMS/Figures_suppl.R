require(tidyverse)
require(FactoMineR)

load("dat.RData")
load("bdd_indic.RData")
par(mfrow=c(1,1))


### Illustration classif communale et choix indic

acp <- select(bdd_indic,starts_with("indic.")) %>% 
  na.omit() %>% 
  PCA(graph=F)

plot.PCA(acp,col.var = "blue",choix = "var",title = NA)

zonages$Typologie[zonages$zone_typo_commune==1] <- "Communes périurbaines, \ntaille des ménages importante"
zonages$Typologie[zonages$zone_typo_commune==2] <- "Communes peu denses, \nparc ancien et prix bas"
zonages$Typologie[zonages$zone_typo_commune==3] <- "Communes peu denses, \nforte vacance et prix bas"
zonages$Typologie[zonages$zone_typo_commune==4] <- "Communes urbaines,\nparc récent et prix élevés"
zonages$Typologie[zonages$zone_typo_commune==5] <- "Communes des espaces tourstiques,\nprix élevés"
zonages$Typologie[zonages$zone_typo_commune==6] <- "Communes peu denses,\nprix bas et turnover important"

map <- merge(mapCom,zonages,by.x="Codgeo",by.y="codgeo",all.x=T)
col.carte <- carto.pal("multi.pal",n1=6)
par(mar=c(0.5,0.5,0.5,0.5))
typoLayer(spdf=map,var="Typologie",border=NA,col=col.carte,legend.title.txt = "Typologie")


