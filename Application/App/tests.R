require(tidyverse)
require(maptools)
require(sp)
require(cartography)



setwd("D:/Zone logement")
load("Zonages/Visuzon/dat.RData")
source("Zonages/Visuzon/fonctions.R")

lesquelles <- names(zonages) %>% grep(pattern = "zone")
var <- names(zonages)[lesquelles]
param <- gsub("zone_inter_","",gsub("fact","",var)) %>% strsplit("_")
fact <- sapply(param,function(x) as.numeric(x[1]))
lim <- sapply(param,function(x) as.numeric(x[2]))
dat <- merge(zonages,geo,by.x="codgeo",by.y="Codgeo") %>% rename(AU=au,EPCI=epci)
ind <- names(dat)[grep("indic",names(dat))]
fact <- as.character(fact) %>% unique()
fact[is.na(fact)] <- "Optim"
lim <- unique(lim)

input <- list(maille="EPCI",zone="200000420",nb_zones="Optim",lim_taille=50000)
#input <- list(maille="AU",zone="001",nb_zones="NA",lim_taille=20000)

sim <- ifelse(input$nb_zones=="Optim",paste("zone_inter_fact_",input$lim_taille,sep=""),
              paste("zone_inter_fact",input$nb_zones,"_",input$lim_taille,sep=""))

qvar <- grep(input$maille,names(dat))
d <- dat[dat[,qvar]==input$zone & !is.na(dat[,qvar]),] %>% droplevels()
# Sélection des zones logement qui correspondent au choix actif

quelles_zones <- unique(d[,sim]) %>% droplevels()
map <- lst_zon[[sim]] 
names(map)[1] <- "z"
map <- subset(map,z %in% quelles_zones)
map$z <- droplevels(map$z)

# Agregation des datnées de base pour représentation graphique

ag <- select(dat,sim,starts_with("indic."),P14_POP)
names(ag)[1] <- "sim"
#ag <- lapply(ag[,-1],FUN=function(x) aggregate(formula=x~ag[,1],FUN = median)) %>% as.data.frame()

ag <- ag %>% group_by(sim) %>% filter(sim %in% quelles_zones) %>%
  summarise(indic.anc=median(indic.anc) ,indic.jeun=median(indic.jeun),
            indic.pauv_anc=median(indic.pauv_anc) , indic.dens=median(indic.dens) ,
            indic.duroc=median(indic.duroc),indic.transac=median(indic.transac),
            indic.soc=median(indic.soc),indic.tens.soc=median(indic.tens.soc),
            indic.suroc=median(indic.suroc), indic.prix_rev=median(indic.prix_rev) ,
            indic.surface_pp=median(indic.surface_pp),indic.menag=median(indic.menag) ,
            indic.rs=median(indic.rs) ,indic.vac=median(indic.vac),
            indic.loyer_socle=median(indic.loyer_socle),indic.loyer_act=median(indic.loyer_act),
            indic.loyer_majore=median(indic.loyer_majore),pop=sum(P14_POP)) 
#names(ag)[1]<-sim
map <- merge(map,ag,by.x="z",by.y="sim") %>% subset()

plot(map)

input <- list(maille="dep",zone="01",nb_zones="Optim",lim_taille=50000)

front <- SelectionContour(maille = input$maille,zone = input$zone,lst = lst_mailles)
map <- SelectionZon(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
                    zone = input$zone,maille = input$maille,don=dat,lst=lst_zon)
plot(map)
plot(front,col="blue",add=T)
