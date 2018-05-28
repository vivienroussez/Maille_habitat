# Fonction pour sélectionner les données à partir de la sélection courante

SelectionCom <- function(nb_zones,lim_taille,maille,zone,don=dat,spdf=sansile)
{
  sim <- ifelse(nb_zones=="Optim",paste("zone_inter_fact_",lim_taille,sep=""),
                paste("zone_inter_fact",nb_zones,"_",lim_taille,sep=""))
  # extraction des données selon la zone à façon (maille + zone)
  # quelle variable 
  qvar <- grep(maille,names(don))
  d <- don[don[,qvar]==zone & !is.na(don[,qvar]),] %>% droplevels()
  com <- subset(spdf,Codgeo %in% d$codgeo,select=Codgeo) %>% merge(d[,c("codgeo","classe")],by.x="Codgeo",by.y="codgeo")
  return(com)
}

# 
# nb_zones <- input$nb_zones
# lim_taille <- input$lim_taille
# zone <- input$zone
# maille <- input$maille
# don <- dat
# lst <- lst_zon

SelectionZon <- function(nb_zones,lim_taille,maille,zone,don=dat,lst=lst_zon)
{
  sim <- paste("zone_inter_fact",ifelse(nb_zones=="Optimal","",nb_zones),"_",lim_taille,sep="")
  # extraction des données selon la zone à façon (maille + zone)
  # quelle variable 
  qvar <- grep(maille,names(don))
  d <- don[don[,qvar]==zone & !is.na(don[,qvar]),] %>% droplevels()
  # Sélection des zones logement qui correspondent au choix actif
  quelles_zones <- unique(d[,sim]) %>% droplevels()
  map <- lst[[sim]] 
  names(map)[1] <- "z"
  map <- subset(map,z %in% quelles_zones)
  map$z <- droplevels(map$z)
  
  # Agregation des données de base pour représentation graphique
  
  ag <- select(don,sim,starts_with("indic."),P14_POP)
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
              indic.loyer_rsa=median(indic.loyer_rsa),pop=sum(P14_POP)) 
  #names(ag)[1]<-sim
  map <- merge(map,ag,by.x="z",by.y="sim")
  return(map)
}



SelectionContour <- function(maille,zone,lst=lst_mailles)
{
  map <- lst[[maille]]
  names(map)[1] <- "m"
  map <- subset(map,m==zone)
  map <- as(map,"SpatialLines")
  return(map)
}
  
