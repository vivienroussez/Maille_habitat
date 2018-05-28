library(tidyverse)
rm(list = ls())

# obtenir zonages final (en ajoutant les îles)
load("bdd_calcul.RData")
load("com_zones.RData")

tab_var_zone <- merge(bdd_calcul_total, zonages) %>%
  mutate(nb_soc.15 = Part.social.15*P14_LOG,
         nb_soc.99 = Part.social.99*D99_LOG,
         nb_suroc15 = Part.suroccupation15*P14_LOG,
         nb_suroc07 = Part.suroccupation07*D99_LOG,
         nb_av_1949.15 = av_1949.15*P14_LOG,
         nb_av_1949.99 = av_1949.99*D99_LOG,
         nb_de_75_auj.15 = de_75_auj.15*P14_LOG,
         nb_de_75_auj.99 = de_75_auj.99*D99_LOG,
         duroc_tot15 = duroc_mean15*P14_LOG,
         duroc_tot99 = duroc_mean99*D99_LOG) 

# FONCTION ====

liste_zon <- names(tab_var_zone)[substr(names(tab_var_zone), 1, 4) == "zone"]
liste_var <- names(tab_var_zone)[substr(names(tab_var_zone), 1, 4) != "zone" & names(tab_var_zone) != "codgeo"]
nb_z <- length(liste_zon)
nb_v <- length(liste_var)


fonc <- function(vec, cod){ # fonction pour trouver le codgeo selon le maximum de Pop
  cod[which.max(vec)]
}

# agregation pour chaque zonages
crea_tableau_liste <- function(){
  tempo <- list()
  for (i in 1:nb_z){
    zon <- liste_zon[i]
    a <- aggregate.data.frame(tab_var_zone[,liste_var], list(zone = tab_var_zone[,zon]), sum)
    t <- group_by(tab_var_zone, tab_var_zone[,match(zon,names(tab_var_zone))]) %>%
      summarise(nom = fonc(P14_POP, codgeo))
    names(t)[1] <- "zone"
    a <- merge(a, t, by = "zone")  # Nommer zone par la commune la + peuplée de l'agrégation
    tempo <- c(tempo, list(a))
  }
  return(tempo)
}

res <- crea_tableau_liste()
names(res) <- liste_zon
# t <- res$zone_inter_fact_40000



agreg <- function(tab){  # fonction création des agrégations
  table <- mutate(tab,
                  indic.menag.14 = P14_PMEN/P14_RP,
                  indic.menag.99 = D99_PMEN/D99_RP,
                  evo.menag = (indic.menag.14 - indic.menag.99)/indic.menag.99,
                  indic.rs.14   = 100*P14_RSECOCC/P14_LOG,
                  indic.rs.99   = 100*D99_RSECOCC/D99_LOG,
                  evo.rs = (indic.rs.14 - indic.rs.99)/indic.rs.99,
                  indic.vac.14  = 100*P14_LOGVAC/P14_LOG,
                  indic.vac.99  = 100*D99_LOGVAC/D99_LOG,
                  evo.vac = (indic.vac.14 - indic.vac.99)/indic.vac.99,
                  indic.prix_rev.12 = 100*(prix_m212/revucm_med13),
                  indic.prix_rev.01 = 100*(prix_m200/revucm_med01),
                  evo.prix_rev = (indic.prix_rev.12 - indic.prix_rev.01)/indic.prix_rev.01,
                  indic.jeun.15 = 100*nb_de_75_auj.15/nb_av_1949.15,
                  indic.jeun.99 = 100*nb_de_75_auj.99/nb_av_1949.99,
                  evo.jeun = (indic.jeun.15 - indic.jeun.99)/indic.jeun.99,
                  indic.transac.12 = 100*compte_som12/P14_LOG,
                  indic.transac.08 = 100*compte_som08/P09_LOG,
                  evo.transac = (indic.transac.12 - indic.transac.08)/indic.transac.08,
                  indic.soc.15 = 100*nb_soc.15/P14_LOG,
                  indic.soc.99 = 100*nb_soc.99/D99_LOG,
                  evo.soc = (indic.soc.15 - indic.soc.99)/indic.soc.99,
                  indic.suroc.15 = 100*nb_suroc15/P14_LOG,
                  indic.suroc.07 = 100*nb_suroc07/P09_LOG,
                  evo.suroc = (indic.suroc.15 - indic.suroc.07)/indic.suroc.07,
                  indic.duroc.15 = duroc_tot15/P14_LOG,
                  indic.duroc.99 = duroc_tot99/D99_LOG,
                  evo.duroc = indic.duroc.15 - indic.duroc.99) %>%
    select(zone, nom, contains("indic."), starts_with("evo"), P14_POP, P14_LOG) %>%
    select(zone, nom, contains(".1"), starts_with("evo"), P14_POP, P14_LOG) # il faudrait faire un matches plutot...
  return(table)
}

liste_zonages <- lapply(res, FUN = agreg)
# t <- liste_zonages$zone_bassin_vie

# SAVE ----
save(liste_zonages, file = "liste_zonages.RData")


