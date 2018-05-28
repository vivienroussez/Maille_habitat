# COMPILATION INDICATEURS

library(tidyverse)

rm(list = ls())
# setwd("M:/ETUDES/Maille habitat/Indicateurs")

recode_arrond <- function(depcom){
  codgeo <- as.character(depcom)
  codgeo[(substr(depcom,1,2)=="75")]<-c("75056")
  codgeo[(substr(depcom,1,4)=="6938")]<-c("69123")
  codgeo[(substr(depcom,1,3)=="132")]<-c("13055")
  return(codgeo)
}



# regroupement des données ----

# jeunesse ====

a15 <- read.csv("Indicateurs/data/ANCIEN15.csv", h = T, sep = ';', dec ='.', encoding = "latin1") %>%
  select(-starts_with("X")) %>%
  rename(av_1949 = Avant.1949, de_75_98 = De.1975.a.1998, de_49_74 = De.1949.a.1974, ap_99 = Après.1999) %>%
  mutate(ap_99 = ifelse(is.na(ap_99), 0, ap_99),
         de_75_auj = de_75_98 + ap_99)
a99 <- read.csv("Indicateurs/data/ANCIEN99.csv", h = T, sep = ';', dec = '.', encoding = "latin1") %>%
  select(-starts_with("X")) %>%
  mutate(Après.1999 = ifelse(is.na(Après.1999), 0, Après.1999)) %>%
  rename(av_1949 = Avant.1949, de_75_98 = De.1975.a.1998, de_49_74 = De.1949.a.1974, ap_99 = Après.1999) %>%
  mutate(ap_99 = ifelse(is.na(ap_99), 0, ap_99),  # pour éviter champ vide (nombreux)
         de_75_auj = de_75_98 + ap_99)

dat_a <- merge(a15, a99, suffixes = c(".15", ".99"), by = "codgeo")


# densite (dens) ====

RP <- read.csv("Indicateurs/data/RP_Hist.csv", sep = ';', dec = ',')

dat_b <- select(RP, codgeo = CODGEO, ends_with("LOGVAC"), ends_with("LOG"), ends_with("POP"), ends_with("RP"), ends_with("PMEN"), ends_with("RSECOCC"))
#

# duroc/transac ====
duroc <- read.csv("Indicateurs/data/DUROC.csv", dec = '.', sep = ';') %>%
  select(-starts_with("X")) 
transactions <- read.csv("Indicateurs/data/Transac.csv", sep = ";", dec = ",")

dat_c <- merge(duroc, transactions, by.x = "codgeo", by.y = "depcom", all.x = T) %>%
  select(codgeo, starts_with("duroc_mean"), starts_with("compte"))



# prix rev_ucm ====
not12 <- read.csv("Indicateurs/data/not12.csv", sep = ";", dec=",", header= TRUE) %>%
  mutate(codgeo = as.character(depcom), c_surf_ok_sum12 = as.integer(c_surf_ok_sum), prix_m212 = as.numeric(prix_m2))

not00 <- read.csv("Indicateurs/data/not00.csv", sep = ";", dec=",", header= TRUE) %>%
  mutate(codgeo = as.character(depcom), c_surf_ok_sum12 = as.integer(c_surf_ok_sum), prix_m200 = as.numeric(prix_m2))


filo_med12 <- read.csv("Indicateurs/data/REV_MED13.csv", sep = ";", header= TRUE) %>%
  rename(revucm_med13=revucm_me)

filo_med00 <- read.csv("Indicateurs/data/REV_MED01.csv", sep = ";", header= TRUE) %>%
  rename(revucm_med01=revucm_me)

dat_d <- merge(filo_med00, filo_med12) %>%
  merge(not12, all.x = TRUE, by = "codgeo") %>%
  merge(not00, all.x = TRUE, by = "codgeo") %>%
  select(codgeo, revucm_med13, revucm_med01, prix_m212, prix_m200)



# Social ====
soc <- read.csv("Indicateurs/data/SOCIAL.csv",sep=';',dec=".")

part_soc15 <- prop.table(xtabs(formula = nblog15~codgeo+statut,data = soc),margin = 1)[,3] %>% as.data.frame()
part_soc15$codgeo <- rownames(part_soc15)
names(part_soc15)[1] <- "Part.social.15"

part_soc99 <- prop.table(xtabs(formula = nblog99~codgeo+statut,data = soc),margin = 1)[,3] %>% as.data.frame()
part_soc99$codgeo <- rownames(part_soc99)
names(part_soc99)[1] <- "Part.social.99"

dat_e <- merge(part_soc15, part_soc99) %>%
  select(codgeo, Part.social.15, Part.social.99)



# suroccupation ====
suro <- read.csv("Indicateurs/data/SUROC.csv", sep = ';', dec = ".")

part_suroc15 <-   prop.table(xtabs(formula = nblog15~codgeo+surocle,data = suro),margin = 1)[,"O"] %>% 
  as.data.frame()
part_suroc15$codgeo <- rownames(part_suroc15)
names(part_suroc15)[1] <- "Part.suroccupation15"

part_suroc07 <-   prop.table(xtabs(formula = nblog07~codgeo+surocle,data = suro),margin = 1)[,"O"] %>% 
  as.data.frame()
part_suroc07$codgeo <- rownames(part_suroc07)
names(part_suroc07)[1] <- "Part.suroccupation07"


dat_g <- merge(part_suroc07, part_suroc15)



# TABLEAU ====
bdd_calcul <- merge(dat_a, dat_b, all.y = T) %>%  # dat_b : base avec le + de communes
  merge(dat_c, all.x = T) %>%
  merge(dat_d, all.x = T) %>%
  merge(dat_e, all.x = T) %>%
  merge(dat_g, all.x = T) %>%
  filter(substr(codgeo,1,2) != "97")

# setwd("M:/ETUDES/Maille habitat")
save(bdd_calcul, file = "bdd_calcul.RData")
rm(list = ls())
load("bdd_calcul.RData")



### Données géographiques pour l'imputation par les voisins ####
# /!\ à charger depuis 0_creation_geo
load("Geo/geo.RData")
source(file='M:/GEOGRAPHIE/GEO2016/LibEtFondsCartes.R' )
tableau <- bdd_calcul


s_ile <- sansile@data[,1:2] %>% rename(codgeo = Codgeo) 
tableau_s_ile <- merge(s_ile, bdd_calcul, by = "codgeo", all.x = T) %>% select(-Surface)
tableau_ile <- anti_join(bdd_calcul, s_ile, by = "codgeo") # data.frame avec les îles

row.names(tableau_s_ile) <- tableau_s_ile$codgeo


comm <- ordonne_comme_nb(nb = contig, dat = tableau_s_ile) # On met les données dans le même ordre que l'objet contig

impute_voisins <- function(x){
  a <- x
  test <- list()
  while (sum(is.na(a))>0) 
  {
    for (i in 1:length(contig))
    {
      test[[i]] <- a[contig[[i]]]
    }
    res <- unlist(lapply(test,mean,na.rm=T))
    print(paste(deparse(substitute(x)),"Nombre de valeurs encore manquantes : ",sum(is.na(res))))
    a[is.na(a)] <- res[is.na(a)]
  }
  return(a)
}

comm_imp <- as.data.frame(apply(tableau_s_ile[,-1], MARGIN = 2, FUN=impute_voisins))
comm_imp$codgeo <- row.names(comm_imp)

nom <- intersect(names(bdd_calcul), names(comm_imp))

bdd_calcul_total <- rbind(tableau_ile, comm_imp) # on retrouve les îles


bdd_calcul_total <- mutate(bdd_calcul_total,
                 nb_soc.15 = Part.social.15*P14_LOG,
                 nb_soc.99 = Part.social.99*D99_LOG,
                 nb_suroc15 = Part.suroccupation15*P14_LOG,
                 nb_suroc07 = Part.suroccupation07*D99_LOG,
                 nb_av_1949.15 = av_1949.15*P14_LOG,
                 nb_av_1949.99 = av_1949.99*D99_LOG,
                 nb_de_75_auj.15 = de_75_auj.15*P14_LOG,
                 nb_de_75_auj.99 = de_75_auj.99*D99_LOG,
                 duroc_tot15 = duroc_mean15*P14_LOG,
                 duroc_tot99 = duroc_mean99*D99_LOG) %>%
  arrange(codgeo)


save(bdd_calcul, bdd_calcul_total, file = "bdd_calcul.RData")



# CALCUL INDIC ----
rm(list=ls())
load("bdd_calcul.RData")

bdd_indic <- mutate(bdd_calcul_total,
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
  select(codgeo, contains("indic."), starts_with("evo"), P14_POP, P14_LOG) %>%
  select(codgeo, contains(".1"), starts_with("evo"), P14_POP, P14_LOG) # il faudrait faire un matches plutot...

save(bdd_indic, file = "bdd_indic.RData")
load("bdd_indic.RData")
