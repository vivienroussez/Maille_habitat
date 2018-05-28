# CHARGEMENT ----
require(maptools)
require(spdep)
library(tidyverse)

rm(list=ls())
# /!\ aller chercher directement dans le save de 0_creation_geo
load("Geo/geo.RData")
# mapCom <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/COMMUNES_METRO.shp")
# contig <- poly2nb(mapCom, row.names = mapCom$Codgeo, queen = T)

# creation tableau iles ----
t <- n.comp.nb(contig_tot)
table(t$comp.id) 

lesquelles <- which(t$comp.id > 2)
taille <- t$comp.id %>% table() %>% length()

# mapCom$Codgeo[lesquelles] %>% length()
# coordinates(mapCom)


# affectation ----
# créée à la main selon les communes/zones auxquelles rattacher chaque commune (voir "à la main" en dessous)
vec <- c("Z_IA", "Z_IB", "Z_IC", "Z_ID", "22210", "56003", "29177", "85234", "17168", "56162", 
         "29177", "29168", "29239", "56152", "56008", "56152")

tab_ile <- as.data.frame(x = c())
for(i in 3:taille){ # on exclu 1 et 2 (métropole + Corse)
  lesquelles <- which(t$comp.id == i)
  com <- mapCom$Codgeo[lesquelles] %>% as.character()
  tab_ileb <- as.data.frame(x = com) %>%
    mutate(zone = vec[i-2])
  tab_ile <- rbind(tab_ile, tab_ileb)
} 


# création zonages finales avec îles inclues ----

load("Zonages_inter.RData")


# ajout différents zonages administratifs (EPCI, Zone d'emploi, Bassin de vie) ----
zon_admin <- read.csv2("Zonages/zon_admin.csv") %>%
  select(codgeo = CODGEO, zone_epci = EPCI, zone_emp = ZE2010, zone_bassin_vie = BV2012)

zones <- mutate(zonages, zone_typo_commune = as.factor(classe)) %>%
  merge(zon_admin) %>%
  select(codgeo, starts_with("zone"))

liste_zon <- names(zones)[substr(names(zones),1,4) == "zone"]
liste_zon_sans_typo <- liste_zon[!(liste_zon %in% c("zone_typo_commune", "zone_epci", "zone_emp", "zone_bassin_vie"))]
# col <- which(names(sum_zon) == liste_zon[1])
col_tot <- which(names(zones) %in% liste_zon_sans_typo) # on ne veut pas remplacer typo


# iles pluri communales (zone "Z") ----
tab_ile1 <- filter(tab_ile, substr(zone, 1, 1) == "Z") %>% mutate(com = as.character(com)) %>% arrange(com)
liste_ile_plu <- pull(tab_ile1, com)
t1 <- filter(zones, codgeo %in% liste_ile_plu) %>% mutate(codgeo = as.character(codgeo)) %>% arrange(codgeo) 
t1[,col_tot] <- tab_ile1$zone  # modifier code pour enlever crochet ?
t1b <- filter(zones, !codgeo %in% liste_ile_plu) %>% arrange(codgeo)
zones_ile1 <- rbind(t1, t1b)


# liste ile mono commune ----
tab_ile2 <- filter(tab_ile, substr(zone, 1, 1) != "Z") %>% mutate(com = as.character(com)) %>% arrange(com)
liste_ile_mono <- pull(tab_ile2, com)

t2 <- merge(tab_ile2, zones_ile1, by.x = "zone", by.y = "codgeo") %>%
  select(codgeo = com, starts_with("zone_"))

zonages <- rbind(zones_ile1, t2) %>% arrange(codgeo)

# SAVE ----
save(zonages, file = "com_zones.RData")
rm(list = ls())
load("com_zones.RData")

#
# DETAILS DES ILES (à la main) ----
lesquelles <- which(t$comp.id == 3)  # Oléron : zone
# 17140 17485 17486 17323 17385 17411 17337 17093
mapCom$Codgeo[lesquelles]


lesquelles <- which(t$comp.id == 4)  # Noirmoutier : zone
# 85163 85083 85011 85106
mapCom$Codgeo[lesquelles]

lesquelles <- which(t$comp.id == 5)  # Ré : zone
# 17207 17360 17019 17121 17051 17297 17318 17161 17286 17369
mapCom$Codgeo[lesquelles]

lesquelles <- which(t$comp.id == 6)  # Belle-Île-en-Mer : zone
# 56114 56152 56009 56241
mapCom$Codgeo[lesquelles]

lesquelles <- which(t$comp.id == 7)  # Bréhat : SEULE !!
mapCom$Codgeo[lesquelles]
# 22016
# à rapprocher de Ploubazlanec (22210)

lesquelles <- which(t$comp.id == 8)  # Arz : SEULE !!
mapCom$Codgeo[lesquelles]
# 56088
# à rapprocher de Arradon : 56003
# à côté d'une autre île... (Ile aux Moines, 56087)

lesquelles <- which(t$comp.id == 9)  # Ouessant : SEULE !!
mapCom$Codgeo[lesquelles]
# 29155
# à rapprocher de Plouarzel : 29177

lesquelles <- which(t$comp.id == 10)  # ile d'Yeu : SEULE !!
mapCom$Codgeo[lesquelles]
# 85113
# à rapprocher de Saint Jean des Monts (85234)

lesquelles <- which(t$comp.id == 11)  # Ile d'Aix : SEULE !!
mapCom$Codgeo[lesquelles]
# 17004
# à rapprocher de Fouras (17168)

lesquelles <- which(t$comp.id == 12)  # Groix : SEULE !!
mapCom$Codgeo[lesquelles]
# 56069
# à rapprocher de Ploemeur (56162)

lesquelles <- which(t$comp.id == 13)  # Ile Molène : SEULE !!
mapCom$Codgeo[lesquelles]
# 29084
# à rapprocher de Plouarzel : 29177

lesquelles <- which(t$comp.id == 14)  # Ile de Sein : SEULE !!
mapCom$Codgeo[lesquelles]
# 29083
# à rapprocher de Plogoff (29168)

lesquelles <- which(t$comp.id == 15)  # Ile de Batz : SEULE !!
mapCom$Codgeo[lesquelles]
# 29082
# à rapprocher de Roscoff (29239)

lesquelles <- which(t$comp.id == 16)  # Ile d'Houat : SEULE !!
mapCom$Codgeo[lesquelles]
# 56086
# à rapprocher de Le Palais : 56152

lesquelles <- which(t$comp.id == 17)  # Ile au Moine : SEULE !!
mapCom$Codgeo[lesquelles]
# 56087
# à rapprocher de Baden : 56008

lesquelles <- which(t$comp.id == 18)  # Hoedic : SEULE !!
mapCom$Codgeo[lesquelles]
# 56085
# à rapprocher de Le Palais : 56152

