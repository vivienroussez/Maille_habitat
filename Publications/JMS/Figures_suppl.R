require(tidyverse)
require(FactoMineR)

setwd("M:/ETUDES/Maille_habitat/")

load("M:/GEOGRAPHIE/GEO2016/Habillage_FR.RData")
load("dat.RData")
load("bdd_indic.RData")
par(mfrow=c(1,1))


### Illustration indic communaux

don <- merge(mapCom,bdd_indic,by.x="Codgeo",by.y="codgeo",all.x=T)
par(mfrow=c(1,2),mar=c(1,1,1,1))

choroLayer(spdf=don,var="indic.prix_rev.12",border=NA,legend.title.txt = "Prix du m2 rapporté \n au revenu médian",nclass = 8,
           legend.pos = "topleft",legend.values.rnd = 1)

choroLayer(spdf=don,var="indic.menag.14",border=NA,legend.title.txt = "Nombre de personnes \n par ménages",nclass = 8,
           legend.pos = "topleft",legend.values.rnd = 1)

### Illustration classif communale et choix indic
par(mfrow=c(1,1))
acp <- select(bdd_indic,starts_with("indic.")) %>% 
  na.omit() %>% 
  PCA(graph=F)

plot.PCA(acp,col.var = "blue",choix = "var",title = NA)

zonages$Typologie[zonages$zone_typo_commune==1] <- "1 - Communes périurbaines, \ntaille des ménages importante"
zonages$Typologie[zonages$zone_typo_commune==2] <- "2 - Communes peu denses, \nparc ancien et prix bas"
zonages$Typologie[zonages$zone_typo_commune==3] <- "3 - Communes peu denses, \nforte vacance et prix bas"
zonages$Typologie[zonages$zone_typo_commune==4] <- "4 - Communes urbaines,\nparc récent et prix élevés"
zonages$Typologie[zonages$zone_typo_commune==5] <- "5 - Communes des espaces tourstiques,\nprix élevés"
zonages$Typologie[zonages$zone_typo_commune==6] <- "6 - Communes peu denses,\nprix bas et turnover important"

map <- merge(mapCom,zonages,by.x="Codgeo",by.y="codgeo",all.x=T)
col.carte <- brewer.pal(n = 6,name =  "Dark2")

par(mar=c(0.5,0.5,0.5,0.5))
plot(map,border=NA)
plot(st_geometry(nuts),col="lightgrey",border="white",add=T)
typoLayer(spdf=map,var="Typologie",border=NA,col=col.carte,legend.title.txt = "Typologie",add=T)
plot(st_geometry(st_centroid(labels)),add=T,pch=15)
labelLayer(x=decale,txt="libgeo")

###############################################################
## Pour la suite, Executer Synth.Rmd ou/et Manuel.Rmd avant
###############################################################


par(mfrow=c(1,2),mar=c(2,2,2,2))
choroLayer(spdf=map,var="indic.prix_rev.12",border=NA,legend.title.txt = "Prix du m2 rapporté \n au revenu médian",nclass = 8,
           legend.pos = "topleft",legend.values.rnd = 1)

choroLayer(spdf=ana,var="indic.menag.14",col = carto.pal(n1=8,"blue.pal"),nclass = 8,border=NA,
           legend.title.txt = "Nombre de personnes \n par ménages",legend.pos = "topleft",legend.values.rnd = 2)



## Heatmap pour la typo
num <- select_if(m@data,is.numeric) %>% scale() %>% as.data.frame()
don_cl <- data.frame(classe=paste("Classe",m@data$typo_maille),num) %>% 
  select(indic.menag.14,indic.rs.14,indic.vac.14,indic.prix_rev.12,indic.jeun.15,
         indic.transac.12,
         indic.soc.15,indic.suroc.15,indic.duroc.15,classe)
# On renomme les variables pour faire un joli graphique
names(don_cl)[1:9] <- c("Nb pers par ménages","Part résid. secondaires","Part log vacants",
                        "Prix log rapportés aux revenus","Indice jeunesse",
                        "Taux transactions",
                        "Part log sociaux","Suroccupation","Durée occupation")
don_cl <- group_by(don_cl,classe)  %>%
  summarise_if(is.numeric,median,na.rm=T) %>%
  gather(-classe,key="var",value="Ecart")
ggplot(don_cl,aes(classe,var,fill=Ecart)) + geom_tile() + 
  scale_fill_gradient2(low="red",high="blue") + xlab("Catégorie") + ylab("Indicateur")


## Carte de la typo
par(mfrow=c(1,1))
cols <- brewer.pal(n = 6,name =  "Dark2")
ordre <- unique(m$typo_maille) %>% as.integer()
colMap <- cols[ordre]
plot(map,border=NA)
plot(st_geometry(nuts),col="lightgrey",border="white",add=T)
typoLayer(spdf = map,var="lib_typo_maille",border=NA,col = colMap,legend.title.txt = "Type de maille",
          legend.pos = "bottomleft",add=T)
plot(st_geometry(st_centroid(labels)),add=T,pch=15)
labelLayer(x=decale,txt="libgeo")

## Score de diversité
num <- select(bdd_indic,starts_with("indic.")) %>%
  mutate(indic.transac.12=ifelse(is.infinite(indic.transac.12),NA,indic.transac.12)) %>% 
  mutate_all(funs((.-mean(.,na.rm=T))/sd(.,na.rm=T)))

cohes <- data.frame(codgeo=bdd_indic$codgeo,maille=zonages[,choix],num) %>%
  merge(cl,by.x="maille",by.y=choix) %>%
  group_by(maille) %>%
  summarise_if(is.numeric,function(x) sd(x,na.rm = T))
cohes$score <- apply(cohes[,-1],MARGIN = 1,sum)
mapCohes <- merge(map,cohes,by.x=choix,by.y="maille",all.x=T)
choroLayer(spdf=mapCohes,var = "score",border=NA,nclass = 8,legend.values.rnd = 1,
           legend.title.txt = "Score de diversité",legend.pos = "topleft")
habille()

# Corrélation sotock et évol
a <- select(m@data,indic.menag.14,indic.rs.14,indic.vac.14,indic.prix_rev.12,indic.jeun.15,
            indic.transac.12,
            indic.soc.15,indic.suroc.15,indic.duroc.15)
# On renomme les variables pour faire un joli graphique
names(a)[1:9] <- c("Nb pers par ménages","Part résid. secondaires","Part log vacants",
                   "Prix log rapportés aux revenus","Indice jeunesse",
                   "Taux transactions",
                   "Part log sociaux","Suroccupation","Durée occupation")

b <- select(m@data,evo.menag,evo.rs,evo.vac,evo.prix_rev,evo.jeun,
            evo.transac,
            evo.soc,evo.suroc,evo.duroc)
names(b)[1:9] <- c("Nb pers ménage","Part résid. secondaires","Part log vacants",
                   "Prix rapp. revenus","Indice jeunesse",
                   "Taux transactions",
                   "Part log sociaux","Suroccupation","Dur. occupation")

cc  <-  cor(a,b,use = "complete.obs") 
cc  <- data.frame(Stocks=rownames(cc),cc) %>%
  gather(key="Evolutions",value="Correlation",-Stocks)
ggplot(cc,aes(Stocks,Evolutions,fill=Correlation)) + geom_tile() + 
  scale_fill_gradient2(low="red",high="blue") + xlab("Stocks") + 
  ylab("Evolutions") + theme(axis.text.x = element_text(angle=90))


# Maille en version leaflet

leaflet() %>% addTiles( attribution = "SLC3") %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(data = reproj, opacity = 3,   color = "black", stroke = TRUE, weight = 1, popup = reproj@data[,choix],
              fill = T, fillColor = "#444444", fillOpacity = 0.2,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

pop <- read.csv2("Publications/Data/RP_Hist.csv") %>% 
  merge(zonages[,c("codgeo",choix)],by.x="CODGEO",by.y="codgeo",all.y=T) %>%
  merge(cl,by.x=choix,by.y=choix) %>%
  group_by(lib_typo_maille) %>%
  summarise(pop14 = sum(P14_POP),pop99=sum(D99_POP)) %>%
  mutate(evol_pop=100*((pop14/pop99)^(1/15)-1),Classe=lib_typo_maille)
ggplot(data=pop,aes(x=Classe,weight=evol_pop,fill=Classe)) + geom_bar() +
  ylab("Evolution de la population") + scale_fill_manual(values=cols) +
  theme(axis.text.x=element_blank(),axis.title.x=element_blank())
