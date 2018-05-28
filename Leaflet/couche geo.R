require(tidyverse)
require(foreach)
require(doParallel)
require(maptools)

# setwd("M:/ETUDES/Zonage logement/")
#setwd("D:/Zone logement/")

# load("com_zones.RData")
# load("../Indicateurs/data/Base.RData")
# load("Zonages/Zonages_reg.RData")

######################################################################
### Ajout des variables correspondant aux simulations interrégionales
######################################################################
imp <- dir("Zonages/CSV")
imp <- imp[substr(imp,1,6)=="ZInter"]
sim <- gsub( "ZInter_","",gsub(".csv","",imp) )
zon <- list()

for (i in 1:length(imp)){
  zon[[i]] <- read.csv(file=paste("Zonages/CSV/",imp[i],sep=""), sep=";", dec=',')
  #names(zon)[i]<-imp[i]
  names(zon[[i]])[1] <- "codgeo" 
  names(zon[[i]])[2] <- paste("zone","_inter_",sim[i],sep="")
}  

# # déjà fait ?
# zonages <- merge(zonages,zon[[1]],by.x="codgeo",by.y="codgeo",all.x=T)
# for (i in 2:length(zon))
# {
#   zonages <- merge(zonages,zon[[i]],by.x="codgeo",by.y="codgeo",all.x=T)
# }
# zonages %>% head()
rm(i,imp,sim)

spdf_zonage <- merge(sansile,zonages,by.x="Codgeo",by.y="codgeo")

lesquelles <- names(spdf_zonage) %>% grep(pattern = "inter") ### A changer selon la version de zonage 
### (reg ou interReg) qu'on veut traiter
var <- names(spdf_zonage)[lesquelles]



fusion_spdf<- function(spdf,crit,nom){
  aa <- unionSpatialPolygons(spdf,crit)
  df <- data.frame(zone=row.names(aa))
  names(df) <- nom
  row.names(df)<-row.names(aa)
  aa <- SpatialPolygonsDataFrame(aa,df)
  return(aa)
}

# On crée toutes les couches correspondantes à chaque simul, en parallèle pour gagner du temps
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,require(maptools))
clusterEvalQ(cl,"fusion_spdf")
registerDoParallel(cl)
system.time({
  lst_zon <- foreach(i=1:length(var)) %dopar% fusion_spdf(spdf_zonage,spdf_zonage@data[,var[i]],nom=var[i])
})
stopCluster(cl)

# system.time({
# lst_zon2 <- list()
# for (i in 1:length(var)) { lst_zon2[[i]] <- fusion_spdf(spdf=spdf_zonage,crit=spdf_zonage@data[,var[i]],nom=var[i])}
# })

names(lst_zon) <- var

##############################################################################
### On crée des couches pour chaque maille de restitution (pour appli shiny)

#load("Zonages/Visuzon/dat.RData")

geo <- select(sansile@data,-Surface,-cat_au,-ur)
geo <- lapply(geo,as.factor) %>% as.data.frame()

dep <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/DEP.shp") 
dep@data <- data.frame(dep=dep@data[,"Num_dep"]) 
reg2016 <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/REG.shp")
reg2016@data <- data.frame(reg2016=reg2016@data[,"Num_reg"])
AU <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/AU.shp")
AU@data <- data.frame(AU=AU@data$au)
ze <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/ZE.shp")
ze@data <- data.frame(ze=ze@data$ze)
bv <- readShapePoly("M:/GEOGRAPHIE/GEO2016/Couches/BV.shp")
bv@data <- data.frame(bv=bv@data$bv)
EPCI <- unionSpatialPolygons(sansile,sansile@data$epci)
d <- data.frame(EPCI=row.names(EPCI))
row.names(d) <- d$EPCI
EPCI <- SpatialPolygonsDataFrame(EPCI,d)

reg <- unionSpatialPolygons(sansile,sansile@data$reg)
d <- data.frame(reg=row.names(reg))
row.names(d)<-d$reg
reg <- SpatialPolygonsDataFrame(reg,d)

lst_mailles <- list("dep"=dep,"AU"=AU,"EPCI"=EPCI,"reg"=reg,"reg2016"=reg2016,"ze"=ze,"bv"=bv)
zonages <- select(zonages,-starts_with("zone_R"))
save(zonages,lst_zon,geo,lst_mailles,sansile,file="Zonages/Visuzon/dat.RData")
save(zonages,lst_zon,geo,lst_mailles,sansile,file="Zonages/VisuMaille/dat.RData")

