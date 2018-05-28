
library(shiny)
require(cartography)
require(tidyverse)
require(sp)
require(leaflet)

#setwd("D:/Zone logement/Zonages/Visuzon_reg/")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  load("dat.RData")
  source("fonctions.R")
  lesquelles <- names(zonages) %>% grep(pattern = "zone")
  var <- names(zonages)[lesquelles]
  param <- gsub("zone_inter_","",gsub("fact","",var)) %>% strsplit("_")
  fact <- sapply(param,function(x) as.numeric(x[1]))
  lim <- sapply(param,function(x) as.numeric(x[2]))
  zon <- names(lst_mailles)
  dat <- merge(zonages,geo,by.x="codgeo",by.y="Codgeo") %>% rename(AU=au,EPCI=epci)
  geo <- rename(geo,AU=au,EPCI=epci)
  ind <- names(dat)[grep("indic",names(dat))]
  fact <- as.character(fact) %>% unique()
  fact[is.na(fact)] <- "Optimal"
  lim <- unique(lim)
  
  observe({
    m <- levels(geo[,input$maille])
    updateSelectInput(session=session,choices = m,inputId = "zone")
    updateSelectInput(session = session,choices = fact,inputId = "nb_zones")
    updateSelectInput(session = session,choices = lim,inputId = "lim_taille")
    updateSelectInput(session = session,choices = ind,inputId = "indic1")
    updateSelectInput(session = session,choices = ind,inputId = "indic2",selected = "indic.prix_rev")
    
  })
  
  # On récupère la couche spatiale et les données qui correspondent au choix actif
  # et on les mets dans un reactive (vont servir pour plusieurs plots)
  #currentsel <- reactive(updateSelCom(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
  #                                    zone = input$zone,maille = input$maille))
  
  #observeEvent(input$maille,{currentsel$com <- updateSelCom()})
  
  # Cartes
  
  output$plotclass <- renderPlot({ ## Carte de la typo sur la sélection
    com <- SelectionCom(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
                        zone = input$zone,maille = input$maille,don=dat,spdf=sansile)
    front <- SelectionContour(maille = input$maille,lst = lst_mailles,zone = input$zone)
    typoLayer(spdf=com,spdfid="Codgeo",var="classe",border=NA,legend.pos = "topright")
    plot(front,col="red",add=T)
    layoutLayer(title = "Classification sur la maille sélectionnée",col="white",
                coltitle = "black",sources = "",author = "")
  })
  
  output$popPlot <- renderPlot({ ## Carte du zonage avec population
    map <- SelectionZon(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
                        zone = input$zone,maille = input$maille,don=dat,lst=lst_zon)
    front <- SelectionContour(maille = input$maille,lst = lst_mailles,zone = input$zone)
    
    plot(map)
    propSymbolsLayer(spdf=map,spdfid = "z",var="pop",add = T,inches = .2,
                     legend.title.txt = "Population",legend.pos = "topright")
    plot(front,col="red",add=T)
    layoutLayer(title = "Population par zone",col="white",
                coltitle = "black",sources = "",author = "")
  })
  
  output$var1 <- renderPlot({
    map <- SelectionZon(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
                        zone = input$zone,maille = input$maille,don=dat,lst=lst_zon)
    front <- SelectionContour(maille = input$maille,lst = lst_mailles,zone = input$zone)
    
    choroLayer(spdf=map,spdfid="z",var=input$indic1,legend.pos = "topright")
    plot(front,col="red",add=T)
    layoutLayer(title = paste("Indicateur",input$indic1),col="white",
                coltitle = "black",sources = "",author = "")
    
  })
  
  output$var2 <- renderPlot({
    map <- SelectionZon(nb_zones = input$nb_zones,lim_taille = input$lim_taille,
                        zone = input$zone,maille = input$maille,don=dat,lst=lst_zon)
    front <- SelectionContour(maille = input$maille,lst = lst_mailles,zone = input$zone)
    choroLayer(spdf=map,spdfid="z",var=input$indic2,legend.pos = "topright")
    plot(front,col="red",add=T)
    layoutLayer(title = paste("Indicateur",input$indic2),col="white",
                coltitle = "black",sources = "",author = "")
    
  })
  
  
  
  #### Pour le deuxième tab
  
  output$lst_var <- renderUI({
    selectizeInput(inputId = "var",choices=ind,label="Choix de la variable à représenter")
  })
  
  output$lst_fact1 <- renderUI({
    selectizeInput(inputId = "fact1",choices=fact,
                   label="Nombre moyen de communes par zone",selected="100")
  })
  
  output$lst_lim1 <- renderUI({
    selectizeInput(inputId = "lim1",choices=lim,label="Population minimale")
  })
  
  output$lst_fact2 <- renderUI({
    selectizeInput(inputId = "fact2",choices=fact,
                   label="Nombre moyen de communes par zone",selected="100")
  })
  
  output$lst_lim2 <- renderUI({
    selectizeInput(inputId = "lim2",choices=lim,label="Population minimale")
  })
  
  output$lst_fact3 <- renderUI({
    selectizeInput(inputId = "fact3",choices = fact,
                   label = "Nombre moyen de communes par zone", selected = "100")
  })
  
  output$lst_lim3 <- renderUI({
    selectizeInput(inputId = "lim3", choices = lim, label = "Population minimale")
  })
  
  output$lst_zon3 <- renderUI({
    selectizeInput(inputId = "zon3", choices = zon, label = "Zonage à représenter")
  })
  
  output$map1 <- renderPlot({
    zzz <- paste("zone_inter_fact",ifelse(input$fact1=="Optimal","",input$fact1),"_",input$lim1,sep="")
    map <- lst_zon[[grep(zzz,names(lst_zon))]]
    don <- select(dat,zzz,input$var) 
    names(don) <- c("zone","indic")
    don <- group_by(don,zone) %>%  summarise(res=median(indic))
    map <- merge(map,don,by.x=zzz,by.y="zone",all.x=T)
    choroLayer(spdf=map,spdfid = zzz,var="res",border=NA,nclass=8,
               legend.title.txt = input$var)
  })
  
  nombre <- function(fact,lim)
  {
    zzz <- paste("zone_inter_fact",ifelse(fact=="Optimal","",fact),"_",lim,sep="")
    map <- lst_zon[[grep(zzz,names(lst_zon))]]
    return(paste("Nombre total de zones :",nrow(map)))
  }
  
  output$comptage1 <- renderText({nombre(fact=input$fact1,lim=input$lim1) })
  
  output$comptage2 <- renderText({nombre(fact=input$fact2,lim=input$lim2)})
  
  output$map2 <- renderPlot({
    zzz <- paste("zone_inter_fact",ifelse(input$fact2=="Optimal","",input$fact2),"_",input$lim2,sep="")
    map <- lst_zon[[grep(zzz,names(lst_zon))]]
    don <- select(dat,zzz,input$var) 
    names(don) <- c("zone","indic")
    don <- group_by(don,zone) %>%  summarise(res=median(indic))
    map <- merge(map,don,by.x=zzz,by.y="zone",all.x=T)
    choroLayer(spdf=map,spdfid = zzz,var="res",border=NA,nclass=8,
               legend.title.txt = input$var)
  })
  
  # output$mailledyn <- renderLeaflet({
  #   zzz <- paste("zone_inter_fact",ifelse(input$fact3=="Optimal","",input$fact3),"_",input$lim3,sep="")
  #   map <- lst_zon[[grep(zzz,names(lst_zon))]]
  #   proj4string(map) <-"+proj=lcc +lat_1=44 +lat_2=49.00000000002 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
  #   reproj <-  spTransform(map, "+init=epsg:4326")
  #   leaflet() %>% addTiles( attribution = "SLC3") %>% 
  #     addPolygons(data = reproj, opacity = 3,   color = "black", stroke = TRUE, weight = 1, popup = reproj$zone_inter_fact90_50000, 
  #                 fill = T, fillColor = "#444444", fillOpacity = 0.2,
  #                 highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                     bringToFront = TRUE))
  # })
  
  output$mailledyn <- renderLeaflet({
    zzz <- paste("zone_inter_fact",ifelse(input$fact3=="Optimal","",input$fact3),"_",input$lim3,sep="")
    map <- lst_zon[[grep(zzz,names(lst_zon))]]
    proj4string(map) <-"+proj=lcc +lat_1=44 +lat_2=49.00000000002 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
    reproj <-  spTransform(map, "+init=epsg:4326")
    leaflet() %>% addTiles( attribution = "SLC3") %>% 
      addPolygons(data = reproj, opacity = 3,   color = zzz, stroke = TRUE, weight = 1, popup = reproj$zone_inter_fact90_50000, 
                  fill = T, fillColor = "#444444", fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
  
  
  output$maillestat <- renderPlot({
    zzz <- paste("zone_inter_fact",ifelse(input$fact3=="Optimal", "", input$fact3), "_", input$lim3, sep="")
    map <- lst_zon[[grep(zzz, names(lst_zon))]]
    plot(map, border="lightgrey")
  })
  
  output$comptage3 <- renderText({nombre(fact=input$fact3,lim=input$lim3)})
  
  
})
