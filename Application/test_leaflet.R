

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


# input$fact3 = fact
# input$lim3 = lim
fact <- "40"
lim <- "40000"

# "zone_inter_fact40_40000"
pal <- colorNumeric(
  palette = "Blues",
  domain = as.numeric(map@data$zone_inter_fact40_40000))

zzz <- paste("zone_inter_fact",ifelse(fact=="Optimal","",fact),"_",lim,sep="")
map <- lst_zon[[grep(zzz,names(lst_zon))]]
proj4string(map) <-"+proj=lcc +lat_1=44 +lat_2=49.00000000002 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
reproj <-  spTransform(map, "+init=epsg:4326")
leaflet() %>% addTiles( attribution = "SLC3") %>% 
  addPolygons(data = reproj, opacity = 3,   
              color = ~pal(as.numeric(zone_inter_fact40_40000)), 
              stroke = T, weight = 1)