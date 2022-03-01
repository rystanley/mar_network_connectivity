#Develop a plot of the regional network for the readme

#load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

#turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
sf::sf_use_s2(FALSE)

#projections to use
planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the network polygon
bioregion <- read_sf("data/shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)
network <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")%>%st_transform(latlong)
bathy <- read_sf("data/shapefiles/Countour_250.shp")%>%
              st_transform(latlong)%>%
              st_intersection(.,bioregion)

#basemap
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong))%>%
  st_intersection(.,bioregion%>%st_transform(latlong)%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box

#Network plot
p1 <- ggplot()+
      geom_sf(data=network,fill="grey90",col="black")+
      geom_sf(data=bioregion,col="black",fill=NA)+
      geom_sf(data=basemap,fill="darkolivegreen3")+
      geom_sf(data=network,col="black",fill=NA)+
      geom_sf(data=bathy,fill=NA)+
      theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank());p1
  
#save the plots
ggsave("inst/SSBoF_newtork.png",p1,width=6,height=6,dpi=300,units="in")
