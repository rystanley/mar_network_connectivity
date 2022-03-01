#Develop a plot of the regional network for the readme

#load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(patchwork)

#turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
sf::sf_use_s2(FALSE)

#projections to use
planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the network polygon
bioregion <- read_sf("data/shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)
bathy <- read_sf("data/shapefiles/Countour_250.shp")%>%
              st_transform(latlong)%>%
              st_intersection(.,bioregion)

network_raw <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")

network <- network_raw%>%
            group_by(NAME)%>%
            summarise(geometry=st_union(geometry))%>% #the shape files has Western Emerald Bank as two sites
            left_join(.,network_raw%>%
                        data.frame()%>%
                        distinct(NAME,.keep_all=TRUE)%>%
                        mutate(area=as.numeric(st_area(geometry)/1000/1000))%>% #recalculate area for the split areas
                        dplyr::select(NAME,STATUS,area,TYPE))%>%
            mutate(species=NA)%>% #this is a dummy variable for ggplot long-form data assembly below
            dplyr::select(NAME,STATUS,TYPE,area,species,geometry)%>%st_transform(latlong)

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

#sites by area with the median site size delimited
areadat <- network%>%
           data.frame()%>%
           dplyr::select(-geometry)%>%
           mutate(name_ord = factor(NAME,levels=network%>%arrange(area)%>%pull(NAME)))

area_sum <- data.frame(name_ord=c("Median","Mean"),area=c(median(areadat$area),mean(areadat$area)))%>%
            mutate(lab=paste0(name_ord," ",round(area,1)," km2"))

#bar plot visual
p2 <- ggplot(data=areadat)+
  geom_bar(stat="identity",aes(x=name_ord,y=area,fill=area),col="black")+
  geom_hline(data=area_sum,aes(yintercept=area,col=lab),lwd=1.5)+
  scale_y_log10(expand=c(0.01,0))+
  scale_fill_viridis()+
  labs(col="",fill=expression(paste("Area ",km^2)),x="",y="")+
  coord_flip()+
  theme_bw();p2

ggsave("output/surface_area_bar.png",p2,width=6,height=7,dpi=300,units="in")

#cumulative size frequency
p3 <- ggplot(data=areadat,aes(area))+
  stat_ecdf(geom = "step", pad = FALSE)+
  scale_x_log10()+
  geom_vline(data=area_sum,aes(xintercept=area,col=lab),lwd=1.5)+
  labs(x=expression(paste("Area ",km^2)),y="% total area",col="")+
  theme_bw()+
  theme(legend.position = c(0.1,0.9));p3

ggsave("output/area_cumfreq.png",p3,width=6,height=6,dpi=300,units="in")

#histogram ##surprise surprise looks like shit -- aslo I am making these quick
p4 <- ggplot(data=areadat,aes(area,fill=..x..),binwidth=50)+
  geom_histogram(col="black")+
  scale_fill_viridis()+
  scale_x_log10()+
  labs(x=expression(paste("Area ",km^2)),y="Frequency")+
  theme_bw()+
  theme(legend.position="none")+
  geom_vline(data=area_sum,aes(xintercept=area,col=lab),lwd=1.5);p4
  
ggsave("output/area_hist_crappy.png",p4,width=6,height=6,dpi=300,units="in")

group_plot <- p2+theme(legend.position="none",axis.text.y=element_blank(),panel.grid = element_blank())+scale_x_discrete(position="top")+labs(x=expression(paste("Draft site area ",km^2)))+
              p3+scale_y_continuous(position="right")+theme(legend.position = c(0.17,0.87),legend.background = element_blank())+
              p4+scale_y_continuous(position="right")+
              plot_layout(ncol=1)      

ggsave("output/grouped_area_diagnostics.png",group_plot,height=10,width=5,units="in",dpi=300)
