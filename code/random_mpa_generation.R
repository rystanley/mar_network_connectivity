if(!require(NetworkConnectR)) devtools::install_github("remi-daigle/NetworkConnectR")
library(sf)
library(rnaturalearth)
library(tidyverse)

#turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
sf::sf_use_s2(FALSE)

#projections to use
planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load the network polygon
bioregion <- read_sf("data/shapefiles/MaritimesPlanningArea.shp")%>%st_transform(latlong)

coastal <- read_sf("data/shapefiles/coastal_planning_area.shp")%>%st_transform(latlong)

bathy <- read_sf("data/shapefiles/Countour_250.shp")%>%
  st_transform(latlong)%>%
  st_intersection(.,bioregion)

network_raw <- read_sf("data/shapefiles/networksites_proposed_OEM_MPA_v2.shp")

network <- network_raw%>%
  filter(!NAME %in% c("Lophelia Coral Conservation Area","Northeast Channel Coral Conservation Area"))%>%
  group_by(NAME)%>%
  summarise(geometry=st_union(geometry))%>% #the shape files has Western Emerald Bank as two sites
  left_join(.,network_raw%>%
              data.frame()%>%
              distinct(NAME,.keep_all=TRUE)%>%
              mutate(area=as.numeric(st_area(geometry)/1000/1000))%>% #recalculate area for the split areas
              dplyr::select(NAME,STATUS,area,TYPE))%>%
  mutate(species=NA)%>% #this is a dummy variable for ggplot long-form data assembly below
  dplyr::select(NAME,STATUS,TYPE,area,species,geometry)%>%st_transform(latlong)

# this of course can be any other grid, but this is for demonstration

ncells=100

grid <- st_make_grid(bioregion,n=ncells,square=FALSE) %>% 
  st_as_sf() %>% 
  st_filter(bioregion)

# stratify the grid
simplecoastal <- coastal %>% st_simplify(preserveTopology = TRUE,dTolerance = 0.01)

adj_edgelist <- st_intersects(grid,grid) %>% 
  as.data.frame() %>% 
  filter(row.id!=col.id)

grid_planar <- grid %>% st_transform(planar)
# 25 km buffer to start
buff_edgelist <- st_intersects(grid_planar,st_buffer(grid_planar,25)) %>% 
  as.data.frame() %>% 
  filter(row.id!=col.id)

# calculate the area in each mpa
areas <- as.numeric(st_area(network))



# test
# newnetwork <- rand_mpa(grid,areas,adj_edgelist)
# newnetwork <- rand_mpa(grid,areas,adj_edgelist,buff_edgelist)
newnetwork <- rand_mpa(grid,areas,adj_edgelist,buff_edgelist,max_edge=4)


ggplot(grid)+
  geom_sf()+
  geom_sf(data=newnetwork,aes(fill=mpa_id))


#### stratififed ####
#yes this is slow... and manually fixes Georges Basin
if(file.exists(paste0("data/stratified_grid_",ncells,".shp"))){grid_stratified <- st_read(paste0("data/stratified_grid_",ncells,".shp"))}else{
  
  grid_stratified <- grid %>% 
    mutate(coastal=lengths(st_intersects(.,simplecoastal))>0,
           bathy=lengths(st_intersects(.,bathy))>0,
           strata=case_when(coastal ~ "coastal",
                            !coastal&bathy ~ "mid-shore",
                            !coastal&!bathy ~ "offshore")) %>%
    mutate(strata=if_else(strata == "offshore" & # hard coded override for Georges Basin = mid-shore
                            st_coordinates(st_centroid(x))[,1]<(-66) &
                            st_coordinates(st_centroid(x))[,2]>42,
                          "mid-shore",
                          strata))
  
  #write it so you don't have to repeat
  st_write(grid_stratified,paste0("data/stratified_grid_",ncells,".shp"))
  
} #end if else 

#make grid stratified a polygon 
grid_stratified_group <- grid_stratified%>%
  group_by(strata)%>%
  summarise(geometry=st_union(geometry))%>%
  ungroup()


ggplot(grid_stratified)+
  geom_sf(aes(fill=strata))

# calculate the area in each mpa

network_cents <- network%>%
  st_centroid() #keep for plotting

network_cents_stratified <- network%>%
  st_centroid()%>%
  st_intersection(grid_stratified)

network_stratified <- network%>%
  left_join(.,network_cents_stratified%>%data.frame()%>%select(NAME,strata))%>%
  mutate(area=as.numeric(st_area(geometry)))


ggplot()+
  geom_sf(data=grid_stratified_group,aes(fill=strata))+
  geom_sf(data=grid_stratified%>%filter(strata=="mid-shore"),fill=NA)+ #show the grid here
  geom_sf(data=network_stratified,aes(fill=strata))+
  geom_sf(data=network_cents_stratified)+
  theme_bw()

areas <- network_stratified$area

newnetwork <- rand_stratified_mpa(grid,areas,grid_stratified$strata,network_stratified$strata,adj_edgelist,buff_edgelist,max_edge=4)

newnetwork_grouped <- newnetwork%>%
  group_by(mpa_id)%>%
  summarise(geometry=st_union(x))%>%
  ungroup()%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  arrange(area)%>%
  suppressMessages()

grid_grouped <- grid_stratified%>%
                group_by(strata)%>%
                summarise(geometry=st_union(geometry))%>%
                ungroup()

ggplot()+
  geom_sf(data=newnetwork_grouped,aes(fill=factor(mpa_id)))+
  geom_sf(data=grid_grouped,fill=NA)+
  theme_bw()+
  theme(legend.position='none')


ggplot(grid)+
  geom_sf()+
  geom_sf(data=newnetwork,aes(fill=mpa_id))
