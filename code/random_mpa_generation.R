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
grid <- st_make_grid(bioregion,n=100,square=FALSE) %>% 
  st_as_sf() %>% 
  st_filter(bioregion)

# stratify the grid
simplecoastal <- coastal %>% st_simplify(preserveTopology = TRUE,dTolerance = 0.01)

#yes this is slow... and manually fixes Georges Basin
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

ggplot(grid_stratified)+
  geom_sf(aes(fill=strata))

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

# grid is a grid covering your planning area
# areas is a vector of mpa area coverage in m^2
# adj_edgelist is the self-intersecting neighbours edgelist of grid, could be done 'in function' but that would slow it down. Used to create contiguous multi-grid cell MPA's.
# buff_edgelist is the intersecting edgelist of grid with a buffered grid, could be done 'in function' but that would slow it down. Used to guarantee minimum distances (buffer distance) between MPAs.
# max_edge is a numeric constraint that represents the largest number of protected neighbouring cells that an unprotected cell can have before it becomes protected itself if the MPA still needs to 'grow'.
rand_mpa <- function(grid,areas,adj_edgelist,buff_edgelist=adj_edgelist,max_edge=4){
  # find number of grid cells for each area
  cells <- floor(areas/as.numeric(st_area(grid[1,])))
  network <- NULL
  for(finalsize in cells){
    
    # create single grid cell mpa "kernel"
    size <- 1
    index_network <- buff_edgelist$row.id %in% network$grid_cell |
      buff_edgelist$row.id %in% buff_edgelist$row.id[buff_edgelist$col.id %in% network$grid_cell]
    id_network <- unique(buff_edgelist$row.id[index_network])
    outnetwork_adj <- adj_edgelist[!adj_edgelist$row.id %in% id_network,]
    
    # create MPA kernel (i.e. a random cell to start)
    mpa <- as.numeric(sample(buff_edgelist$row.id[!index_network],1))
    
    # grow the kernel if necessary
    while(size<finalsize){
      index_mpa <- outnetwork_adj$row.id %in% mpa & !outnetwork_adj$col.id %in% mpa
      
      # random growth or override?
      tab <- table(outnetwork_adj$col.id[index_mpa])
      if(max(tab)<max_edge){
        mpa <- c(mpa,
                 sample(outnetwork_adj$col.id[index_mpa],1))
      } else {
        # override
        mpa <- c(mpa,
                 as.numeric(sample(x=names(tab[tab==max(tab)])),1))
      }
      
      
      size <- length(mpa)
    }
    
    # warnings
    if(any(mpa %in% network$grid_cell)){
      warning("mpa is within network, this function is broken")
    }
    if(any(mpa %in% buff_edgelist$col.id[buff_edgelist$row.id %in% network$grid_cell])){
      warning("mpa is within buffer, this function is broken")
    }
    
    # create df with `mpa` and bind it to the network
    if(is.null(network)){
      mpadf <- data.frame(mpa_id=1,
                          grid_cell=mpa)
    } else {
      mpadf <- data.frame(mpa_id=max(network$mpa_id)+1,
                          grid_cell=mpa)
    }
    network <- dplyr::bind_rows(network,mpadf)
  }
  
  # join network with grid
  return(network %>% 
           left_join(grid %>% 
                       mutate(grid_cell=as.numeric(row.names(.))),by = "grid_cell") %>% 
           st_as_sf() 
  )
}


# test
# newnetwork <- rand_mpa(grid,areas,adj_edgelist)
# newnetwork <- rand_mpa(grid,areas,adj_edgelist,buff_edgelist)
newnetwork <- rand_mpa(grid,areas,adj_edgelist,buff_edgelist,max_edge=4)


ggplot(grid)+
  geom_sf()+
  geom_sf(data=newnetwork,aes(fill=mpa_id))
