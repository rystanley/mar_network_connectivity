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

edgelist <- st_intersects(grid,grid) %>% 
  as.data.frame() %>% 
  filter(row.id!=col.id)

# calculate the area in each mpa
areas <- as.numeric(st_area(network))

# grid is a grid covering your planning area
# edgelist is the intersecting neighbours edgelist of grid, could be done 'in function' but that would slow it down
# areas is a vector of mpa area coverage in m^2
rand_mpa <- function(grid,edgelist,areas){
  # find number of grid cells for each area
  cells <- floor(areas/as.numeric(st_area(grid[1,])))
  network <- NULL
  for(finalsize in cells){
    
    # create single grid cell mpa "kernel"
    size <- 1
    index_network <- edgelist$row.id %in% network$grid_cell |
      edgelist$row.id %in% edgelist$row.id[edgelist$col.id %in% network$grid_cell]
    
    mpa <- as.numeric(sample(edgelist$row.id[!index_network],1))

    # grow the kernel if necessary
    while(size<finalsize){
      index_mpa <- edgelist$col.id %in% mpa 
      # edgelist$row.id[index_mpa]
      mpa <- c(mpa,
               sample(edgelist$row.id[index_mpa & !index_network],1))
      size <- length(mpa)
    }
    
    # warnings
    if(any(mpa %in% network$grid_cell)){
      warning("mpa is within network, this function is broken")
    }
    if(any(mpa %in% edgelist$row.id[edgelist$col.id %in% network$grid_cell])){
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
           st_as_sf() %>% 
           filter(-grid_cell)
           )
}


# test
newnetwork <- rand_mpa(grid,edgelist,areas)

ggplot(grid)+
  geom_sf()+
  geom_sf(data=newnetwork,aes(fill=mpa_id))
