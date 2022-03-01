### code to download the conservation objectives from open data and translate them into an sf '.rdata' file for later processing

#load libraries ----
    library(sf)
    library(httr)
    library(ckanr)
    library(dplyr)
    library(ggplot2)
    
    ckanr_setup(url="https://open.canada.ca/data")
    
    #turn off the sf use of s2 objects, which for some reason seems to propagate errors. 
    sf::sf_use_s2(FALSE)
    
#projections to use ----
    planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0" #UTM in 'km'
    latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#download the data, unzip, delete -----

    #this is the file path for the open-data entry of the conservation objectives and fisheries objectives
    url <-"https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Offshore_Ecological_Human_Use_MPA_Scotian%20Shelf/Download/MPA_NetworkDesign.gdb.zip"
    
    #download the file and save it as a temporary object ** takes a little while
    download.file(url, "data/temp.zip")
    
    #create directory to story the unzipped files to (note these are retained and not tracked by git)
    if(!dir.exists("data/conservation_objectives/")){dir.create("data/conservation_objectives/")}
    
    #unzip the file ** takes surprisingly long
    utils::unzip("data/temp.zip", exdir = "data/conservation_objectives")
    
    #clean up the zipped file that is no-longer needed
    file.remove("data/temp.zip")

#Read in the data and save as a shape file ----
    
    #get the file path for the .gdb file
    gdbDir <- list.files("data/conservation_objectives/", recursive=TRUE, pattern="\\.gdb$",
                         include.dirs = TRUE, full.names = TRUE)
    
    #get a list of layers within the .gdb files
    layers <- rgdal::ogrListLayers(gdbDir)
    
    #create a directory to put the converted shape files. 
    if(!dir.exists("data/shapefiles/COs/")){dir.create("data/shapefiles/COs/")}
    
    #Run a loop to open, transform and write as .shp
    for(i in layers){
      
      #poor mans progress message
      message("Working on '",i,"' : ",which(i == layers)," of ",length(layers)," Consevation Objectives")
      
      #read in shape file
      temp <- st_read(gdbDir, stringsAsFactors = FALSE, layer = i)%>%st_transform(latlong)
      
      #file name for the new shape file
      file_name <- paste0("data/shapefiles/COs/",i,".shp")
      
      #check to see if that files is already in the COs file. If so it will be overwritten based on the following logical
      delete_dsn_logical <- file.exists(file_name)
      
      #write the file ** Note that warnings are suppressed.They will tell you about abbreviate_shapefile_names() 
      #which has something to do with the ESRI driver and is not important for this analysis 
      st_write(temp,file_name,delete_dsn=delete_dsn_logical,quiet=TRUE)%>%suppressWarnings()
      
      rm(temp)
      
    }
    


#clean up the 