#this is a file analogous to bird_ranges

#step 1: already generated in the range_gen script: plants that have ranges that interesect California? 

#what about non-native plants? what did we end up deciding? 


#here I will try to get a list of all california natives (like in birds)

#this part can be done on the local. 
install.packages("rvest")
install.packages("httr")
library(httr)
library(rvest)
library(dplyr)
library(sf)
library(ggplot2)

# cannot find a amphibians checklist so will work directly with the range data. 

#CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE MIGHT HAVE BETTER RANGE MAPS!
amphibians_ranges<- st_read("amphibians/AMPHIBIANS")

sf_use_s2(FALSE)
amphibians_ranges<- st_make_valid(amphibians_ranges)

california<- st_read("Cali_geometry/ca-state-boundary/CA_State_TIGER2016.shp")

california<- st_transform(california, crs = "WGS84")

#there are only 46 amphibians that intersect with California? probably less actually. 

amphibians_intersect_california<- st_intersection(amphibians_ranges, california)
unique(amphibians_intersect_california$sci_name)

p<- ggplot() +
  geom_sf(data = california)+
  geom_sf(data = amphibians_intersect_california[3,], fill = "red")

#plotted and spot checked a few species, and it looks good with the ranges. 


#get all the amphibians ranges and their intersection with california. 
#will not cross reference this with any kind of list because I can't find one. 


############
#Now: We have a list of amphibians that overlap with california but also that exist in the california species checklist of native plants. 
#it looks like a bunch of subspecies were removed.
#SWITCH BACK TO LOCAL FOR ECOREGIONS STUFF!
############

#from California
#apply the ecoregion thresholding thing. 
#these are all the california species (only about 4,000 of them)
install.packages("lwgeom")
library(lwgeom)
amphibians_intersect_california$range_area<- st_area(amphibians_intersect_california)

st_write(amphibians_intersect_california, "amphibians/full_california_amphibians_ranges.shp")
ecoregions<- st_read("Cali_Geometry/ca_eco_l3/ca_eco_l3.shp")


ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.02*ecoregions$area #can always amend this minimum threshold by just accessing the area. 



#TODO: preprocess the ecoregion data so that the range is not included if the ecoregion area in a certain range is below a threshold. 
#require that at least 5% of the ecoregion be covered in order for it to be included in the range. 

#for real. this will take forever to run. 
#running this again to force myself to have a break. 
#saving ecoregions_plant_data_intersection just in case. 
#this should assign species with particular ranges to each ecoregion. HOWEVER, it might only preserve the ecoregion gometry not the actual species range geometry!

#FOR AMPHIBIANS: will not filter further. will allow ecoregions to be specific. 
ecoregions_amphibians_data_filtered<- st_intersection(ecoregions, amphibians_intersect_california)

#do a test:
ecoregions_amphibians_data_filtered
plot(ecoregions_amphibians_data_filtered[100,])
#get the areas of all the intersections 
#started at 7:59 pm 
ecoregions_amphibians_data_filtered$eco_area<- st_area(ecoregions_amphibians_data_filtered)
#filter the data so that the overlapped area is greater than the minimum threshold. 
#the geometry of this is: for each tuple species i, ecoregion j, the geometry is the overlap of species 
# i only in ecoregion j. 
#note: each species is uniuqe and only has one geometry. 
ecoregions_amphibians_data_filtered<- ecoregions_amphibians_data_filtered%>%
  filter(eco_area>= min_thresh | eco_area >= 0.10*range_area)


ecoregion_codes<- ecoregions$US_L3CODE

#honestly I don't know, I need to go to sleep. 

#do not need to rerun this. already have all the ecoregion species stuff. 
#ecoregion bird data is already merged! This is basically good to go for the entire ecoregion. but also need to do the other thing with the ecoregions (hex data stuff)
#these shape files are good for the entire overlapping species list for a region, need to be able to populate hex data as well.

#edited this to only select some fields, but actually only edited it for one ecoregion. 
for(code in ecoregion_codes) {
  folder_path <- file.path("amphibians/ecoregion_data", code)
  
  # Check if folder exists, if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Created folder:", folder_path, "\n")
  }
  
  #also selects the geometry. 
  temp <- ecoregions_amphibians_data_filtered %>%
    filter(US_L3CODE == code) %>%
    dplyr::select(sci_name, US_L3CODE, US_L3NAME, L1_KEY)
  
  filepath_shape <- file.path("amphibians/ecoregion_data", code, "ecoregion.shp")
  filepath_checklist <- file.path("amphibians/ecoregion_data", code, "checklist.csv")
  
  # Check if shapefile already exists
  if(file.exists(filepath_shape)) {
    print(paste("Shapefile", filepath_shape, "already exists. Deleting..."))
    file.remove(filepath_shape)
  }
  
  sf::st_write(temp, filepath_shape)
  print(unique(temp$sci_name))
  write.csv(unique(temp$sci_name), file = filepath_checklist)
}

#by ecoregion it's almost too small. Might have to figure out a way to deal with this properly. 
#in this file I have now generated all the amphibians ranges. However, the checklists all have * * instead of *_* which might become an issue.  