library(sf)
library(dplyr)


#read in the shape files and convert to the geometry that we want. 
conserved_areas<- st_read("Cali_Geometry/conserved_areas_CA_NAT_RESOURCES/30x30_Conserved_Areas%2C_Terrestrial_(2023).shp")

conserved_areas<- st_as_sf(conserved_areas)

selected_columns <- conserved_areas %>%
  dplyr::select(MNG_AGNCY, OBJECTID, cpad_PARK_)

selected_columns$GEOMETRY<- NULL

#select only managers that have at least 20 parks 

#should maybe filter on parks of a certain size. 
managers_of_interest <- selected_columns %>%
  group_by(MNG_AGNCY) %>%
  summarise(count = n()) %>%
  filter(count > 50 & !grepl("city of", MNG_AGNCY, ignore.case = TRUE) & !grepl("unknown", MNG_AGNCY, ignore.case = TRUE))

#right now not filtering on size of conserved area, but might do that. 

#these managers of interest will be further filtered just for sake of time. 
#filtered to what I think are the most useful. 
#probably still too many. but we will try this. 
managers_of_interest<- managers_of_interest[c(4:12, 19, 34, 35, 36, 64, 65, 68:75, 78), ]

#join back into an SF object 
conserved_areas_of_interest<- st_as_sf(data.frame(left_join(managers_of_interest, conserved_areas, by = "MNG_AGNCY")))
#now need to intersect with the bird data. could either do this here, or in the for loop below to save space. 


#now need to either do a bunch of lapplys or make a giant for loop and do a bunch of lapplys within. 
st_write(conserved_areas_of_interest, "Cali_Geometry/conserved_areas_of_interest_variable_managers.shp")
list_management_groups<- unlist(data.frame(managers_of_interest)$MNG_AGNCY)


for(i in 1: length(list_management_groups))
{
  current_manager<- list_management_groups[i]
  trimmed_areas_of_interest<- conserved_areas_of_interest%>%
    filter(MNG_AGNCY == current_manager)
  
  
}
