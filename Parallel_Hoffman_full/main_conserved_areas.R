library(sf)
library(dplyr)
#this is for plotting everything? I don't know. 

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


conserved_areas_of_interest<- st_read("Cali_Geometry/conserved_areas_of_interest_variable_managers.shp")

unique(conserved_areas_of_interest$MNG_AGNCY)

conserved_areas_of_interest<- st_as_sf(data.frame(left_join(managers_of_interest, conserved_areas, by = "MNG_AGNCY")))


####STEP 2: Make the geometries valid
#geometrcy is not valid. need to make geometry valid and change to WGS 84 as the 
sf_use_s2(FALSE)
st_is_valid(conserved_areas_of_interest)
conserved_areas_of_interest<- st_transform(conserved_areas_of_interest, "WGS84")
conserved_areas_of_interest<- st_make_valid(conserved_areas_of_interest)



conserved_areas_consolidated_geometries<- conserved_areas_of_interest %>%
  group_by(UNIT_NAME, MNG_AGNCY) %>% 
  summarize(geometry = st_union(geometry)) #group the collective geometries together for each individual reserve. 


mng_agencies<- unique(conserved_areas_consolidated_geometries$MNG_AGNCY)

final_join_plot_CA<- final_join %>%
  dplyr::select(clade_sum_pdSigCl,clade_sum_mpdSigCl,clade_sum_mntdSigCl,clade_sum_pdSigEc,clade_sum_mpdSigEc,clade_sum_mntdSigEc, bird_h3_indx, bird_geometry)
#now need to plot 

final_join_plot_CA<- st_as_sf(final_join_plot)

final_join_plot_CA<- drop_na(final_join_plot)


for(i in mng_agencies)
{
  reserve<- conserved_areas_consolidated_geometries %>% 
    filter(MNG_AGNCY == i)
  
  plot_area <- ggplot() +
    theme_void() +  # Remove default axes and background
    coord_equal() +    # Ensure equal aspect ratio
    geom_sf(data = final_join_plot, aes(fill = clade_sum_pdSigCl)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, 
                         name = "Significance", 
                         breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                         labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5"))+
    geom_sf(data = reserve,  alpha = 0.4, color = "green")
  
  # Output the plot
  ggsave(paste("Reserve_images/", i, "_pd_map_Ecol.png"), plot_area, width = 10, height = 10, dpi = 300)
  
}
#want to make a plot of the california entire ecoregions with the conserved areas overlayed. 


#For the reserves: want to identify hexagons that overlap with/touch each reserve 
#want to also do other stuff with the hexagons. 
