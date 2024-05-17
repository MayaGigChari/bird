library(sf)
library(dplyr)


#accidentally replaced all cl's with ec's 
#read in the shape files and convert to the geometry that we want. 
conserved_areas<- st_read("Cali_Geometry/conserved_areas_CA_NAT_RESOURCES/30x30_Conserved_Areas%2C_Terrestrial_(2023).shp")

conserved_areas<- st_as_sf(conserved_areas)

selected_columns <- conserved_areas %>%
  dplyr::select(MNG_AGNCY, OBJECTID, cpad_PARK_)

selected_columns$geometry<- NULL

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

managers_of_interest<- managers_of_interest[-c(4,5,6,17,18), ]

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



uc_reserve<- conserved_areas_overlapping_polygons%>%
  filter(MNG_AGNCY == "University of California")

uc_polygons<- unique(uc_reserve$h3_index)

uc_polygons<- polygons%>%
  filter(h3_index %in%uc_polygons)

conserved_areas_consolidated_geometries<- conserved_areas_of_interest %>%
  group_by(UNIT_NAME, MNG_AGNCY) %>% 
  summarize(geometry = st_union(geometry)) #group the collective geometries together for each individual reserve. 

conserved_areas_consolidated_geometries<- st_make_valid(conserved_areas_consolidated_geometries)
#want to intersect the conserved areas with the hexagons so that I can color 


#try to intersect with just conserved_areas_of_interest instead of the consolidated geometries because something is wrong with those geometries. 
conserved_areas_overlapping_polygons<- st_intersection(conserved_areas_of_interest, polygons)
#want to make a plot of the california entire ecoregions with the conserved areas overlayed. 

#want to extract all the h3_indexes that are in this dataset. 

polygons_to_highlight<- unique(conserved_areas_overlapping_polygons$h3_index)


polygons_to_highlight<- polygons%>%
  filter(h3_index %in%polygons_to_highlight)
#this will keep all conserved areas and also will return the polygons and maybe the polygon geometry but probably not. 

final_join_plot_CA<- final_join %>%
  dplyr::select(Clade_sum_pdSigCl,Clade_sum_mpdSigCl,Clade_sum_mntdSigCl,Clade_sum_pdSigEc,Clade_sum_mpdSigEc,Clade_sum_mntdSigEc, bird_h3_indx, bird_geometry)
#now need to plot 

final_join_plot_CA<- st_as_sf(final_join_plot)

final_join_plot_CA<- drop_na(final_join_plot)

plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal() +    # Ensure equal aspect ratio
  geom_sf(data = final_join_plot_CA, aes(fill = Clade_sum_mntdSigEc, color = Clade_sum_mntdSigEc)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, 
                       name = "Cumulative Significance", 
                       breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                       labels = c("-5", "", "", "", "", "0", "", "", "", "", "5"),
                       limits = c(-5, 5)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, 
                        name = "Cumulative Significance", 
                        breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                        labels = c("-5", "", "", "", "", "0", "", "", "", "", "5"),
                        limits = c(-5, 5))+
  geom_sf(data = uc_polygons, color = "green",  fill = "green", alpha = 0.5)

# Output the plot
ggsave("Reserve_images/uc_polygons_mntd_ec.png", plot_area, width = 10, height = 10, dpi = 300)

#just for uc reserves 

#want to plot just the reserves by management agency. 

plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal() +    # Ensure equal aspect ratio
  geom_sf(data = conserved_areas_consolidated_geometries, aes(fill = MNG_AGNCY)) +
  geom_sf(data = ecoregions, alpha = 0.1)

ggsave("Reserve_images/reserves_labeled_by_management.png", plot_area, width = 10, height = 10, dpi = 300)

  
#below: will do some kind of processing to count the number of polygons per each management agency that has a pd above average. 

#maybe we want to join the two tables by h3_index first (the reserve table with overlapping hexagons and the other one. just a left join)

conserved_areas_overlapping_polygons_df<- data.frame(conserved_areas_overlapping_polygons)
final_join_plot_CA_df<- data.frame(final_join_plot_CA)

total_hexagonal_info<- data.frame(left_join(conserved_areas_overlapping_polygons, final_join_plot_CA_df, join_by("h3_index" =="bird_h3_indx")))

#now want to group by each management agency and count the number of significantly high, significantly low and zero hexagons overlapping. 

total_hexgonal_info_props <- total_hexagonal_info %>%
  group_by(MNG_AGNCY) %>%
  summarize(
    pd_low_Cl = sum(clade_sum_pdSigCl == -1, na.rm = TRUE) / n(),
    pd_high_Cl = sum(clade_sum_pdSigCl == 1, na.rm = TRUE) / n(), 
    pd_norm_Cl = sum(clade_sum_pdSigCl == 0, na.rm = TRUE) / n(),
    mpd_low_Cl = sum(clade_sum_mpdSigCl == -1, na.rm = TRUE) / n(),
    mpd_high_Cl = sum(clade_sum_mpdSigCl == 1, na.rm = TRUE) / n(),
    mpd_norm_Cl = sum(clade_sum_mpdSigCl == 0, na.rm = TRUE) / n(),
    mntd_low_Cl = sum(clade_sum_mntdSigCl == -1, na.rm = TRUE) / n(),
    mntd_high_Cl = sum(clade_sum_mntdSigCl == 1, na.rm = TRUE) / n(),
    mntd_norm_Cl = sum(clade_sum_mntdSigCl == 0, na.rm = TRUE) / n(),
    pd_low_Ec = sum(clade_sum_pdSigEc == -1, na.rm = TRUE) / n(),
    pd_high_Ec = sum(clade_sum_pdSigEc == 1, na.rm = TRUE) / n(),
    pd_norm_Ec = sum(clade_sum_pdSigEc == 0, na.rm = TRUE) / n(),
    mpd_low_Ec = sum(clade_sum_mpdSigEc == -1, na.rm = TRUE) / n(),
    mpd_high_Ec = sum(clade_sum_mpdSigEc == 1, na.rm = TRUE) / n(),
    mpd_norm_Ec = sum(clade_sum_mpdSigEc == 0, na.rm = TRUE) / n(),
    mntd_low_Ec = sum(clade_sum_mntdSigEc == -1, na.rm = TRUE) / n(),
    mntd_high_Ec = sum(clade_sum_mntdSigEc == 1, na.rm = TRUE) / n(),
    mntd_norm_Ec = sum(clade_sum_mntdSigEc == 0, na.rm = TRUE) / n()
  )

#make graphs for all the ecoregion stuff (bar graphs)

#extract just the ecoregions
ecoregion_total_hexagonal_info_props<- total_hexgonal_info_props[c(1, 9, 12, 15)]

library(ggplot2)

# Melt the data frame to long format for easier plotting
melted_data <- tidyr::pivot_longer(ecoregion_total_hexagonal_info_props, cols = -MNG_AGNCY)

# Create bar plots using ggplot2 and facet_wrap
melted_data <- tidyr::pivot_longer(total_hexgonal_info_sums, cols = -MNG_AGNCY)

# Modify names for better presentation
melted_data$name <- gsub("_.*", "", melted_data$name)

# Create bar plots using ggplot2 and facet_wrap
ggplot(melted_data, aes(x = MNG_AGNCY, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ name, scales = "free_y", ncol = 3, labeller = labeller(name = c(mntd = "MNTD", mpd = "MPD", pd = "PD"))) +
  labs(title = "Proportion of hexagons covered with significantly high Phylodiversity across management agencies", 
       y = "Proportion",
       x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

ggsave("Reserve_images/reserve_high_histogram.png", width = 11, height = 8.5, units = "in")



#next want to make a plot of all areas that don't have a reserve yet. 

final_join_plot_CA_no_reserve<- final_join_plot_CA%>%
  filter(!bird_h3_indx %in%polygons_to_highlight$h3_index)

#want to plot california, then plot all these things. 

plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal() +    # Ensure equal aspect ratio
  geom_sf(data = ecoregions, alpha = 0.4, fill = "green")+
  geom_sf(data = final_join_plot_CA_no_reserve, aes(fill = clade_sum_mntdSigEc, color = clade_sum_mntdSigEc)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, 
                       name = "Cumulative Significance", 
                       breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                       labels = c("-5", "", "", "", "", "0", "", "", "", "", "5"),
                       limits = c(-5, 5)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, 
                        name = "Cumulative Significance", 
                        breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
                        labels = c("-5", "", "", "", "", "0", "", "", "", "", "5"),
                        limits = c(-5, 5))


# Output the plot
ggsave("Reserve_images/unoccupied_polygons_mntd_Ec.png", plot_area, width = 10, height = 10, dpi = 300)

  