#this is a file that concatenates all the data that we have seen so far and produces a color scaled map of california 
#so far: based on taxa of birds, plants, mammals, squamates and butterflies. Amphibian tree is too small
#need to go through individual stuff and redo everythign?? hopefully no. 
#THIS IS IN THE RANGE_CONCATENATION FILE!

library(sf)
library(dplyr)


#read in all the ranges. 

bird_final_data<- data.frame(st_read("birds/final_output.shp"))
plant_final_data<- data.frame(st_read("Plants/final_output.shp"))
squamate_final_data<- data.frame(st_read("squamate/final_output.shp"))
mammal_final_data<- data.frame(st_read("Mammals/final_output.shp"))
butterfly_final_data<- data.frame(st_read("butterflies/final_output.shp"))


#need to now filter for multiple ecoregions. 


bird_final_data<- unique(handle_ecoregions(bird_final_data)%>%
  dplyr::select(h3_indx, pd_vals, mpd_vls, mntd_vl, tree_sz, mssn___, prprtn_, pdSigCl, mpdSigCl, mntdSigCl, pdSigEc, mpdSgEc, mntdSgE))

plant_final_data<- unique(handle_ecoregions(plant_final_data)%>%
  dplyr::select(h3_indx, pd_vals, mpd_vls, mntd_vl, tree_sz, mssn___, prprtn_, pdSigCl, mpdSigCl, mntdSigCl, pdSigEc, mpdSgEc, mntdSgE))

squamate_final_data<- unique(handle_ecoregions(squamate_final_data)%>%
  dplyr::select(h3_indx, pd_vals, mpd_vls, mntd_vl, tree_sz, mssn___, prprtn_, pdSigCl, mpdSigCl, mntdSigCl, pdSigEc, mpdSgEc, mntdSgE))

mammal_final_data<- unique(handle_ecoregions(mammal_final_data)%>%
  dplyr::select(h3_indx, pd_vals, mpd_vls, mntd_vl, tree_sz, mssn___, prprtn_, pdSigCl, mpdSigCl, mntdSigCl, pdSigEc, mpdSgEc, mntdSgE))

butterfly_final_data<- unique(handle_ecoregions(butterfly_final_data)%>%
  dplyr::select(h3_indx, pd_vals, mpd_vls, mntd_vl, tree_sz, mssn___, prprtn_, pdSigCl, mpdSigCl, mntdSigCl, pdSigEc, mpdSgEc, mntdSgE))

bird_final_data<- data.frame(bird_final_data)
plant_final_data<- data.frame(plant_final_data)
squamate_final_ldata<- data.frame(squamate_final_data)
mammal_final_data<- data.frame(mammal_final_data)
butterfly_final_data<- data.frame(butterfly_final_data)



colnames(bird_final_data) <- paste("bird" ,colnames(bird_final_data),sep="_") 
colnames(plant_final_data) <- paste("plant" ,colnames(plant_final_data),sep="_") 
colnames(squamate_final_data) <- paste("squamate" ,colnames(squamate_final_data),sep="_") 
colnames(mammal_final_data) <- paste("mammal" ,colnames(mammal_final_data),sep="_") 
colnames(butterfly_final_data) <- paste("butterfly" ,colnames(butterfly_final_data),sep="_") 

join_1<- inner_join(bird_final_data, plant_final_data, join_by("bird_h3_indx"== "plant_h3_indx"))
join_2<- inner_join(join_1, squamate_final_data, join_by("bird_h3_indx"== "squamate_h3_indx"))
join_3<- inner_join(join_2, mammal_final_data, join_by("bird_h3_indx"== "mammal_h3_indx"))
final_join<- inner_join(join_3, butterfly_final_data, join_by("bird_h3_indx"== "butterfly_h3_indx"))

Clade_sums_pdSigCl<- final_join$bird_pdSigCl + final_join$plant_pdSigCl + final_join$mammal_pdSigCl + final_join$squamate_pdSigCl + final_join$butterfly_pdSigCl
Clade_sums_mpdSigCl<- final_join$bird_mpdSigCl + final_join$plant_mpdSigCl + final_join$mammal_mpdSigCl + final_join$squamate_mpdSigCl + final_join$butterfly_mpdSigCl
Clade_sums_mntdSigCl<- final_join$bird_mntdSigCl + final_join$plant_mntdSigCl + final_join$mammal_mntdSigCl + final_join$squamate_mntdSigCl + final_join$butterfly_mntdSigCl


Clade_sums_pdSigEc<- final_join$bird_pdSigEc + final_join$plant_pdSigEc + final_join$mammal_pdSigEc + final_join$squamate_pdSigEc + final_join$butterfly_pdSigEc
Clade_sums_mpdSigEc<- final_join$bird_mpdSgEc + final_join$plant_mpdSgEc + final_join$mammal_mpdSgEc + final_join$squamate_mpdSgEc + final_join$butterfly_mpdSgEc
Clade_sums_mntdSigEc<- final_join$bird_mntdSgE + final_join$plant_mntdSgE + final_join$mammal_mntdSgE + final_join$squamate_mntdSgE + final_join$butterfly_mntdSgE

final_join$clade_sum_pdSigCl<- Clade_sums_pdSigCl
final_join$clade_sum_mpdSigCl<- Clade_sums_mpdSigCl
final_join$clade_sum_mntdSigCl<- Clade_sums_mntdSigCl
final_join$clade_sum_pdSigEc<- Clade_sums_pdSigEc
final_join$clade_sum_mpdSigEc<-Clade_sums_mpdSigEc
final_join$clade_sum_mntdSigEc<-Clade_sums_mntdSigEc

final_join_plot<- final_join %>%
  dplyr::select(clade_sum_pdSigCl,clade_sum_mpdSigCl,clade_sum_mntdSigCl,clade_sum_pdSigEc,clade_sum_mpdSigEc,clade_sum_mntdSigEc, bird_h3_indx, bird_geometry)
#now need to plot 

final_join_plot<- st_as_sf(final_join_plot)

final_join_plot<- drop_na(final_join_plot)

library(ggplot2)

# Create the plot
plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal() +    # Ensure equal aspect ratio
  geom_sf(data = final_join_plot, aes(fill = clade_sum_pdSigCl, color = clade_sum_pdSigCl)) +
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
                        limits = c(-5, 5)) + 
  geom_sf(data = ecoregions, alpha = 0)

# Output the plot
print(plot_area)
ggsave("final_pd_Cal_whitespace.png", plot_area, width = 10, height = 10, dpi = 300)

saveRDS(final_join, "final_join_data_with_sums_whitespace")


#now going to try to read in the shape files for all the reserves and plot them. 


#plot of hexagons and california ecoregions. 

plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal() +    # Ensure equal aspect ratio
  geom_sf(data = final_join_plot, aes(fill = clade_sum_pdSigCl, color = clade_sum_pdSigCl)) +
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
                        limits = c(-5, 5)) + 
  geom_sf(data = ecoregions, alpha = 0)

# Output the plot
print(plot_area)
