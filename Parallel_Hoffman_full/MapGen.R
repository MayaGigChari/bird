#this is a script for generating the various maps
#this function kind of takes a while

###step 1: 


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




bird_final_data_nona<- st_as_sf(drop_na(bird_final_data))

plant_final_data_nona<- st_as_sf(drop_na(plant_final_data))

squamate_final_data_nona<- st_as_sf(drop_na(squamate_final_data))

mammal_final_data_nona<- st_as_sf(drop_na(mammal_final_data))

butterfly_final_data_nona<- st_as_sf(drop_na(butterfly_final_data))

#this still somehow does not work. we should also only have 1099 or something hexagons. 


library(ggplot2)


plot_area <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()    # Ensure equal aspect ratio

plot_area <- plot_area + 
  geom_sf(data = bird_final_data_nona, aes(fill = factor(mntdSgE), color = factor(mntdSgE)))

#some of these are not actually correct. 
# Customize the legend and color scale
plot_area_fin <- plot_area+
  scale_color_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"),
                     name = "O value",
                     labels = c("< 0.025 ", "insignificant", "> 0.975"),
                     guide = "none") +
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"),
                    name = "P value",
                    labels = c("< 0.025 ", "insignificant", "> 0.975"),
                    guide = "legend")

#birds are really under dispersed on the california level and normal/over dispersed on the ecoregion levels. 
plot_area_fin<- plot_area_fin + 
  geom_sf(data = ecoregions,alpha = 0)


ggsave("images/bird_Ecoregion_genus_level_mntd_distribution_final.png", plot_area_fin, width = 10, height = 10, dpi = 300)
dev.off()


#here need to generate a map with the hexagon data and the ecoregions also. 
plot_area_cali <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()+
  geom_sf(data = california) + 
  geom_sf(data = ecoregions, aes(fill = L3_KEY))+
  geom_sf(data = polygons, alpha = 0.1)

ggsave("images/ecoregions_with_hexes_overlaid.png", plot_area_cali, width = 10, height = 10, dpi = 300)


