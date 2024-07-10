#this is a script for generating the various maps
#this function kind of takes a while

###step 1: 


bird_final_data<- data.frame(st_read("birds/final_output_baro5.shp"))
plant_final_data<- data.frame(st_read("Plants/final_output_baro5.shp"))
squamate_final_data<- data.frame(st_read("squamate/final_output_baro5.shp"))
mammal_final_data<- data.frame(st_read("Mammals/final_output_baro5.shp"))
butterfly_final_data<- data.frame(st_read("butterflies/final_output_baro5.shp"))


#need to now filter for multiple ecoregions. 


#this doesn't happen. 
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
  geom_sf(data = butterfly_final_data_nona, aes(fill = factor( mntdSigCl), color = factor( mntdSigCl)))

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

ggsave("images/butterfly_California_genus_level_mntd_distribution_final_baro5.png", plot_area_fin, width = 10, height = 10, dpi = 300)
dev.off()


#here need to generate a map with the hexagon data and the ecoregions also. 
plot_area_cali <- ggplot() +
  theme_void() +  # Remove default axes and background
  coord_equal()+
  geom_sf(data = california) + 
  geom_sf(data = ecoregions, aes(fill = L3_KEY))+
  geom_sf(data = polygons, alpha = 0.1)

ggsave("images/ecoregions_with_hexes_overlaid.png", plot_area_cali, width = 10, height = 10, dpi = 300)

#need to take the whole matrix and determine for each hexagon which counts co-occur 
#need to have a matrix of 3x3: pd, mpd, mntd x pd, mpd, mntd for each combination (5 choose 2)

#could also have 3 5x5 matrixes, one for each metric 


#need to do this for california first 

install.packages("corrplot")
install.packages("Hmisc")
library(Hmisc)
library(corrplot)
final_join

colnames(final_join)

cross_clade_cali_matrix<- final_join%>%
  dplyr::select(bird_pdSigCl, bird_mpdSigCl, bird_mntdSigCl, plant_pdSigCl, plant_mpdSigCl, plant_mntdSigCl,
                squamate_pdSigCl, squamate_mpdSigCl, squamate_mntdSigCl, mammal_pdSigCl, mammal_mpdSigCl, mammal_mntdSigCl, butterfly_pdSigCl, butterfly_mpdSigCl, butterfly_mntdSigCl)

colnames(cross_clade_cali_matrix)<-c("bird pd", "bird mpd", "bird mntd", "plant pd", "plant mpd","plant mntd", "squamate pd", "squamate mpd", "squamate mntd", "mammal pd", "mammal mpd", "mammal mntd", "butterfly pd", "butterfly mpd", "butterfly mntd")

cross_clade_cali_matrix<- drop_na(cross_clade_cali_matrix)

cor_matrix <- cor(cross_clade_cali_matrix, method = "spearman")

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, tl.col = "black")


cor_results <- rcorr(as.matrix(cross_clade_cali_matrix), type = "spearman")
cor_matrix <- cor_results$r
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, tl.col = "black")

p_matrix <- cor_results$P

# Visualize the correlation matrix with significance levels
# Use corrplot with significance level
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix, sig.level = 0.05, insig = "blank", number.cex = 0.4)


cross_clade_eco_matrix<- final_join%>%
  dplyr::select(bird_pdSigEc, bird_mpdSgEc, bird_mntdSgE, plant_pdSigEc, plant_mpdSgEc, plant_mntdSgE,
                squamate_pdSigEc, squamate_mpdSgEc, squamate_mntdSgE, mammal_pdSigEc, mammal_mpdSgEc, mammal_mntdSgE, butterfly_pdSigEc, butterfly_mpdSgEc, butterfly_mntdSgE)

colnames(cross_clade_eco_matrix)<-c("bird pd", "bird mpd", "bird mntd", "plant pd", "plant mpd","plant mntd", "squamate pd", "squamate mpd", "squamate mntd", "mammal pd", "mammal mpd", "mammal mntd", "butterfly pd", "buttefly mpd", "butterfly mntd")


cross_clade_eco_matrix<- drop_na(cross_clade_eco_matrix)

cor_matrix_eco <- cor(cross_clade_eco_matrix, method = "spearman")

corrplot(cor_matrix_eco, method = "color", addCoef.col = "black", tl.cex = 0.4, tl.col = "black")


cor_results_eco <- rcorr(as.matrix(cross_clade_eco_matrix), type = "spearman")
cor_matrix_eco <- cor_results_eco$r
p_matrix_eco <- cor_results_eco$P

# Visualize the correlation matrix with significance levels
# Use corrplot with significance level
corrplot(cor_matrix_eco, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_eco, sig.level = 0.05, insig = "blank", number.cex = 0.4)


#chi square test 
library(corrplot)
library(Hmisc)

# Set column names for the dataset
colnames(cross_clade_cali_matrix) <- c("bird pd", "bird mpd", "bird mntd", 
                                       "plant pd", "plant mpd", "plant mntd", 
                                       "squamate pd", "squamate mpd", "squamate mntd", 
                                       "mammal pd", "mammal mpd", "mammal mntd", 
                                       "butterfly pd", "butterfly mpd", "butterfly mntd")

# Remove rows with NA values
cross_clade_cali_matrix <- na.omit(cross_clade_cali_matrix)

# Initialize a matrix to store p-values of Chi-Square tests
n <- ncol(cross_clade_cali_matrix)
chi_square_p_values <- matrix(NA, n, n, dimnames = list(colnames(cross_clade_cali_matrix), colnames(cross_clade_cali_matrix)))

# Perform Chi-Square test of independence for each pair of columns
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    contingency_table <- table(cross_clade_cali_matrix[, i], cross_clade_cali_matrix[, j])
    chi_test <- chisq.test(contingency_table)
    chi_square_p_values[i, j] <- chi_test$p.value
    chi_square_p_values[j, i] <- chi_test$p.value
  }
}

non_significance_matrix <- ifelse(chi_square_p_values < 0.05, 0, 1)

# Print non-significant relationships
non_significant_relationships <- which(non_significance_matrix == 1, arr.ind = TRUE)
if (nrow(non_significant_relationships) > 0) {
  for (k in 1:nrow(non_significant_relationships)) {
    row <- non_significant_relationships[k, 1]
    col <- non_significant_relationships[k, 2]
    if (row < col) { # Print each pair only once
      cat("Non-significant relationship between", colnames(cross_clade_cali_matrix)[row], "and", colnames(cross_clade_cali_matrix)[col], 
          "with p-value =", chi_square_p_values[row, col], "\n")
    }
  }
} else {
  cat("All relationships are significant.\n")
}

#need to do this but for quantile data instead of the normal data. 

# for individual metrics: 


cali_pd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_pdSigCl,plant_pdSigCl, 
                squamate_pdSigCl, mammal_pdSigCl, butterfly_pdSigCl))

cali_mpd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_mpdSigCl,plant_mpdSigCl, 
                squamate_mpdSigCl, mammal_mpdSigCl, butterfly_mpdSigCl))

cali_mntd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_mntdSigCl,plant_mntdSigCl, 
                squamate_mntdSigCl, mammal_mntdSigCl, butterfly_mntdSigCl))

eco_pd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_pdSigEc,plant_pdSigEc, 
                squamate_pdSigEc, mammal_pdSigEc, butterfly_pdSigEc))

eco_mpd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_mpdSgEc,plant_mpdSgEc, 
                squamate_mpdSgEc, mammal_mpdSgEc, butterfly_mpdSgEc))


eco_mntd_matrix<- na.omit(final_join%>%
  dplyr::select(bird_mntdSgE,plant_mntdSgE, 
                squamate_mntdSgE, mammal_mntdSgE, butterfly_mntdSgE))


colnames(cali_pd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")
colnames(cali_mpd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")
colnames(cali_mntd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")
colnames(eco_pd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")
colnames(eco_mpd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")
colnames(eco_mntd_matrix)<-c("bird","plant", "squamate",  "mammal","butterfly")



cor_results_cali_pd <- rcorr(as.matrix(cali_pd_matrix), type = "spearman")
cor_matrix_cali_pd <- cor_results_cali_pd$r
p_matrix_cali_pd <- cor_results_cali_pd$P



cor_results_cali_mpd <- rcorr(as.matrix(cali_mpd_matrix), type = "spearman")
cor_matrix_cali_mpd <- cor_results_cali_mpd$r
p_matrix_cali_mpd <- cor_results_cali_mpd$P


cor_results_cali_mntd <- rcorr(as.matrix(cali_mntd_matrix), type = "spearman")
cor_matrix_cali_mntd <- cor_results_cali_mntd$r
p_matrix_cali_mntd <- cor_results_cali_mntd$P


cor_results_eco_pd <- rcorr(as.matrix(eco_pd_matrix), type = "spearman")
cor_matrix_eco_pd <- cor_results_eco_pd$r
p_matrix_eco_pd <- cor_results_eco_pd$P


cor_results_eco_mpd <- rcorr(as.matrix(eco_mpd_matrix), type = "spearman")
cor_matrix_eco_mpd <- cor_results_eco_mpd$r
p_matrix_eco_mpd <- cor_results_eco_mpd$P


cor_results_eco_mntd <- rcorr(as.matrix(eco_mntd_matrix), type = "spearman")
cor_matrix_eco_mntd <- cor_results_eco_mntd$r
p_matrix_eco_mntd <- cor_results_eco_mntd$P


# Use corrplot with significance level
library(corrplot)
library(grDevices)

png("Correlation_Plot_California_PD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_cali_pd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_cali_pd, 
         sig.level = 0.05, insig = "blank")
dev.off()

# Save high-resolution PNG for California MPD
png("Correlation_Plot_California_MPD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_cali_mpd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_cali_mpd, 
         sig.level = 0.05, insig = "blank")
dev.off()

# Save high-resolution PNG for California MNTD
png("Correlation_Plot_California_MNTD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_cali_mntd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_cali_mntd, 
         sig.level = 0.05, insig = "blank")
dev.off()

# Save high-resolution PNG for Ecosystem PD
png("Correlation_Plot_Ecosystem_PD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_eco_pd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_eco_pd, 
         sig.level = 0.05, insig = "blank")
dev.off()

# Save high-resolution PNG for Ecosystem MPD
png("Correlation_Plot_Ecosystem_MPD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_eco_mpd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_eco_mpd, 
         sig.level = 0.05, insig = "blank")
dev.off()

# Save high-resolution PNG for Ecosystem MNTD
png("Correlation_Plot_Ecosystem_MNTD.png", width = 2000, height = 2000, res = 300)
corrplot(cor_matrix_eco_mntd, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, tl.col = "black", p.mat = p_matrix_eco_mntd, 
         sig.level = 0.05, insig = "blank")
dev.off()
