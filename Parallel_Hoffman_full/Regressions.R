#this is a script for regressing all the pd's mpd's and mntd's against each other pairwise.
library(dplyr)
joined_data<- readRDS("final_join_data_with_sums_whitespace")

joined_data_pd<- joined_data%>%
  dplyr::select(bird_pd_vals, plant_pd_vals, mammal_pd_vals, squamate_pd_vals, mammal_pd_vals,butterfly_pd_vals)

colnames(joined_data_pd)<- c("bird", "plant", "mammal", "squamate", "butterfly")


joined_data_pd_nona<- drop_na(joined_data_pd)




library(ggplot2)

# Create the scatter plot
plot <- ggplot(joined_data_pd_nona, aes(y = plant_pd_vals, x = butterfly_pd_vals)) +
  geom_point() 

# Display the plot
ggsave("images/regression_test_2.png")

dev.off()

library(ggplot2)
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)


# Variables to use in pairs
variables <- c("bird", "plant", "mammal", "squamate", "butterfly")

# Create empty list to store plots
plot_list <- list()

# Nested loop to create pairwise plots
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    x <- variables[i]
    y <- variables[j]
    plot <- ggplot(joined_data_pd_nona, aes_string(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
      labs(x = x, y = y) +
      theme_minimal() +  # Adjust theme
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      stat_cor(method = "pearson", size = 1)  # Add R-squared value
    plot_list[[length(plot_list) + 1]] <- plot
  }
}

# Convert the list of plots into a grid
k <- grid.arrange(grobs = plot_list, ncol = 2)

# Save the grid as a large image (8x10 inches)
ggsave("pd_pairwise_regression_plot.png",k,  width = 8, height = 10, units = "in")




#####for mpd
joined_data_mpd<- joined_data%>%
  dplyr::select(bird_mpd_vls, plant_mpd_vls, mammal_mpd_vls, squamate_mpd_vls, mammal_mpd_vls,butterfly_mpd_vls)

colnames(joined_data_mpd)<- c("bird", "plant", "mammal", "squamate", "butterfly")


joined_data_mpd_nona<- drop_na(joined_data_mpd)



# Create the scatter plot

# Variables to use in pairs
variables <- c("bird", "plant", "mammal", "squamate", "butterfly")

# Create empty list to store plots
plot_list <- list()

# Nested loop to create pairwise plots
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    x <- variables[i]
    y <- variables[j]
    plot <- ggplot(joined_data_mpd_nona, aes_string(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
      labs(x = x, y = y) +
      theme_minimal() +  # Adjust theme
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      stat_cor(method = "pearson", size = 1)  # Add R-squared value
    plot_list[[length(plot_list) + 1]] <- plot
  }
}

# Convert the list of plots into a grid
l<- grid.arrange(grobs = plot_list, ncol = 2)

# Save the grid as a large image (8x10 inches)
ggsave("mpd_pairwise_regression_plot.png", plot = l, width = 8, height = 10, units = "in")




#####for mntd

joined_data_mntd<- joined_data%>%
  dplyr::select(bird_mntd_vl, plant_mntd_vl, mammal_mntd_vl, squamate_mntd_vl, mammal_mntd_vl,butterfly_mntd_vl)

colnames(joined_data_mntd)<- c("bird", "plant", "mammal", "squamate", "butterfly")


joined_data_mntd_nona<- drop_na(joined_data_mntd)



# Create the scatter plot

# Variables to use in pairs
variables <- c("bird", "plant", "mammal", "squamate", "butterfly")

# Create empty list to store plots
plot_list <- list()

# Nested loop to create pairwise plots
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    x <- variables[i]
    y <- variables[j]
    plot <- ggplot(joined_data_mntd_nona, aes_string(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
      labs(x = x, y = y) +
      theme_minimal() +  # Adjust theme
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      stat_cor(method = "pearson", size = 1)  # Add R-squared value
    plot_list[[length(plot_list) + 1]] <- plot
  }
}

# Convert the list of plots into a grid
m<- grid.arrange(grobs = plot_list, ncol = 2)

# Save the grid as a large image (8x10 inches)
ggsave("mntd_pairwise_regression_plot.png", plot = m, width = 8, height = 10, units = "in")
