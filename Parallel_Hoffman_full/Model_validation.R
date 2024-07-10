
packages_to_install <- c("picante", "ape", "stringr", "dplyr", "phytools", "data.table", "drc", "jsonlite", "tidyverse")

# Load necessary packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}

current_directory <- getwd()

Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)

model_functions <- list(
  L.3(), L.4(), L.5(), LL.3(), LL.4(), LL.5(), baro5()
  # Add other functions as needed
)

calculate_mape <- function(data, response_var, predictor_var, model_functions) {
  mape_results <- numeric(length(model_functions))  # Initialize results vector
  
  for (i in seq_along(model_functions)) {
    model <- model_functions[[i]]  # Get model function
    fit <- drm(data[[response_var]] ~ data[[predictor_var]], fct = model)
    predicted <- predict(fit)
    mape <- mean(abs((data[[response_var]] - predicted) / data[[response_var]])) * 100
    mape_results[i] <- mape
  }
  return(mape_results)
}

calculate_aic <- function(data, response_var, predictor_var, model_functions) {
  aic_results <- numeric(length(model_functions))  # Initialize results vector
  
  for (i in seq_along(model_functions)) {
    model <- model_functions[[i]]  # Get model function
    fit <- drm(data[[response_var]] ~ data[[predictor_var]], fct = model)
    aic <- AIC(fit)
    aic_results[i] <- aic
  }
  return(aic_results)
}


#get the data
#for the ranges: 

pd_data_sim_plants<- read.csv("Plants/0505_mishlerCI_pd_output_bootstrap_0505.csv")
mpd_data_sim_plants<- read.csv("Plants/0505_mishlerCI_mpd_output_bootstrap_0505.csv")
mntd_data_sim_plants<- read.csv("Plants/0505_mishlerCI_mntd_output_bootstrap_0505.csv")

pd_data_sim_plants<- data_clean(pd_data_sim_plants, "pd")
mpd_data_sim_plants<- data_clean(mpd_data_sim_plants, "mpd")
mntd_data_sim_plants<- data_clean(mntd_data_sim_plants, "mntd")

# Calculate MAPE for all models

mape_pd_low_plants <- calculate_mape(pd_data_sim_plants, "Low", "tree_size", model_functions)
mape_pd_high_plants <- calculate_mape(pd_data_sim_plants, "High", "tree_size", model_functions)
mape_mpd_low_plants <- calculate_mape(mpd_data_sim_plants, "Low", "tree_size", model_functions)
mape_mpd_high_plants <- calculate_mape(mpd_data_sim_plants, "High", "tree_size", model_functions)
mape_mntd_low_plants <- calculate_mape(mntd_data_sim_plants, "Low", "tree_size", model_functions)
mape_mntd_high_plants <- calculate_mape(mntd_data_sim_plants, "High", "tree_size", model_functions)


list_names<- list("L.3()", "L.4()", "L.5()", "LL.3()", "LL.4()", "LL.5()", "baro5()")

mape_data_plants <- data.frame(
  Model = unlist(list_names),
  MAPE_PD_Low = mape_pd_low_plants,
  MAPE_PD_High = mape_pd_high_plants,
  MAPE_MPD_Low = mape_mpd_low_plants,
  MAPE_MPD_High = mape_mpd_high_plants,
  MAPE_MNTD_Low = mape_mntd_low_plants,
  MAPE_MNTD_High = mape_mntd_high_plants
)

mape_data_plants$row_means <- rowMeans(mape_data_plants[, -1])
write.csv(mape_data_plants, file = "mape_scores_plants.csv")

# Sort the data frame by row_means
mape_data_plants <- mape_data_plants %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(mape_data_plants, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Mean Absolute Percentage Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average MAPE across variable models: Plants")
ggsave("Plants/images/MAPE_plot_plants.png",width = 10, height = 6)

# Calculate AIC for all models
aic_pd_low_plants <- calculate_aic(pd_data_sim_plants, "Low", "tree_size", model_functions)
aic_pd_high_plants <- calculate_aic(pd_data_sim_plants, "High", "tree_size", model_functions)
aic_mpd_low_plants <- calculate_aic(mpd_data_sim_plants, "Low", "tree_size", model_functions)
aic_mpd_high_plants <- calculate_aic(mpd_data_sim_plants, "High", "tree_size", model_functions)
aic_mntd_low_plants <- calculate_aic(mntd_data_sim_plants, "Low", "tree_size", model_functions)
aic_mntd_high_plants <- calculate_aic(mntd_data_sim_plants, "High", "tree_size", model_functions)

# Display AIC results



aic_data_plants <- data.frame(
  Model = unlist(list_names),
  ACE_PD_Low = aic_pd_low_plants,
  ACE_PD_High = aic_pd_high_plants,
  ACE_MPD_Low = aic_mpd_low_plants,
  ACE_MPD_High = aic_mpd_high_plants,
  ACE_MNTD_Low = aic_mntd_low_plants,
  ACE_MNTD_High = aic_mntd_high_plants
)

# Sort the data frame by row_sums
aic_data_plants$row_means <- rowMeans(aic_data_plants[, -1])

# Sort the data frame by row_means
aic_data_plants <- aic_data_plants %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(aic_data_plants, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "AIC score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average AIC across variable models: Plants")
ggsave("Plants/images/AIC_plot_plants.png",width = 10, height = 6)




#FOR SQUAMATES 

pd_data_sim_squamates<- read.csv("squamate/CI_pd_output_bootstrap_bird.csv")
mpd_data_sim_squamates<- read.csv("squamate/CI_mpd_output_bootstrap_bird.csv")
mntd_data_sim_squamates<- read.csv("squamate/CI_mntd_output_bootstrap_bird.csv")

pd_data_sim_squamates<- data_clean(pd_data_sim_squamates, "pd")
mpd_data_sim_squamates<- data_clean(mpd_data_sim_squamates, "mpd")
mntd_data_sim_squamates<- data_clean(mntd_data_sim_squamates, "mntd")
# Calculate MAPE 

mape_pd_low_squamates <- calculate_mape(pd_data_sim_squamates, "Low", "tree_size", model_functions)
mape_pd_high_squamates <- calculate_mape(pd_data_sim_squamates, "High", "tree_size", model_functions)
mape_mpd_low_squamates <- calculate_mape(mpd_data_sim_squamates, "Low", "tree_size", model_functions)
mape_mpd_high_squamates <- calculate_mape(mpd_data_sim_squamates, "High", "tree_size", model_functions)
mape_mntd_low_squamates <- calculate_mape(mntd_data_sim_squamates, "Low", "tree_size", model_functions)
mape_mntd_high_squamates <- calculate_mape(mntd_data_sim_squamates, "High", "tree_size", model_functions)



mape_data_squamates <- data.frame(
  Model = unlist(list_names),
  MAPE_PD_Low = mape_pd_low_squamates,
  MAPE_PD_High = mape_pd_high_squamates,
  MAPE_MPD_Low = mape_mpd_low_squamates,
  MAPE_MPD_High = mape_mpd_high_squamates,
  MAPE_MNTD_Low = mape_mntd_low_squamates,
  MAPE_MNTD_High = mape_mntd_high_squamates
)

write.csv(mape_data_squamates, file = "mape_scores_squamates.csv")


mape_data_squamates$row_means <- rowMeans(mape_data_squamates[, -1])

# Sort the data frame by row_means
mape_data_squamates <- mape_data_squamates %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(mape_data_squamates, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Mean Absolute Percentage Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average MAPE across variable models: Squamates")
ggsave("squamate/images/MAPE_plot_plants.png",width = 10, height = 6)


# Calculate AIC for all models
aic_pd_low_squamates <- calculate_aic(pd_data_sim_squamates, "Low", "tree_size", model_functions)
aic_pd_high_squamates <- calculate_aic(pd_data_sim_squamates, "High", "tree_size", model_functions)
aic_mpd_low_squamates <- calculate_aic(mpd_data_sim_squamates, "Low", "tree_size", model_functions)
aic_mpd_high_squamates <- calculate_aic(mpd_data_sim_squamates, "High", "tree_size", model_functions)
aic_mntd_low_squamates <- calculate_aic(mntd_data_sim_squamates, "Low", "tree_size", model_functions)
aic_mntd_high_squamates <- calculate_aic(mntd_data_sim_squamates, "High", "tree_size", model_functions)

# Display AIC results



aic_data_squamates <- data.frame(
  Model = unlist(list_names),
  ACE_PD_Low = aic_pd_low_squamates,
  ACE_PD_High = aic_pd_high_squamates,
  ACE_MPD_Low = aic_mpd_low_squamates,
  ACE_MPD_High = aic_mpd_high_squamates,
  ACE_MNTD_Low = aic_mntd_low_squamates,
  ACE_MNTD_High = aic_mntd_high_squamates
)

# Sort the data frame by row_sums
aic_data_squamates$row_means <- rowMeans(aic_data_squamates[, -1])

# Sort the data frame by row_means
aic_data_squamates <- aic_data_squamates %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(aic_data_squamates, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Row Means of AIC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average AIC across variable models: squamates")
ggsave("squamates/images/AIC_plot_plants.png",width = 10, height = 6)



#FOR BIRDS

pd_data_sim_birds<- read.csv("birds/0507CI_pd_output_bootstrap.csv")
mpd_data_sim_birds<- read.csv("birds/0507CI_mpd_output_bootstrap.csv")
mntd_data_sim_birds<- read.csv("birds/0507CI_mntd_output_bootstrap.csv")

pd_data_sim_birds<- data_clean(pd_data_sim_birds, "pd")
mpd_data_sim_birds<- data_clean(mpd_data_sim_birds, "mpd")
mntd_data_sim_birds<- data_clean(mntd_data_sim_birds, "mntd")
#calculate MAPE 


mape_pd_low_birds <- calculate_mape(pd_data_sim_birds, "Low", "tree_size", model_functions)
mape_pd_high_birds <- calculate_mape(pd_data_sim_birds, "High", "tree_size", model_functions)
mape_mpd_low_birds <- calculate_mape(mpd_data_sim_birds, "Low", "tree_size", model_functions)
mape_mpd_high_birds <- calculate_mape(mpd_data_sim_birds, "High", "tree_size", model_functions)
mape_mntd_low_birds <- calculate_mape(mntd_data_sim_birds, "Low", "tree_size", model_functions)
mape_mntd_high_birds <- calculate_mape(mntd_data_sim_birds, "High", "tree_size", model_functions)



mape_data_birds <- data.frame(
  Model = unlist(list_names),
  MAPE_PD_Low = mape_pd_low_birds,
  MAPE_PD_High = mape_pd_high_birds,
  MAPE_MPD_Low = mape_mpd_low_birds,
  MAPE_MPD_High = mape_mpd_high_birds,
  MAPE_MNTD_Low = mape_mntd_low_birds,
  MAPE_MNTD_High = mape_mntd_high_birds
)

write.csv(mape_data_birds, file = "mape_scores_birds.csv")


mape_data_birds$row_means <- rowMeans(mape_data_birds[, -1])

# Sort the data frame by row_means
mape_data_birds <- mape_data_birds %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(mape_data_birds, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Mean Absolute Percentage Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average MAPE across variable models: Birds")
ggsave("birds/images/MAPE_plot_birds.png",width = 10, height = 6)



# Calculate AIC for all models
aic_pd_low_birds <- calculate_aic(pd_data_sim_birds, "Low", "tree_size", model_functions)
aic_pd_high_birds <- calculate_aic(pd_data_sim_birds, "High", "tree_size", model_functions)
aic_mpd_low_birds <- calculate_aic(mpd_data_sim_birds, "Low", "tree_size", model_functions)
aic_mpd_high_birds <- calculate_aic(mpd_data_sim_birds, "High", "tree_size", model_functions)
aic_mntd_low_birds <- calculate_aic(mntd_data_sim_birds, "Low", "tree_size", model_functions)
aic_mntd_high_birds <- calculate_aic(mntd_data_sim_birds, "High", "tree_size", model_functions)

# Display AIC results



aic_data_birds <- data.frame(
  Model = unlist(list_names),
  ACE_PD_Low = aic_pd_low_birds,
  ACE_PD_High = aic_pd_high_birds,
  ACE_MPD_Low = aic_mpd_low_birds,
  ACE_MPD_High = aic_mpd_high_birds,
  ACE_MNTD_Low = aic_mntd_low_birds,
  ACE_MNTD_High = aic_mntd_high_birds
)

# Sort the data frame by row_sums
aic_data_birds$row_means <- rowMeans(aic_data_birds[, -1])

# Sort the data frame by row_means
aic_data_birds <- aic_data_birds %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(aic_data_birds, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Row Means of AIC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Row Means of AIC Values for Different Models")
ggtitle("Average AIC across variable models: birds")
ggsave("birds/images/AIC_plot_birds.png",width = 10, height = 6)


### Butterflies


pd_data_sim_butter<- read.csv("butterflies/GENUSCI_pd_output_bootstrap_bird.csv")
mpd_data_sim_butter<- read.csv("butterflies/GENUSCI_mpd_output_bootstrap.csv")
mntd_data_sim_butter<- read.csv("butterflies/GENUSCI_mntd_output_bootstrap.csv")

pd_data_sim_butter<- data_clean(pd_data_sim_butter, "pd")
mpd_data_sim_butter<- data_clean(mpd_data_sim_butter, "mpd")
mntd_data_sim_butter<- data_clean(mntd_data_sim_butter, "mntd")

#MAPE 

mape_pd_low_butter <- calculate_mape(pd_data_sim_butter, "Low", "tree_size", model_functions)
mape_pd_high_butter <- calculate_mape(pd_data_sim_butter, "High", "tree_size", model_functions)
mape_mpd_low_butter <- calculate_mape(mpd_data_sim_butter, "Low", "tree_size", model_functions)
mape_mpd_high_butter <- calculate_mape(mpd_data_sim_butter, "High", "tree_size", model_functions)
mape_mntd_low_butter <- calculate_mape(mntd_data_sim_butter, "Low", "tree_size", model_functions)
mape_mntd_high_butter <- calculate_mape(mntd_data_sim_butter, "High", "tree_size", model_functions)


mape_data_butter <- data.frame(
  Model = unlist(list_names),
  MAPE_PD_Low = mape_pd_low_butter,
  MAPE_PD_High = mape_pd_high_butter,
  MAPE_MPD_Low = mape_mpd_low_butter,
  MAPE_MPD_High = mape_mpd_high_butter,
  MAPE_MNTD_Low = mape_mntd_low_butter,
  MAPE_MNTD_High = mape_mntd_high_butter
)

write.csv(mape_data_butter, file = "mape_scores_butterflies.csv")


mape_data_butter$row_means <- rowMeans(mape_data_butter[, -1])

# Sort the data frame by row_means
mape_data_butter <- mape_data_butter %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(mape_data_butter, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Mean Absolute Percentage Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average MAPE across variable models: Butterflies")
ggsave("butterflies/images/MAPE_plot_butterflis.png",width = 10, height = 6)


# Calculate AIC for all models
aic_pd_low_butter <- calculate_aic(pd_data_sim_butter, "Low", "tree_size", model_functions)
aic_pd_high_butter <- calculate_aic(pd_data_sim_butter, "High", "tree_size", model_functions)
aic_mpd_low_butter <- calculate_aic(mpd_data_sim_butter, "Low", "tree_size", model_functions)
aic_mpd_high_butter <- calculate_aic(mpd_data_sim_butter, "High", "tree_size", model_functions)
aic_mntd_low_butter <- calculate_aic(mntd_data_sim_butter, "Low", "tree_size", model_functions)
aic_mntd_high_butter <- calculate_aic(mntd_data_sim_butter, "High", "tree_size", model_functions)

# Display AIC results



aic_data_butter <- data.frame(
  Model = unlist(list_names),
  ACE_PD_Low = aic_pd_low_butter,
  ACE_PD_High = aic_pd_high_butter,
  ACE_MPD_Low = aic_mpd_low_butter,
  ACE_MPD_High = aic_mpd_high_butter,
  ACE_MNTD_Low = aic_mntd_low_butter,
  ACE_MNTD_High = aic_mntd_high_butter
)

# Sort the data frame by row_sums
aic_data_butter$row_means <- rowMeans(aic_data_butter[, -1])

# Sort the data frame by row_means
aic_data_butter <- aic_data_butter %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(aic_data_butter, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Row Means of AIC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average AIC across variable models: butterflies")
ggsave("butterflies/images/AIC_plot_birds.png",width = 10, height = 6)


#MAMMALS


pd_data_sim_mammal<- read.csv("Mammals/CI_pd_output_bootstrap_bird.csv")
mpd_data_sim_mammal<- read.csv("Mammals/CI_mpd_output_bootstrap.csv")
mntd_data_sim_mammal<- read.csv("Mammals/CI_mntd_output_bootstrap.csv")

pd_data_sim_mammal<- data_clean(pd_data_sim_mammal, "pd")
mpd_data_sim_mammal<- data_clean(mpd_data_sim_mammal, "mpd")
mntd_data_sim_mammal<- data_clean(mntd_data_sim_mammal, "mntd")

#MAPE


mape_pd_low_mammal <- calculate_mape(pd_data_sim_mammal, "Low", "tree_size", model_functions)
mape_pd_high_mammal <- calculate_mape(pd_data_sim_mammal, "High", "tree_size", model_functions)
mape_mpd_low_mammal <- calculate_mape(mpd_data_sim_mammal, "Low", "tree_size", model_functions)
mape_mpd_high_mammal <- calculate_mape(mpd_data_sim_mammal, "High", "tree_size", model_functions)
mape_mntd_low_mammal <- calculate_mape(mntd_data_sim_mammal, "Low", "tree_size", model_functions)
mape_mntd_high_mammal <- calculate_mape(mntd_data_sim_mammal, "High", "tree_size", model_functions)


mape_data_mammal <- data.frame(
  Model = unlist(list_names),
  MAPE_PD_Low = mape_pd_low_mammal,
  MAPE_PD_High = mape_pd_high_mammal,
  MAPE_MPD_Low = mape_mpd_low_mammal,
  MAPE_MPD_High = mape_mpd_high_mammal,
  MAPE_MNTD_Low = mape_mntd_low_mammal,
  MAPE_MNTD_High = mape_mntd_high_mammal
)

write.csv(mape_data_mammal, file = "mape_scores_mammals.csv")


mape_data_mammal$row_means <- rowMeans(mape_data_mammal[, -1])

# Sort the data frame by row_means
mape_data_mammal <- mape_data_mammal %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(mape_data_mammal, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Mean Absolute Percentage Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Average MAPE across variable models: Mammals")
ggsave("Mammals/images/MAPE_plot_mammals.png",width = 10, height = 6)


# Calculate AIC for all models
aic_pd_low_mammal <- calculate_aic(pd_data_sim_mammal, "Low", "tree_size", model_functions)
aic_pd_high_mammal <- calculate_aic(pd_data_sim_mammal, "High", "tree_size", model_functions)
aic_mpd_low_mammal <- calculate_aic(mpd_data_sim_mammal, "Low", "tree_size", model_functions)
aic_mpd_high_mammal <- calculate_aic(mpd_data_sim_mammal, "High", "tree_size", model_functions)
aic_mntd_low_mammal <- calculate_aic(mntd_data_sim_mammal, "Low", "tree_size", model_functions)
aic_mntd_high_mammal <- calculate_aic(mntd_data_sim_mammal, "High", "tree_size", model_functions)

# Display AIC results

#worst fit for mntd low. 

aic_data_mammal <- data.frame(
  Model = unlist(list_names),
  ACE_PD_Low = aic_pd_low_mammal,
  ACE_PD_High = aic_pd_high_mammal,
  ACE_MPD_Low = aic_mpd_low_mammal,
  ACE_MPD_High = aic_mpd_high_mammal,
  ACE_MNTD_Low = aic_mntd_low_mammal,
  ACE_MNTD_High = aic_mntd_high_mammal
)

# Sort the data frame by row_sums
aic_data_mammal$row_means <- rowMeans(aic_data_mammal[, -1])

# Sort the data frame by row_means
aic_data_mammal <- aic_data_mammal %>% arrange(desc(row_means))

# Plot using ggplot2
ggplot(aic_data_mammal, aes(x = reorder(Model, -row_means), y = row_means)) +
  geom_point() +
  labs(x = "Model Functions", y = "Row Means of AIC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Row Means of AIC Values for Different Models")
ggtitle("Average AIC across variable models: Mammals")
ggsave("Mammals/images/AIC_plot_birds.png",width = 10, height = 6)


list_names<- list("L.3()", "L.4()", "L.5()", "LL.3()", "LL.4()", "LL.5()", "m_baro5()")


total_data_AIC <- data.frame(
  Model = unlist(list_names),
  mammals= aic_data_mammal$row_means,
  plants= aic_data_plants$row_means,
  squamates= aic_data_squamates$row_means,
  birds= aic_data_birds$row_means,
  butterfliess= aic_data_butter$row_means
)



library(ggplot2)
library(dplyr)

# Reshape the data
total_data_long <- total_data_AIC %>%
  tidyr::gather(key = "Species", value = "Mean", -Model)

# Plotting
ggplot(total_data_long, aes(x = Model, y = Mean, color = Species)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "AIC",
       x = "Model",
       y = "Mean")


total_data_mape <- data.frame(
  Model = unlist(list_names),
  mammals= mape_data_mammal$row_means,
  plants= mape_data_plants$row_means,
  squamates= mape_data_squamates$row_means,
  birds= mape_data_birds$row_means,
  butterfliess= mape_data_butter$row_means
)

library(ggplot2)
library(dplyr)

# Reshape the data
total_data_long <- total_data_mape %>%
  tidyr::gather(key = "Species", value = "Mean", -Model)

# Plotting
ggplot(total_data_long, aes(x = Model, y = Mean, color = Species)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "mape",
       x = "Model",
       y = "Mean")



#TODO: take pool of models, find best AIC score and calculate the difference between AIC and best models and feed this into aikaike weights thing. 

#need to assume that ecoregion-specific models 

install.packages("qpcR")
library(qpcR)

#need to use akaike weights 

#maybe for every model (mpd low, mntd low, pd low, pd high, mpd high, mntd high etc. take the AIC )
akaike.weights(aic_data_birds[,7])

birds_akaike<- sapply(aic_data_birds[c(9:11)], akaike.weights)
plants_akaike<- sapply(aic_data_plants[c(9:11)], akaike.weights)
butterflies_akaike<- sapply(aic_data_butter[c(9:11)], akaike.weights)
squamates_akaike<- sapply(aic_data_squamates[c(9:11)], akaike.weights)
mammals_akaike<- sapply(aic_data_mammal[c(9:11)], akaike.weights)


akaike_scores_pd<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,1]),
  plants = unlist(plants_akaike[3,1]),
  butter = unlist(butterflies_akaike[3,1]),
  squam = unlist(squamates_akaike[3,1]),
  mamm = unlist(mammals_akaike[3,1])
)


write.csv(akaike_scores_pd, file = "akaike_scores.csv")

akaike_scores_pd_high<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,2]),
  plants = unlist(plants_akaike[3,2]),
  butter = unlist(butterflies_akaike[3,2]),
  squam = unlist(squamates_akaike[3,2]),
  mamm = unlist(mammals_akaike[3,2])
)



#mpd low
akaike_scores_mpd<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,2]),
  plants = unlist(plants_akaike[3,2]),
  butter = unlist(butterflies_akaike[3,2]),
  squam = unlist(squamates_akaike[3,2]),
  mamm = unlist(mammals_akaike[3,2])
)

write.csv(akaike_scores_mpd, file = "akaike_scores_mpd.csv")



#squamates are kind of fucked up. 
#mpd high 
akaike_scores_mpd_high<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,4]),
  plants = unlist(plants_akaike[3,4]),
  butter = unlist(butterflies_akaike[3,4]),
  squam = unlist(squamates_akaike[3,4]),
  mamm = unlist(mammals_akaike[3,4])
)

#maybe I should just use LL.5 for this. 

akaike_scores_mntd<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,3]),
  plants = unlist(plants_akaike[3,3]),
  butter = unlist(butterflies_akaike[3,3]),
  squam = unlist(squamates_akaike[3,3]),
  mamm = unlist(mammals_akaike[3,3])
)

write.csv(akaike_scores_mntd, file = "akaike_scores_mntd.csv")


akaike_scores_mntd_high<- data.frame(
  model = unlist(list_names),
  birds= unlist(birds_akaike[3,6]),
  plants = unlist(plants_akaike[3,6]),
  butter = unlist(butterflies_akaike[3,6]),
  squam = unlist(squamates_akaike[3,6]),
  mamm = unlist(mammals_akaike[3,6])
)


#baro5 works the best probably. doesn't really matter between mntd and mpd baro5. 

#squamates don't really work that well anyways. 
#going to use the average. mntd will be 
data_long <- akaike_scores_mntd%>%
  pivot_longer(cols = -model, names_to = "category", values_to = "value")

# Create the plot
ggplot(data_long, aes(x = model, y = value, color = category)) +
  geom_point() +
  geom_line() +
  scale_y_log10() + # Use logarithmic scale for y-axis due to the wide range of values
  labs(title = "Akaike weights across clades: MNTD",
       x = "Model",
       y = "Value",
       color = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/akaike_scores_mntd.png",width = 10, height = 6)


aic_data_plants$ACE_PD_AVG<- (aic_data_plants[2]+aic_data_plants[3])/2
aic_data_plants$ACE_MPD_AVG<- (aic_data_plants[4]+aic_data_plants[5])/2
aic_data_plants$ACE_MNTD_AVG<- (aic_data_plants[6]+aic_data_plants[7])/2

aic_data_butter$ACE_PD_AVG<- (aic_data_butter[2]+aic_data_butter[3])/2
aic_data_butter$ACE_MPD_AVG<- (aic_data_butter[4]+aic_data_butter[5])/2
aic_data_butter$ACE_MNTD_AVG<- (aic_data_butter[6]+aic_data_butter[7])/2

aic_data_squamates$ACE_PD_AVG<- (aic_data_squamates[2]+aic_data_squamates[3])/2
aic_data_squamates$ACE_MPD_AVG<- (aic_data_squamates[4]+aic_data_squamates[5])/2
aic_data_squamates$ACE_MNTD_AVG<- (aic_data_squamates[6]+aic_data_squamates[7])/2

aic_data_mammal$ACE_PD_AVG<- (aic_data_mammal[2]+aic_data_mammal[3])/2
aic_data_mammal$ACE_MPD_AVG<- (aic_data_mammal[4]+aic_data_mammal[5])/2
aic_data_mammal$ACE_MNTD_AVG<- (aic_data_mammal[6]+aic_data_mammal[7])/2

aic_data_birds$ACE_PD_AVG<- (aic_data_birds[2]+aic_data_birds[3])/2
aic_data_birds$ACE_MPD_AVG<- (aic_data_birds[4]+aic_data_birds[5])/2
aic_data_birds$ACE_MNTD_AVG<- (aic_data_birds[6]+aic_data_birds[7])/2

#want to take the average AIC for each model. 



# Reshape the data
#baro5 is not the best for describing everything. 

#need to do genus-level for squamates. 

#should probably use the LL.5 function for mntd_low 

#should I just redo everything with LL.5?

####MODEL FITTING USING BARO_5 model.

#should look at squamates in particular for this issue. 


#THIS part is for plotting all the qqplots together and is for a figure for the masters thesis. 
library(dplyr)
pd_data_plants<- read.csv("Plants/0505_mishlerCI_pd_output_bootstrap_0505.csv")
mpd_data_plants<- read.csv("Plants/0505_mishlerCI_mpd_output_bootstrap_0505.csv")
mntd_data_plants<- read.csv("Plants/0505_mishlerCI_mntd_output_bootstrap_0505.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 
pd_model_plants<-as_tibble(t(surfaceGen(pd_data_plants, "pd", func = LL.5())), rownames = "key") #need to make models. 
mpd_model_plants<-as_tibble(t(surfaceGen(mpd_data_plants, "mpd",func = LL.5())), rownames = "key")
mntd_model_plants<-as_tibble(t(surfaceGen(mntd_data_plants, "mntd",func = LL.5())), rownames = "key")

#for pd
pd_plants_preds<- prediction_regression_data(pd_data, metric = "pd", func = baro5(), clade = "Plants")
mpd_plants_preds<- prediction_regression_data(mpd_data, metric = "mpd", func = baro5(), clade = "Plants")
mntd_plants_preds<- prediction_regression_data(mntd_data, metric = "mntd", func = baro5(), clade = "Plants")

###for birds

library(dplyr)
##FOR BIRDS

pd_data_birds<- read.csv("birds/0507CI_pd_output_bootstrap.csv")
mpd_data_birds<- read.csv("birds/0507CI_mpd_output_bootstrap.csv")
mntd_data_birds<- read.csv("birds/0507CI_mntd_output_bootstrap.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 
pd_model_birds<-as_tibble(t(surfaceGen(pd_data_birds, "pd", func = baro5())), rownames = "key") #need to make models. 
mpd_model_birds<-as_tibble(t(surfaceGen(mpd_data_birds, "mpd", func = baro5())), rownames = "key")
mntd_model_birds<-as_tibble(t(surfaceGen(mntd_data_birds, "mntd", func = baro5())), rownames = "key")


pd_birds_preds<- prediction_regression_data(pd_data, metric = "pd", func = baro5(), clade = "birds")
mpd_birds_preds<- prediction_regression_data(mpd_data, metric = "mpd", func = baro5(), clade = "birds")
mntd_birds_preds<- prediction_regression_data(mntd_data, metric = "mntd", func = baro5(), clade = "birds")


###FOR SQUAMATES
pd_data_squam<- read.csv("squamate/CI_pd_output_bootstrap_bird.csv")
mpd_data_squam<- read.csv("squamate/CI_mpd_output_bootstrap.csv")
mntd_data_squam<- read.csv("squamate/CI_mntd_output_bootstrap.csv")

#need to figure out how to write this to json. 
pd_model_squam<-as_tibble(t(surfaceGen(pd_data_squam, "pd", func = LL.5())), rownames = "key") #need to make models. 
mpd_model_squam<-as_tibble(t(surfaceGen(mpd_data_squam, "mpd",func = LL.5())), rownames = "key")
mntd_model_squam<-as_tibble(t(surfaceGen(mntd_data_squam, "mntd",func = LL.5())), rownames = "key")

#for pd
pd_squam_preds<- prediction_regression_data(pd_data_squam, metric = "pd", func = baro5(), clade = "squamate")
mpd_squam_preds<- prediction_regression_data(mpd_data_squam, metric = "mpd", func = baro5(), clade = "squamate")
mntd_squam_preds<- prediction_regression_data(mntd_data_squam, metric = "mntd", func = baro5(), clade = "squamate")


###FOR MAMMALS

pd_data_Mammals<- read.csv("Mammals/CI_pd_output_bootstrap_bird.csv")
mpd_data_Mammals<- read.csv("Mammals/CI_mpd_output_bootstrap.csv")
mntd_data_Mammals<- read.csv("Mammals/CI_mntd_output_bootstrap.csv")


pd_model_Mammals<-as_tibble(t(surfaceGen(pd_data_Mammals, "pd", func = LL.5())), rownames = "key") #need to make models. 
mpd_model_Mammals<-as_tibble(t(surfaceGen(mpd_data_Mammals, "mpd",func = LL.5())), rownames = "key")
mntd_model_Mammals<-as_tibble(t(surfaceGen(mntd_data_Mammals, "mntd",func = LL.5())), rownames = "key")

#for pd
pd_Mammals_preds<- prediction_regression_data(pd_data_Mammals, metric = "pd", func = baro5(), clade = "Mammals")
mpd_Mammals_preds<- prediction_regression_data(mpd_data_Mammals, metric = "mpd", func = baro5(), clade = "Mammals")
mntd_Mammals_preds<- prediction_regression_data(mntd_data_Mammals, metric = "mntd", func = baro5(), clade = "Mammals")


### BUTTERFLIES

pd_data_butterflies<- read.csv("butterflies/GENUSCI_pd_output_bootstrap_bird.csv")
mpd_data_butterflies<- read.csv("butterflies/GENUSCI_mpd_output_bootstrap.csv")
mntd_data_butterflies<- read.csv("butterflies/GENUSCI_mntd_output_bootstrap.csv")

pd_model_butterflies<-as_tibble(t(surfaceGen(pd_data_butterflies, "pd", func = LL.5())), rownames = "key") #need to make models. 
mpd_model_butterflies<-as_tibble(t(surfaceGen(mpd_data_butterflies, "mpd",func = LL.5())), rownames = "key")
mntd_model_butterflies<-as_tibble(t(surfaceGen(mntd_data_butterflies, "mntd",func = LL.5())), rownames = "key")

#for pd
pd_butterflies_preds<- prediction_regression_data(pd_data_butterflies, metric = "pd", func = baro5(), clade = "butterflies")
mpd_butterflies_preds<- prediction_regression_data(mpd_data_butterflies, metric = "mpd", func = baro5(), clade = "butterflies")
mntd_butterflies_preds<- prediction_regression_data(mntd_data_butterflies, metric = "mntd", func = baro5(), clade = "butterflies")




library(ggplot2)
ggplot(pd_plants_preds)+
  points(x = "Actual_Low", y = "Predictions_Low")


pd_1 <- pd_plants_preds %>% mutate(dataset = "pd_plants_preds")
mpd_1 <- mpd_plants_preds %>% mutate(dataset = "mpd_plants_preds")
mntd_1 <- mntd_plants_preds %>% mutate(dataset = "mntd_plants_preds")

pd_2 <- pd_birds_preds %>% mutate(dataset = "pd_birds_preds")
mpd_2 <- mpd_birds_preds %>% mutate(dataset = "mpd_birds_preds")
mntd_2 <- mntd_birds_preds %>% mutate(dataset = "mntd_birds_preds")

pd_3 <- pd_squam_preds %>% mutate(dataset = "pd_squam_preds")
mpd_3 <- mpd_squam_preds %>% mutate(dataset = "mpd_squam_preds")
mntd_3 <- mntd_squam_preds %>% mutate(dataset = "mntd_squam_preds")


pd_4 <- pd_Mammals_preds %>% mutate(dataset = "pd_Mammals_preds")
mpd_4 <- mpd_Mammals_preds %>% mutate(dataset = "mpd_Mammals_preds")
mntd_4 <- mntd_Mammals_preds %>% mutate(dataset = "mntd_Mammals_preds")


pd_5 <- pd_butterflies_preds %>% mutate(dataset = "pd_butterflies_preds")
mpd_5 <- mpd_butterflies_preds %>% mutate(dataset = "mpd_butterflies_preds")
mntd_5 <- mntd_butterflies_preds %>% mutate(dataset = "mntd_butterflies_preds")







# Combine all dataframes into one
combined_df <- bind_rows(pd_1,mpd_1,mntd_1, pd_2, mpd_2, mntd_2, pd_3, mpd_3, mntd_3, pd_4, mpd_4, mntd_4, pd_5, mpd_5, mntd_5)

# Plot using ggplot2
ggplot(combined_df, aes(x = Actual_Low , y = Predictions_Low )) +
  geom_point() +
  labs(title = "Actual vs Predicted Low",
       x = "Scaled True",
       y = "Scaled Predicted") +
  theme_minimal()
