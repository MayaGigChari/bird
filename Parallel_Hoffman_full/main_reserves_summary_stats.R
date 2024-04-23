library(sf)
library(picante)
library(dplyr)

directory_path <- "birds/reserves"

# List all files in the directory
files <- list.files(directory_path, full.names = TRUE)

filenames <- basename(files)
filenames <- gsub("\\.rds$", "", filenames)
filenames <- gsub("_full_data_output", "", filenames)

# Output the filenames
print(filenames)
# Create an empty list to store data frame



# Loop through each file
# Create an empty dataframe to store results
results_df <- data.frame()

# Loop through each file
for (file in file_names) {
  # Read the file
  data_temp_file <- read_rds(file) 
  
  # Calculate variables
  pd_exp_cali <- sum(unlist(data_temp_file$pd_significance_ecoregions) == 0)
  pd_high_cali <- sum(unlist(data_temp_file$pd_significance_california) == 1)
  pd_low_cali <- sum(unlist(data_temp_file$pd_significance_california) == -1)
  
  mpd_exp_cali <- sum(unlist(data_temp_file$mpd_significance_ecoregions) == 0)
  mpd_high_cali <- sum(unlist(data_temp_file$mpd_significance_california) == 1)
  mpd_low_cali <- sum(unlist(data_temp_file$mpd_significance_california) == -1)
  
  mntd_exp_cali <- sum(unlist(data_temp_file$mntd_significance_ecoregions) == 0)
  mntd_high_cali <- sum(unlist(data_temp_file$mntd_significance_california) == 1)
  mntd_low_cali <- sum(unlist(data_temp_file$mntd_significance_california) == -1)
  
  pd_exp_ecoregions <- sum(unlist(data_temp_file$pd_significance_ecoregions) == 0)
  pd_high_ecoregions <- sum(unlist(data_temp_file$pd_significance_ecoregions) == 1)
  pd_low_ecoregions <- sum(unlist(data_temp_file$pd_significance_ecoregions) == -1)
  
  mpd_exp_ecoregions <- sum(unlist(data_temp_file$mpd_significance_ecoregions) == 0)
  mpd_high_ecoregions <- sum(unlist(data_temp_file$mpd_significance_ecoregions) == 1)
  mpd_low_ecoregions <- sum(unlist(data_temp_file$mpd_significance_ecoregions) == -1)
  
  mntd_exp_ecoregions <- sum(unlist(data_temp_file$mntd_significance_ecoregions) == 0)
  mntd_high_ecoregions <- sum(unlist(data_temp_file$mntd_significance_ecoregions) == 1)
  mntd_low_ecoregions <- sum(unlist(data_temp_file$mntd_significance_ecoregions) == -1)
  
  # Create a vector containing all the variables
  variable_vector <- c(
    pd_exp_cali,
    pd_high_cali,
    pd_low_cali,
    mpd_exp_cali,
    mpd_high_cali,
    mpd_low_cali,
    mntd_exp_cali,
    mntd_high_cali,
    mntd_low_cali,
    pd_exp_ecoregions,
    pd_high_ecoregions,
    pd_low_ecoregions,
    mpd_exp_ecoregions,
    mpd_high_ecoregions,
    mpd_low_ecoregions,
    mntd_exp_ecoregions,
    mntd_high_ecoregions,
    mntd_low_ecoregions
  )
  
  # Assign the vector to a row in the results dataframe
  results_df <- rbind(results_df, variable_vector)
}

variable_names <- c(
  "pd_exp_cali",
  "pd_high_cali",
  "pd_low_cali",
  "mpd_exp_cali",
  "mpd_high_cali",
  "mpd_low_cali",
  "mntd_exp_cali",
  "mntd_high_cali",
  "mntd_low_cali",
  "pd_exp_ecoregions",
  "pd_high_ecoregions",
  "pd_low_ecoregions",
  "mpd_exp_ecoregions",
  "mpd_high_ecoregions",
  "mpd_low_ecoregions",
  "mntd_exp_ecoregions",
  "mntd_high_ecoregions",
  "mntd_low_ecoregions"
)

# Set row names to be the reserves/reserve names
rownames(results_df) <- filenames
colnames(results_df)<- variable_names

# Print the dataframe
print(results_df)

write.csv(results_df, file = "birds/reserve_summary_statistics_output.csv")



# Initialize an empty list to store datasets
data_list <- list()

# Read in each RDS file and store them in the list
for (file in files) {
  data_list[[file]] <- readRDS(file)
}

# Concatenate all datasets together using cbind
combined_data <- do.call(rbind, data_list)

combined_data$tree_size<- lapply(combined_data$taxa_present_in_tree, length)
# Print the combined dataset
print(combined_data$tree_size)

#now all the data is combined together. 

library(ggplot2)

# Assuming your combined dataset is named combined_data
# Replace "MNG_AGNCY" and "true_pd" with the actual column names in your dataset
# Plot using ggplot
b<-ggplot(combined_data, aes(x = unlist(tree_size), y = unlist(true_pd), color = MNG_AGNCY)) +
  geom_point() +
  labs(x = "Number of Taxa Present in Tree", y = "True PD") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave("birds/images/all_reserves.png", plot = b, width = 10, height = 6, dpi = 300)


g<- ggplot(combined_data, aes(x = unlist(tree_size), y = unlist(true_pd), color = MNG_AGNCY)) +
  geom_point() +
  labs(x = "Number of Taxa Present in Tree", y = "True PD") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8)) +
  facet_wrap(~US_L3CODE)

ggsave("birds/images/ecoregion_sep_reserves.png", plot = g, width = 10, height = 6, dpi = 300)


count_data <- combined_data %>%
  filter(pd_significance_california >= 0) %>%
  group_by(MNG_AGNCY) %>%
  summarise(count = n())

# Create bar chart
q<- ggplot(count_data, aes(x = MNG_AGNCY, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Management Agency", y = "Count", title = "Number of Tuples with pd_significance_california >= 0 by Management Agency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggsave("birds/images/reserves_highest_count_barplot.png", plot = q, width = 10, height = 6, dpi = 300)

unique(count_data$pd_significance_ecoregions)

combined_data_2<- na.omit(combined_data)
#this is not correct. 
#counting geometries is like counting tuples. 
total_counts <- combined_data_2 %>%
  group_by(MNG_AGNCY) %>%
  summarise(total_count = n_distinct(geometry))  # Counting distinct US_L3CODEs

# Filter data where pd_significance_california >= 0, group by MNG_AGNCY, and count the number of tuples
count_data <- combined_data_2 %>%
  filter(!is.null(pd_significance_ecoregions), mpd_significance_ecoregions == 0) %>%
  group_by(MNG_AGNCY) %>%
  summarise(count = n_distinct(geometry)) %>%  # Counting distinct geometries
  filter(count > 1)  # Filter for count greater than 1

# Merge total counts with count data
count_data <- inner_join(count_data, data.frame(total_counts), by = "MNG_AGNCY") %>%
  mutate(normalized_count = count / total_count)

# Create bar chart
r<- ggplot(count_data, aes(x = MNG_AGNCY, y = normalized_count)) +
  geom_bar(stat = "identity") +
  labs(x = "Management Agency", y = "Normalized Count", title = "Normalized Number of Tuples with pd_significance_california >= 0 by Management Agency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggsave("birds/images/reserves_normalized_count_barplog_ecoregions.png", plot = r, width = 10, height = 6, dpi = 300)


#want to plot the birds of california expected cure. 
seq_sizes<- seq(5,250, by = 1)

ci_cali<- lapply(seq_sizes, cI_generator, params_json_file = "birds/pd_model_params.json")

list_low<- list()
list_high<- list()
for(i in 1: length(seq_sizes))
{
  list_low[i]<- ci_cali[[i]]$lower_vals
  list_high[i]<- ci_cali[[i]]$upper_vals
}
list_low<-unlist(list_low)
list_high<- unlist(list_high)


# Show combined plot
print(combined_plot)
