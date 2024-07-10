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


clade = "birds_ranges"

current_directory <- getwd()

Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)

#get the data
#pd_data <- retrieve_data(clade, "pd")
#mpd_data <- retrieve_data(clade, "mpd")
#mntd_data <- retrieve_data(clade, "mntd")

#generate the models
#pd_model<-surfaceGen(pd_data, "pd")  
#mpd_model<-surfaceGen(mpd_data, "mpd")
#mntd_model<-surfaceGen(mntd_data, "mntd")

#for the ranges: 

pd_data<- read.csv("birds/0507CI_pd_output_bootstrap.csv")
mpd_data<- read.csv("birds/0507CI_mpd_output_bootstrap.csv")
mntd_data<- read.csv("birds/0507CI_mntd_output_bootstrap.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 
pdf(file="birds/full_model_pd_0507_baro5.pdf")
pd_model<-as_tibble(t(surfaceGen(pd_data, "pd", func = baro5())), rownames = "key") #need to make models. 
dev.off()
pdf(file="birds/full_model_mpd_0507_baro5.pdf")
mpd_model<-as_tibble(t(surfaceGen(mpd_data, "mpd", func = baro5())), rownames = "key")
dev.off()
pdf(file="birds/genus_full_model_mntdd_0507_baro5.pdf")
mntd_model<-as_tibble(t(surfaceGen(mntd_data, "mntd", func = baro5())), rownames = "key")
dev.off()
#plot(mpd_model$tree_sizes, mpd_model$low)


prediction_regression(pd_data, metric = "pd", func = baro5(), clade = "birds")
prediction_regression(mpd_data, metric = "mpd", func = baro5(), clade = "birds")
prediction_regression(mntd_data, metric = "mntd", func = baro5(), clade = "birds")

#wite model coefficients to csv files for visualization. 
write.csv(pd_model, "birds/pd_model_params_0507_baro5.csv", row.names = TRUE)
write.csv(mpd_model, "birds/mpd_model_params_0507_baro5.csv", row.names = TRUE)
write.csv(mntd_model, "birds/mntd_model_params_0507_baro5.csv", row.names = TRUE)

json_pd<- toJSON(x = pd_model, dataframe = 'rows', pretty = F)
write(json_pd, file = "birds/pd_model_params_0507_baro5.json")

json_mpd<- toJSON(x = mpd_model, dataframe = 'rows', pretty = F)
write(json_mpd, file = "birds/mpd_model_params_0507_baro5.json")

json_mntd<- toJSON(x = mntd_model, dataframe = 'rows', pretty = F)
write(json_mntd, file = "birds/mntd_model_params_0507_baro5.json")


#surface generation for ecoregions: 

dir_list_ecoregions <- list.dirs("birds/ecoregion_data_2",recursive = FALSE)  

#these values have definitely chagned. 
for(i in dir_list_ecoregions)
{
  pd_data_temp<- read.csv(paste(i, "/0507_ecoregionsCI_pd_output_bootstrap.csv", sep = ""))
  mpd_data_temp<- read.csv(paste(i, "/0507_ecoregionsCI_mpd_output_bootstrap.csv", sep = ""))
  mntd_data_temp<- read.csv(paste(i,"/0507_ecoregionsCI_mntd_output_bootstrap.csv", sep = ""))
  
  pdf(file=paste( i, "/pd_fit_baro5.pdf", sep = ""))
  pd_model_temp<-as_tibble(t(surfaceGen(pd_data_temp, "pd", func = baro5())), rownames = "key") #need to make models. 
  dev.off()
  pdf(file=paste(i, "/mpd_fit_baro5.pdf", sep = ""))
  mpd_model_temp<-as_tibble(t(surfaceGen(mpd_data_temp, "mpd", func = baro5())), rownames = "key")
  dev.off()
  pdf(file=paste(i, "/mntd_fit_baro5.pdf", sep = ""))
  mntd_model_temp<-as_tibble(t(surfaceGen(mntd_data_temp, "mntd", func = baro5())), rownames = "key")
  dev.off()
  
  json_pd<- toJSON(x = pd_model_temp, dataframe = 'rows', pretty = F)
  write(json_pd, file = paste(i, "/pd_model_params_baro5.json", sep = ""))
  
  json_mpd<- toJSON(x = mpd_model_temp, dataframe = 'rows', pretty = F)
  write(json_mpd, file = paste(i,"/mpd_model_params_baro5.json", sep = ""))
  
  json_mntd<- toJSON(x = mntd_model_temp, dataframe = 'rows', pretty = F)
  write(json_mntd, file = paste(i, "/mntd_model_params_baro5.json", sep = ""))
  
}

#penalizes complexity. 

#also need to get the pd, mpd and mntd statistics for each ecoregion like was done for the hex data. 

