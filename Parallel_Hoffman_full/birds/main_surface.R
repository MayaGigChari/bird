packages_to_install <- c("picante", "ape", "stringr", "dplyr", "phytools", "data.table", "drc", "jsonlite")

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



clade = "birds"

current_directory <- getwd()

Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)

#get the data
pd_data <- retrieve_data(clade, "pd")
mpd_data <- retrieve_data(clade, "mpd")
mntd_data <- retrieve_data(clade, "mntd")

#generate the models
pd_model<-surfaceGen(pd_data, "pd")
mpd_model<-surfaceGen(mpd_data, "mpd")
mntd_model<-surfaceGen(mntd_data, "mntd")

#wite model coefficients to csv files for visualization. 
write.csv(pd_model, "pd_model_params.csv", row.names = TRUE)
write.csv(mpd_model, "mpd_model_params.csv", row.names = TRUE)
write.csv(mntd_model, "mntd_model_params.csv", row.names = TRUE)



