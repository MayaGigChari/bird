#main for birds 

#get the current working directory 
parallel_hoffman_full_path <- normalizePath("../Parallel_Hoffman_full/birds") #this constructs a path to one level above the current path

# Set the path to cophen.R using a relative path


# Source cophen.R
setwd(parallel_hoffman_full_path)
coph_path = ("Cophen_function.R")
source(coph_path)
setwd(current_directory)

# Call the function from cophen.R


cophen_function()

# Save outputs to the local directory "birds"
output_path <- file.path(current_directory, "birds", "output.txt")
cat("Output from cophen_function", file = output_path)