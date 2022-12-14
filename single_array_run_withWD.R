#script for one run of a ph_comstruct; to run in parallel. 
#todo: edit output so that it is task ID AND jobID. 
#this script just gets environmental variables 



a<-Sys.getenv(x = "JOB_ID")
#this is the syntax for accessing one element in the environmental variable list.

folder <- paste(a, "output_files", sep = "_")
folder2 <- paste(a, "times", sep = "_")
if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}
if (file.exists(folder2)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder2)
}

#created new directories to hold the outputs.   



start_time<-format(Sys.time(), "%H:%M:%S") #identify start time of script

#install.packages("phylocomr", repos = "http://cran.us.r-project.org")
#install.packages("ape", repos = "http://cran.us.r-project.org")
library(phylocomr)
library(ape)

args = commandArgs(trailingOnly=TRUE) #not sure what this part does, but I think it allows us to accept args when the script is sent. 
print(args) #prints the arguments which here should be equivalent to each SGE_TASK_ID
n = as.integer(args[1]) #makes the SGE_TASK_ID an integer

print("args successfully loaded")

sample_tree_cluster<-read.tree(file = "sample_tree_for_cluster.tre")

full_tree_cluster<-read.tree(file = "full_tree_for_cluster.tre")

print("trees read")

df_touse_cluster<-data.frame(sample = "species", occurrence = 1, names = sample_tree_cluster$tip.label)
res_sample_cluster<- ph_comstruct(sample = df_touse_cluster, phylo = full_tree_cluster, randomizations = 1)
typeof(res_sample_cluster)
#write.table(res_sample_cluster, file = "sample_phcomstruct_output") #better to have csv maybe, easier to manipulate?

print("ph_comstruct successfully applied")

path_to_output <- "/u/home/m/mchari/bird"
#fileName = paste(path, "phcom_out",n,".csv", sep = '')
#fileName_time = paste(path, "phcom_out",n,"time.csv", sep = '')#filepath for time outputs. 

path_csv<-file.path(path_to_output, folder)#trying to formally make file paths
path_txt<-file.path(path_to_output,folder2)

print(path_csv)
print(path_txt)

fileIDcsv = paste(path_csv,"/",n, "out.csv", sep = '') #this fileID can be applied to all outputs.
fileIDtimes = paste(path_txt,"/", n, "time.csv", sep = '') #this fileID can be applied to$

print(fileIDcsv)
print(fileIDtimes)

write.csv(res_sample_cluster, file = fileIDcsv) #this i believe is the output file. I think it's maybe better than table?

print("data output")

end_time<-format(Sys.time(), "%H:%M:%S") #identify end time of script

times = c(start_time, end_time)
point = c(1,2)
df_times = data.frame (time = times, point = point)
write.csv(df_times, file = fileIDtimes)

#write.table(times_list, file = fileIDtxt)
#write.table(c(start_time, end_time), file = folder2))

#print("times output")
