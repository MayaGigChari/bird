#in this script I will create parameter combinations and paralellize everything in hoffman 
#TODO: make everything into functions. 

#define global variables/ variables to change. 

#number of randomizations

#local variables for mod. 
#TODO: figure out a way to change these local variables without interacting with the code. 

n_rand = 1000 #want 500 randomizations for each tree size. 
params = seq(55, 300, by=5) #this is a list of tree sizes. 

#need 50000 randomizations. 

new_libPaths = .libPaths(c('/u/home/m/mchari/R',.libPaths()))
.libPaths(new_libPaths)
print(.libPaths())


#create new directories to hold outputs
a<-Sys.getenv(x = "JOB_ID")

folder <- paste(a, "output_files", sep = "_")
folder2 <- paste(a, "times", sep = "_")
if (file.exists(folder))
{
  
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




#install.packages("phylocomr",repos = "http://cran.us.r-project.org")
#install.packages("ape", repos = "http://cran.us.r-project.org", lib = "/u/home/m/mchari/R")
library("phylocomr", lib.loc = .libPaths())
library("ape", lib.loc = .libPaths())

#these are tree sizes of "sample input trees" In this case these are the "params" we will be parallelizing over. 


args = commandArgs(trailingOnly=TRUE) #not sure what this part does, but I think it allows us to accept args when the script is sent. 
print(args) #prints the arguments which here should be equivalent to each SGE_TASK_ID
n = as.integer(args[1]) #makes the SGE_TASK_ID an integer

#this line separates and assigns jobs to different parameter combinations (in this case tree sizes )


current_index = n %/% n_rand +1 

sample_size = params[current_index] #this gives us the "sample size"/tree size for the current run. 

print("args successfully loaded") #another checkpoint

#load the tree

sample_tree_filename = paste("sample_tree_cluster", sample_size, sep = "") #folder that contains all the null generate tree data. 
sample_tree_cluster<-read.tree(file = paste("~/bird/Parallel_Hoffman_full/sample_trees/", sample_tree_filename, sep = "")) 
full_tree_cluster<-read.tree(file = "full_tree_for_cluster.tre") #this file should be contained in the same working directory. 

print("trees read")

df_touse_cluster<-data.frame(sample = "species", occurrence = 1, names = sample_tree_cluster$tip.label) #create a dataframe compatibile with phcomstruct function
res_sample_cluster<- ph_comstruct(sample = df_touse_cluster, phylo = full_tree_cluster, randomizations = 1) #the real bulk of the program. Not sure why 2 randomizations but will keep this.  

#typeof(res_sample_cluster)
#write.table(res_sample_cluster, file = "sample_phcomstruct_output") #better to have csv maybe, easier to manipulate?

print("ph_comstruct successfully applied")

path_to_output <- "/u/home/m/mchari/bird/Parallel_Hoffman_full"
#fileName = paste(path, "phcom_out",n,".csv", sep = '')
#fileName_time = paste(path, "phcom_out",n,"time.csv", sep = '')#filepath for time outputs. 

path_csv<-file.path(path_to_output, folder)#trying to formally make file paths
path_txt<-file.path(path_to_output,folder2)

print(path_csv)
print(path_txt)

print(sample_size)

fileIDcsv = paste(path_csv,"/",sample_size,"_",n, "out.csv", sep = '') #this fileID can be applied to all outputs.
fileIDtimes = paste(path_txt,"/",sample_size,"_",n, "time.csv", sep = '') #this fileID can be applied to$

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
