#need to make this more generalizable. 
#list of variables that need to be generalized: 

#n_rand 
#params1/params
#num_randomizations
#sample_tree_filename (right now specific to birds)
#sample_tree_cluster (right now specific to birds)
#cophen_read (right now specific to birds)
#path_to_output
#take advantage of the fact that this script is in the working directory 

n_rand = 1 #want 2  randomizations for each tree size. internally each script will produce 500 random values. Can also do 1? 
min_null_tree_size = 5
max_null_tree_size = 100
tree_size_increment = 5
Clade = "squamate" #need to make it so this can be any clade. 
num_randomizations = 1000
username = "mchari" #here edit to make hoffman 2 username.

#above is all local variable declarations. 

path_to_output <- paste(getwd(), '/', Clade, sep = "") #this is fairly universal. 
new_libPaths = .libPaths(c(paste('/u/home/m/',username, '/R', sep = ""),.libPaths())) #library path for cluster
.libPaths(new_libPaths)

print("paths created")

params = seq(min_null_tree_size, max_null_tree_size, by=tree_size_increment) 


#this seems to have an issue. 
#create new directories to hold outputs


a<-Sys.getenv(x = "JOB_ID")


folder <- paste(path_to_output, "/", a, "_output_files", sep = "")
folder2 <- paste(path_to_output, "/",a, "_times", sep = "")
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

#install.packages("picante",repos = "http://cran.us.r-project.org")
#install.packages("dplyr",repos = "http://cran.us.r-project.org")
#install.packages("ape",repos = "http://cran.us.r-project.org")
#library("picante", lib.loc = .libPaths())
#library("ape", lib.loc = .libPaths())
#library("dplyr", lib.loc = .libPaths())

#try this and see if it works.

packages_to_install <- c("picante", "dplyr", "ape")

for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "http://cran.us.r-project.org", lib = new_libPaths)
  }
}

library(picante)
library(ape)
library(dplyr)

#print("libraries successfully loaded)

args = commandArgs(trailingOnly=TRUE) #not sure what this part does, but I think it allows us to accept args when the script is sent. 
print(args) #prints the arguments which here should be equivalent to each SGE_TASK_ID.
n = as.integer(args[1]) #makes the SGE_TASK_ID an integer

current_index = n %/% n_rand +1 
sample_size = params[current_index] #this gives us the "sample size"/tree size for the current run. 
print(sample_size)
print("args successfully loaded") #another checkpoint

#here we will load all the data. we don't even need the larger tree here! Just need the sample tree.  
sample_tree_filename = paste("sample_tree_cluster", sample_size, sep = "") #folder that contains all the null generate tree data. 
sample_tree_cluster<-read.tree(file = paste("~/bird/Parallel_Hoffman_full/",Clade, "/sample_trees/", sample_tree_filename, sep = "")) 
full_tree<-read.tree(file =  paste("~/bird/Parallel_Hoffman_full/", Clade, "/cali_tree.tre",sep = ""))
cophen_read<-readRDS(paste("~/bird/Parallel_Hoffman_full/", Clade, "/cali_cophen_matrix",sep = ""))

commdata_maker<- function(tree)
{
  comm<-data.frame(row.names = tree$tip.label, clump1 = 1, clump2 = tree$tip.label)
  comm2<-subset(comm, select = clump1)
  return(t(as.matrix(comm2)))
}

comm_sample<-commdata_maker(sample_tree_cluster)

v <- c(rep(1, num_randomizations))

vectorized_stats_calculation<- function(v, coph_mat)
{
  #comm_sample<-commdata_maker(sample_tree)
  a<-coph_mat
  mpd<- picante::mpd(comm_sample, picante:::taxaShuffle(a))
  mntd<-picante::mntd(comm_sample, picante::taxaShuffle(a))
  pd<-picante::pd(comm_sample,picante:::tipShuffle(full_tree))
  r<-c(pd$PD,mpd,mntd)
  names(r)<-c("pd","mpd","mntd")
  l<-r
  #v<-(picante::mpd(comm_sample, coph_mat)) 
}

stats<-lapply(v, vectorized_stats_calculation, coph_mat = cophen_read)


#prepare the data for export and export the data. 
temp <- unlist(stats[1])
if(num_randomizations>1)
{
  for(i in 2:num_randomizations)
  {
    temp<-cbind(unlist(stats[i]), temp)
  }
}
colnames(temp) = rep("trial", num_randomizations)
res_sample_cluster<-temp


#path_to_output <- paste("/u/home/m/mchari/bird/Parallel_Hoffman_full/", Clade, sep = "")
#path_csv<-file.path(path_to_output, folder)#trying to formally make file paths
#path_txt<-file.path(path_to_output,folder2)

path_csv<-file.path(folder)#trying to formally make file paths
path_txt<-file.path(folder2)

fileIDcsv = paste(path_csv,"/",sample_size,"_",n, "out.csv", sep = '') #this fileID can be applied to all outputs.
fileIDtimes = paste(path_txt,"/",sample_size,"_",n, "time.csv", sep = '') #this fileID can be applied to

#checkpoint
print(fileIDcsv)
print(fileIDtimes)

write.csv(res_sample_cluster, file = fileIDcsv, row.names = FALSE) #this i believe is the output file. I think it's maybe better than table?

#checkpoint
print("data output successful")

#stuff for time calculation
end_time<-format(Sys.time(), "%H:%M:%S") #identify end time of script

times = c(start_time, end_time)
point = c(1,2)
df_times = data.frame (time = times, point = point)
write.csv(df_times, file = fileIDtimes)

#end of file. 


