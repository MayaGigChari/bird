library(ape)
library(phylocomr)
library(tictoc)


full_tree<-read.tree("full_tree_for_cluster.tre")
species_names<-full_tree$tip.label

cov_maker<- function(tree)
{
  cov<-data.frame(sample = "species", occurrence = 1, names = tree$tip.label)
}

comstruct_aid<-function(cov_mat)
{
  ph_comstruct(cov_mat, full_tree, randomizations = 1)
}

metric_extract<-function(data, metrics) #vector data is in the form of ph_comstruct output file 
{
  if(metrics == "mpd_random")
    name = "mpd.rnd"
  else if(metrics == "mpd_true")
    name = "mpd"
  else if(metrics == "mntd")
    name = "mntd.rnd"
  metric_list<-unlist(data)
  return(metric_list[name])
  
}

CI_calc<- function(mpd_metrics_list)
{
  mpd_metrics_unlist<-(as.numeric(unlist(mpd_metrics_list)))
  lower_bound<- quantile(mpd_metrics_unlist,.025)
  upper_bound<-quantile(mpd_metrics_unlist,0.975)
  return(c(lower_bound, upper_bound))
}

tree_writer<- function(tree_list)
{
  write.tree(tree_list)
}

tic()
randomizations = rep(5, each = 1000)
samples<-lapply(randomizations, sample, x = species_names)
trees<-lapply(samples, keep.tip, phy = full_tree)
dfs_touse<- lapply(trees, cov_maker)
res_sample<-lapply(dfs_touse, comstruct_aid)
toc()

mpd_rand_metrics<- lapply(res_sample, metric_extract, metrics = 'mpd_random')
mpd_true_metrics<-lapply(res_sample,metric_extract, metrics = "mpd_true")

write.csv(res_sample, file = "pd_calcs_sim.csv")
# the tic toc functions just give us some measure of time.. this program took 11 hours to run!

#visualizing the distribution of the true mpd values for all our random trees. 

mpd_vals_hist<-as.numeric((unlist(mpd_true_metrics)))
hist(mpd_vals_hist, main = "mpd's of 1000 random 5-tip trees",xlab = "mpd")

# get the 95% confidence intervals
CI_rand<- CI_calc(mpd_rand_metrics) #calculate CI's
CI_true<-CI_calc(mpd_true_metrics)
#find pd data that is outside the extremes of the CI and correlated tree data

mpd_below_range_index_true<- which(mpd_true_metrics < CI[1]) #indexes the same through the pd vector and the trees vector. 
mpd_above_range_index_true<- which(mpd_true_metrics > CI[2])

trees_above_CI_true<- trees[c(mpd_above_range_index_true)]
trees_below_CI_true<-trees[c(mpd_below_range_index_true)]

mpd_above_CI_true<- mpd_true_metrics[c(mpd_above_range_index_true)]
mpd_below_CI_true<-mpd_true_metrics[c(mpd_below_range_index_true)]


for(i in 1:length(trees_above_CI_true))
{
  write.tree(trees_above_CI_true[i], file = paste("above_CI_data/tree_above_CI",i, sep = ''))
  write.csv(mpd_above_CI_true[i], file = paste("above_CI_data/mpd_above_CI",i,sep = ''))
}

for(i in 1:length(trees_below_CI_true))
{
  write.tree(trees_below_CI_true[i], file = paste("below_CI_data/tree_below_CI",i, sep = ''))
  write.csv(mpd_below_CI_true[i], file = paste("below_CI_data/mpd_below_CI",i,sep = ''))
}


write.csv(as.numeric(unlist(mpd_true_metrics)),file= "mpd_metrics.csv",row.names=F)
