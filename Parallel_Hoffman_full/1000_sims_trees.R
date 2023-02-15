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
    name = "mpd.true"
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

# get the 95% confidence intervals
CI<- CI_calc(mpd_metrics) #calculate CI's

#find pd data that is outside the extremes of the CI and correlated tree data

pd_below_range_index<- which(mpd_metrics < CI[1]) #indexes the same through the pd vector and the trees vector. 
pd_above_range_index<- which(mpd_metrics > CI[2])

trees_above_CI<- trees[c(pd_above_range_index)]
trees_below_CI<-trees[c(pd_below_range_index)]



