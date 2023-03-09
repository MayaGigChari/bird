#this is a script for making the cophenetic matrix for a given parent tree. It requires a cpp file that 
#contains code for the fastest produced cophenetic matrix. 
install.packages("MASS")
library(MASS)
install.packages("bigmemory")
library(bigmemory)
library(tictoc)
library(picante)
library(phylocomr)
Rcpp::sourceCpp("Cophen.cpp")

cophen <- function(phy) {
  n <- ape::Ntip(phy)
  out <- .Call(
    "cophen",
    tree = list(
      ROOT = as.integer(n + 1),
      MAXNODE = as.integer(max(phy$edge[, 1])),
      ENDOFCLADE = as.integer(dim(phy$edge)[1]),
      ANC = as.integer(phy$edge[, 1]),
      DES = as.integer(phy$edge[, 2]),
      EDGES = as.double(c(phy$edge.length, 0)),
      COPHEN = as.double(array(matrix(0, n, n)))
    )
  )
  cc <- matrix(out$COPHEN, nrow = n, byrow = FALSE)
  rownames(cc) <- colnames(cc) <- phy$tip.label
  return(cc)
}

commdata_maker<- function(tree)
{
  comm<-data.frame(row.names = tree$tip.label, clump1 = 1, clump2 = tree$tip.label)
  comm2<-subset(comm, select = clump1)
  return(t(as.matrix(comm2)))
}

full_tree<- read.tree("full_tree_for_cluster.tre")
sample_tree<- read.tree("sample_tree_for_cluster.tre")

tic()
cophen_touse<- cophen(full_tree)
cophen_touse<-as.matrix(cophen_touse)
toc()

comm_d_sample<-commdata_maker(sample_tree)

v <- c(rep(1, 5))


comm_sample<-commdata_maker(sample_tree)
 
vectorized_stats_calculation<- function(v, coph_mat)
{
  #comm_sample<-commdata_maker(sample_tree)
  a<-coph_mat
  mpd<- picante::mpd(comm_sample, picante:::taxaShuffle(a))
  mntd<-picante::mntd(comm_sample, picante::taxaShuffle(a))
  pd<-picante::pd(comm_sample,picante:::tipShuffle(full_tree))
  r<-c(pd$PD,mpd,mntd)
  #names(r)<-c("pd","mpd","mntd")
  l<-r
  #v<-(picante::mpd(comm_sample, coph_mat)) 
}

vectorized_mpd_calculation(sample_tree, cophen_read)

library(dplyr)

stats<-lapply(v, vectorized_stats_calculation, coph_mat = cophen_read)

print(unlist(stats[1]))

temp = unlist(stats[1])
for(i in 2:5)
  temp<-cbind(unlist(stats[i]), temp)
colnames(temp) = rep("trial", 5)

write.csv(temp, file = "test.csv", row.names = FALSE )
read.csv("test.csv")


write.csv(matrix(stats, nrow=1), file ="myfile.csv", row.names=FALSE)

read.csv("myfile.csv")


 #instead of using picante we will use my own PD function which takes out all the fluff.
 #TODO: lab meeting ask why this is different than the true pd value??
 pdcalc<- function(tree)
 {
   return(sum(tree$edge.length))
 }
 
#here samp is in the form of a tree subset. in the function commdata_maker is called.
#operation = "mpd" or "mntd"
 
#parallelized operation: can do something file based to minimize redundancies? 

mpd_calc_cophen <- function(operation, samp, dis, phy, runs=9, cores = getOption("mc.cores")) 
{
  #added a new parameter phy, takes the parent phylogeny. 
  samp <- commdata_maker(samp)
  print(samp)
  tmpfn <- NULL
  # here we want to start with a vector of shuffled dis matrices
  if (operation %in% c("mpd", "mntd")) {
    shuffled_taxa_vec<-c()
    dis <- as.matrix(dis)
    fn <- if(operation == "mpd") picante::mpd else picante::mntd
    obs <- fn(samp, dis)
    #find a way to parallelize this. 
    tmpfn <- function(ii, samp, dis) { fn(samp, picante:::taxaShuffle(dis)) } #all he's doing here is shuffling the taxa and computing the mpd again. 
  } 
  else if (operation == "pd") {
    print("true")
    obs <- as.vector(picante::pd(samp, phy)$PD)
    tmpfn <- function(ii, samp, phy) { picante::pd(samp, picante:::tipShuffle(phy))$PD }
  } else {
    stop("unsupported operation", operation)
  }
  
  return(obs)
  rand <- t(mclapplyLB(cl = cl, X=1:runs, FUN = tmpfn, samp, dis))
  #stopCluster(cl)
  #rand.mean <- colMeans(rand, na.rm=TRUE)
  #rand.sd <- apply(X = rand, MARGIN = 2, FUN = sd, na.rm = TRUE)
  #obs.z <- (obs - rand.mean)/rand.sd
  #obs.rank <- apply(X = rbind(obs, rand), MARGIN = 2, FUN = rank)[1, ]
  #obs.rank <- ifelse(is.na(rand.mean), NA, obs.rank)
  #data.frame(ntaxa = specnumber(samp), obs, rand.mean, rand.sd, obs.rank, obs.z, obs.p = obs.rank/(runs + 1), runs = runs, row.names = row.names(samp))
}


SES_update("pd", sample, full_tree_cophen, full_tree)

pdcalc(sample_tree)

pd(sample, full_tree)

#sample has to be in that community data matrix form. 
mpd(comm_d_sample,cophen_read)

saveRDS(cophen_touse, file = "cophenetic_matrix" )
?mpd
mat_r1<- phylocom$sample[1,]
mat_r1<-t(mat_r1)

write.table(cophen_touse,file="test.txt", row.names = TRUE) # keeps the rownames
table<- read.table("test.txt", header = TRUE)
table<-as.matrix(table)
table2<-as.matrix(table)

identical(table2, cophen_touse)


cophen_read<-readRDS("cophenetic_matrix")

class(cophen_read)


#what if we set up the parallelization as only the taxa shuffle step? 
#use hoffman2 to produce a bunch of different shuffled trees and compute mpd of each tree. 
