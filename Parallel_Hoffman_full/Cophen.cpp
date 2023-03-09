#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

/* 
 * FUNCTIONS TO GENERATE VARIANCE-COVARIANCE MATRIX from an S3 'phylo' object from the R-package ape (Paradis E, J Claude, and K Strimmer 2004 [BIOINFORMATICS 20:289-290])
 * author: Jonathan M Eastman 01.11.2011
 */

// [[Rcpp::export]]
void sortedges(std::vector<double> const &unsortededges,
               std::vector<double> &edges,
               std::vector<int> const &des)
{
  std::vector<int>::size_type i,j;
  //int i, j;
  for(i=0; i<edges.size(); i++) {
    for(j=0; j<des.size(); j++){
      if(des.at(j)==(signed)i+1)
      {
        edges.at(i)=unsortededges.at(j);                             
      }
    }
  }
}

// [[Rcpp::export]]
void gatherdescendants(int const &node,
                       int const &root,
                       int const &endofclade,
                       std::vector<int> &TIPS,
                       std::vector<int> const &anc, 
                       std::vector<int> const &des)
{
  /* 
   * node: current internal node
   * root: most internal node
   * endofclade: number of rows in the phylogenetic edge matrix (of the 'phylo' object)
   * TIPS: vector in which to store all (tip) descendants of node (by node label)
   * anc: first column of 'phylo' edge matrix (e.g., phy$edge[,1])
   * des: second column of 'phylo' edge matrix (e.g., phy$edge[,2])
   */
  
  int i, eoc=endofclade, r=root, n=node;
  
  for(i=0; i<eoc; i++) {
    if(anc.at(i) == n)
    {
      if(des.at(i)>r)
      {
        gatherdescendants(des.at(i), r, eoc, TIPS, anc, des);
      }
      else 
      {
        TIPS.push_back(des.at(i));
        
      }
    }
  }
}


// [[Rcpp::export]]
void descend_vcv(int const &node,
                 double const &edge,
                 int const &root,
                 int const &endofclade,
                 std::vector<int> const &anc, 
                 std::vector<int> const &des, 
                 std::vector<double> &vcv)
{
  /* 
   * node: internal node of which descendants are recorded
   * edge: length to add to covariances of all descendants where 'node' is internal
   * root: node to initiate vcv update
   * endofclade: rows in the phylogenetic edge matrix (of the 'phylo' object)
   * anc: first column of phylo edge matrix (e.g., phy$edge[,1]
   * des: second column of phylo edge matrix (e.g., phy$edge[,2]
   * vcv: the current variance-covariance matrix
   * TIPS: an empty vector in which to store all (tip) descendants of node
   */
  
  int n;
  std::vector<int>::size_type a,b,j;
  n=root-1;
  std::vector<int> TIPS;    
  TIPS.reserve(root-1);
  
  gatherdescendants(node, root, endofclade, TIPS, anc, des);
  
  /* add 'edge' length of 'node' to covariances V[a,b] where a and b are tips descended from 'node' and a!=b [OFFDIAGONAL ELEMENTS in V] */
  for(a=0; a<TIPS.size(); a++) {
    for(b=a; b<TIPS.size(); b++) {
      if(a!=b)
      {
        vcv[ TIPS.at(a)-1 + n*( TIPS.at(b)-1 ) ] = vcv[ TIPS.at(b)-1 + n*(TIPS.at(a)-1 ) ] += edge;
      }
    }
  }
  
  /* add 'edge' length of 'node' to variances of V[a,b] where a and b are tips descended from 'node' and a==b [DIAGONAL ELEMENTS in V]*/ 
  for(j=0; j<TIPS.size(); j++) {
    vcv[ TIPS.at(j)-1 + n*( TIPS.at(j)-1 ) ] += edge;
  }
  
  std::vector<int>().swap(TIPS);
}


// [[Rcpp::export]]
void vcv_to_cophenetic (int const &root,
                        std::vector<double> &vcv,
                        std::vector<double> &cophen)
{
  /* 
   * root: most internal node
   * vcv: the current state of the variance-covariance matrix 
   */
  
  double tmp, aDiag, bDiag;
  int a, b, r=root, nn, n;
  nn=r-1;
  
  /* cycle through lower triangle of VCV matrix, computing the cophenetic distance of each element */
  for(a=0; a<nn; a++) {
    aDiag = vcv[ a + nn*a ];
    for(b=a+1; b<nn; b++) {
      bDiag = vcv[ b + nn*b ];
      tmp = vcv[ b + nn*a ];
      cophen[ a + nn*b ] = cophen[ b + nn*a ] = (aDiag+bDiag) - 2*tmp;
    }
  }
  
  /* assign 0 to diagonal in V */
  for(n=0; n<nn; n++) {
    cophen[ n + nn*n ] = 0;
  }
  
  
}

// [[Rcpp::export]]
void vcv_internal (int const &maxnode,
                   int const &root,
                   int const &endofclade,
                   std::vector<int> const &anc, 
                   std::vector<int> const &des, 
                   std::vector<double> const &edges, 
                   std::vector<double> &vcv)
{
  /* 
   * maxnode: largest internal node
   * root: most internal node
   * endofclade: number of rows in the phylogenetic edge matrix (of the 'phylo' object)
   * anc: first column of phylo edge matrix (e.g., phy$edge[,1]
   * des: second column of phylo edge matrix (e.g., phy$edge[,2]
   * edges: sorted branch lengths (by node label), including the root (ntips+1)
   * vcv: the current state of the variance-covariance matrix 
   */
  
  int n, nn, r=root, mx=maxnode, eoc=endofclade;    
  nn=r-1;
  
  /* cycle over internal nodes within the tree; tree stem does not contribute to VCV so is skipped */
  for(n=r+1; n<=mx; n++) {
    
    /* find descendants of n; add edge length of n to variances for tips and covariances among tips */    
    descend_vcv(n, edges.at(n-1), r, eoc, anc, des, vcv);
    
  }
  
  /* add leaves to lengths in V[n,n] where n is a tip [DIAGONAL ELEMENTS in V]*/
  for(n=1; n<r; n++) {
    vcv[ n-1 + nn*( n-1 ) ] += edges.at(n-1);
  }
}
// [[Rcpp::export]]
RcppExport SEXP cophen (SEXP tree) 
{
  /* 
   * tree: a list of elements 
   * ROOT: most internal node
   * MAXNODE: least internal internal node (and largest valued in edge matrix)
   * ENDOFCLADE: rows in edge matrix
   * ANC: first column of 'phylo' edge matrix (e.g., phy$edge[,1])
   * DES: second column of 'phylo' edge matrix (e.g., phy$edge[,2])
   * EDGES: edge lengths, sorted by node label and including the root (at position Ntip(phy)+1)
   * COPHEN: the current state of the variance-covariance matrix, initialized with 0s in R
   */
  
  try {
    std::vector<int>::size_type i;
    
    /* call in parameters associated with 'phylo' object */
    Rcpp::List phylo(tree);
    
    int maxnode =  Rcpp::as<int>(phylo["MAXNODE"]);
    int root = Rcpp::as<int>(phylo["ROOT"]);
    int endofclade =  Rcpp::as<int>(phylo["ENDOFCLADE"]);
    std::vector<int> anc=phylo["ANC"];
    std::vector<int> des=phylo["DES"];
    std::vector<double> unsortededges=phylo["EDGES"];
    std::vector<double> edges=phylo["EDGES"];
    std::vector<double> V=phylo["COPHEN"];
    std::vector<double> cophen=V;
    
    /* initialize edges */
    for(i=0; i<edges.size(); i++) {
      edges.at(i)=0;
    }        
    
    /* sort edges by node label */
    sortedges(unsortededges, edges, des);
    
    /* call to primary function that updates VCV matrix */
    vcv_internal(maxnode, root, endofclade, anc, des, edges, V);
    
    /* call to converter function for VCV to COPHENETIC */
    vcv_to_cophenetic(root, V, cophen);
    
    /* PREPARE OUTPUT FOR R */
    return Rcpp::List::create(Rcpp::Named("COPHEN",cophen));
    
    
  } catch( std::exception &ex ) {        
    forward_exception_to_r( ex );
  } catch(...) { 
    ::Rf_error( "C++ exception: unknown reason" ); 
  }
  return R_NilValue; 
}


