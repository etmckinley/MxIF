#' MxIF tSNE
#'
#' This function runs 2D tSNE on a data set, user specifies columns for processing.
#' Eliot McKinley 9/15/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param markers columns for each marker to run tSNE on
#' @param seed set seed for tSNE, default=42
#' @param perplexity, set perplexity for tSNE, default=50
#' @param iterations set iterations for tSNE , default=1000
#' @param verbose whether to output tSNE iteration steps to console, default=true
#' @keywords tSNE MxIF
#' @export
#' @examples
#' MxIF.tsne(data, c(1,2, 5:9))
#' MxIF.tsne(data, c(1:5, 6 ,8) seed=50, perplexity=30, iterations= 500, verbose=FALSE)
#' 

MxIF.tsne <- function(data, markers, seed=42 , perplexity=50, iterations=1000, verbose=TRUE){
  
  set.seed(seed)
  rtsne_out <- Rtsne(as.matrix(data[,markers]), verbose=verbose, perplexity=perplexity, max_iter=iterations, dims=2)
  
  #insert tSNE coordinates into table
  data$tSNE1= rtsne_out$Y[,1] 
  data$tSNE2= rtsne_out$Y[,2]
  
  return(data)
  
}