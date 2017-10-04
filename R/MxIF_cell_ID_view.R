#' MxIF Cell ID View
#'
#' This function outputs red overlay of selected cells.
#' Eliot McKinley 9/27/17
#' @param AFRemoved Image underlay for cell ID view
#' @param LABELS tiff image of cell segmentation (ID=grayscale value)
#' @param IDs array of cell IDs to overlay
#' @param alpha transparency or red overlay
#' @keywords cell filter viewer Ids MxIF
#' @export
#' @examples
#' MxIF.cell.ID.view(AFRemoved, LABELS, c(100,200), alpha=.2)


MxIF.cell.ID.view <- function(AFRemoved, LABELS, IDs, alpha=.3){
  
  library(tiff)
  library(png)
  
  #create RGB array with AFRemoved image
  Out=array(0,c(nrow(AFRemoved),ncol(AFRemoved),3))
  Out[,,1:3]=AFRemoved
  Red=Out[,,1]
  
  #find the cell ID requested
  LABELS.filt=ifelse(LABELS %in%IDs, 1, 0)
  LABELS.filt=matrix(LABELS.filt, nrow=nrow(LABELS), byrow=FALSE)
  
  #add to the overlay
  Red[LABELS.filt==1]=alpha
  
  #replace the Red channel with overlay
  Out[,,1]=Red
  
  return(Out)
}