#' MxIF Cell Filter View
#'
#' This function outputs visulaization of cells filtered.
#' Eliot McKinley 9/25/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param LABELS tiff image of cell segmentation (ID=grayscale value)
#' @param NOVLP png image of the Non-overlap segmentation results
#' @keywords cell filter MxIF
#' @export
#' @examples
#' MxIF.tsne.plot(data, 3)
#' MxIF.tsne.plot(data, 3, colors="threecolor")
#' MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"))
#' MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"), sz=2, highQ=0.95, lowQ=0.05)

MxIF.cell.filter.view <- function(data, LABELS, NOVLP){

  library(tiff)
  library(png)

  NOVLP.R=NOVLP[,,1]
  NOVLP.G=NOVLP[,,2]
  NOVLP.B=NOVLP[,,3]

  #filter labels to binary of cells not filtered out
  LABELS.filt=ifelse(LABELS %in%data$ID, 1, 0)
  LABELS.filt=matrix(LABELS.filt, nrow=nrow(NOVLP), byrow=FALSE)

  lines=NOVLP.R*NOVLP.G

  NOVLP.R[LABELS.filt==0]=0.5
  NOVLP.R[LABELS==0]=0
  NOVLP.R[lines>0]=1
  NOVLP.R[NOVLP.B==1 & lines!=1]=0

  NOVLP.G[LABELS.filt==0]=0.5
  NOVLP.G[LABELS==0]=0
  NOVLP.G[lines>0]=1
  NOVLP.G[NOVLP.B==1 & lines !=1]=0

  NOVLP.B[LABELS.filt==0]=0.5
  NOVLP.B[LABELS==0]=0
  NOVLP.B[NOVLP.B==1 & NOVLP.R==1 & lines!=1]=1
  NOVLP.B[lines>0]=0


  NOVLP[,,1]=NOVLP.R
  NOVLP[,,2]=NOVLP.G
  NOVLP[,,3]=NOVLP.B

  return(NOVLP)
}
