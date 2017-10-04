#' MxIF Cell Filtering
#'
#' This function returns cells filtered for specified properties.
#' Eliot McKinley 9/15/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param MinCellSize minumum cell area (pixels), default=150
#' @param MinCellSize maximum cell area (pixels), default=2000
#' @param MinNucSize minumum nuclear area (pixels), default=30
#' @param MaxNucSize maximum nuclear area (pixels), default=1000
#' @param MinMemSize minimum membrane area (pixels), default=20
#' @param MinCytSize minimum cytoplasm area (pixels), default=0
#' @keywords filter MxIF
#' @export
#' @examples
#' cell.filter(data)
#' cell.filter(data, MinCellSize=50, MaxCellSize=2500, MinNucSize=20, MaxNucSize=1500, MinMemSize=50, MinCytSize=10)

MxIF.cell.filter <- function(data, MinCellSize=150, MaxCellSize=2000, MinNucSize=30, MaxNucSize=1000, MinMemSize=20, MinCytSize=0 ){
  data=subset(data, Cell_Area > MinCellSize)
  data=subset(data, Cell_Area < MaxCellSize)
  data=subset(data, Nuc_Area > MinNucSize)
  data=subset(data, Nuc_Area < MaxNucSize)
  data=subset(data, Mem_Area > MinMemSize)
  data=subset(data, Cyt_Area > MinCytSize)

  return(data)
  }
