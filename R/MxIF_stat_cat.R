#' MxIF Stat Cat
#'
#' This function concatenates stats files into one CSV.
#' Eliot McKinley 10/4/17
#' @param file location of folder containing stats files
#' @keywords stats concatenate MxIF
#' @export
#' @examples
#' MxIF.stat.cat(file)

MxIF.stat.cat <- function(file){
  stats.files=list.files(path = file, pattern="\\.csv$", full.names = TRUE)
  alldata=NULL
  for (i in 1:length(stats.files)){
    dat0=read.csv(file=stats.files[i], header=TRUE)
    alldata=rbind(alldata,dat0)
  }
  
  return(alldata)  
}
