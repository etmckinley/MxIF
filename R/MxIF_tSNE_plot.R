#' MxIF tSNE plot
#'
#' This function plots tSNE results. N.B. column names for plotting must be tSNE1 and tSNE2
#' Eliot McKinley 9/15/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param marker column to display
#' @param colors what colors to use, default is "colorblind" friendly, can also choose "threecolor" black-red-blue, or set your own
#' @param sz, what size dot to display, default=1.5
#' @param highQ  max percentile default=.98
#' @param lowQ  max percentile default=.02
#' @param legend whether to show legend, default=FALSE
#' @keywords tSNE MxIF
#' @export
#' @examples
#' MxIF.tsne.plot(data, 3)
#' MxIF.tsne.plot(data, 3, colors="threecolor")
#' MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"))
#' MxIF.tsne.plot(data, 3, colors=c("#000000","#98EF56"), sz=2, highQ=0.95, lowQ=0.05)

MxIF.tsne.plot <- function(data, marker,  colors= "colorblind", sz=1.5, highQ=0.98, lowQ=0.02, legend=FALSE){
  
  library(ggplot2) 
  #set colors
  if (colors[1] == "colorblind"){
    colors=c("#000000", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7","#fddbc7", "#f4a582", "#d6604d", "#b2182b")
  }
  if (colors[1] =="threecolor"){
    colors=c("#000000", "#920000", "#006ddb")
  }
  
  #get max_min_limit
  max_limit=  as.numeric(quantile(data[,marker], highQ, na.rm=TRUE) ) #median_val+sd_num*sd_val
  min_limit=as.numeric(quantile(data[,marker], lowQ, na.rm=TRUE) )
  if (max_limit==min_limit){
    max_limit=min_limit+0.00001
  }

  if (legend==FALSE){
    p=ggplot(data, aes(tSNE1,tSNE2) )+
      geom_point(aes(color=data[,marker]) ,size=sz, na.rm=TRUE)+
      scale_color_gradientn(colors=colors, limits=c(min_limit,max_limit), na.value=colors[length(colors)])+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_blank(),
            text = element_text(size=14), axis.title=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
            plot.background=element_blank(), legend.position="none", 
            legend.title = element_blank(), legend.key = element_blank())
    
  }

  if (legend==TRUE){
    p=ggplot(data, aes(tSNE1,tSNE2) )+
      geom_point(aes(color=data[,marker]) ,size=sz, na.rm=TRUE)+
      scale_color_gradientn(colors=colors, limits=c(min_limit,max_limit), na.value=colors[length(colors)], breaks=c(min_limit,max_limit), labels=c("low", "high"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_blank(),
            text = element_text(size=14), axis.title=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
            plot.background=element_blank(), legend.position=c(.15,.92), legend.direction="horizontal", 
            legend.title = element_blank(), legend.key = element_rect(fill="transparent", color="transparent"))
    
  }

return(p)

}