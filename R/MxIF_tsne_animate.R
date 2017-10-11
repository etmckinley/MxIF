#' MxIF tSNE animate
#'
#' This function plots tSNE results as an animated gif. N.B. column names for plotting must be tSNE1 and tSNE2
#' Eliot McKinley 9/15/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param markers columns to display
#' @param file where to save data
#' @param colors what colors to use, default is "colorblind" friendly, can also choose "threecolor" black-red-blue, or set your own
#' @param sz, what size dot to display, default=1.5
#' @param highQ  max percentile default=.98
#' @param lowQ  max percentile default=.02
#' @keywords tSNE MxIF
#' @export
#' @examples
#' MxIF.tsne.animate(posData, markers, file)
#' MxIF.tsne.animate(posData, markers, file, colors="threecolor")
#' MxIF.tsne.animate(posData, markers, file, colors=c("#000000","#98EF56"))
#' MxIF.tsne.animate(posData, markers, file, colors=c("#000000","#98EF56"), sz=2, highQ=0.95, lowQ=0.05)

MxIF.tsne.animate <- function(data, markers, file,  colors= "colorblind", sz=1.5, highQ=0.98, lowQ=0.02){
  options(warn = -1) 
  library(ggplot2)
  library(magick)
  library(reshape)
  
  #set colors
  if (colors[1] == "colorblind"){
    colors=c("#000000", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7","#fddbc7", "#f4a582", "#d6604d", "#b2182b")
  }
  if (colors[1] =="threecolor"){
    colors=c("#000000", "#920000", "#006ddb")
  }
  
  
  img <- image_graph(1000, 1000, res = 96)
  data=data[,c(markers, ncol(data)-1, ncol(data))]
  data=melt(data , id=c("tSNE1","tSNE2"))
  data=split(data,data$variable)
  out=lapply(data,function(data){
    max_limit=  as.numeric(quantile(data$value, highQ, na.rm=TRUE) )
    min_limit=as.numeric(quantile(data$value, lowQ, na.rm=TRUE) )
    p=ggplot(posData, aes(tSNE1,tSNE2) )+geom_point(aes(color=data$value) ,size=sz, na.rm=TRUE)+
      scale_color_gradientn(colors=colors, limits=c(min_limit,max_limit), na.value=colors[length(colors)])+#,  breaks=c(min_limit,max_limit), labels=c("low", "high"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_blank(),
            text = element_text(size=14), axis.title=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
            plot.background=element_blank(), legend.position="none", 
            legend.title = element_blank(), legend.key = element_blank())+
      ggtitle(data$variable)

     print(p)
    
  } )
  dev.off()
  
  img <- image_background(image_trim(img), 'white')
  animation <- image_animate(img, fps = 1)

  image_write(animation, file)
  options(warn=0)
}
