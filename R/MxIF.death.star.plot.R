#' MxIF Create Death Star Plot following binary deconvolution
#'
#' This function creates a circular "death star" plot from binary binned data and outputs counts for each group. Title of the plot denotes the marker corresponding to each concentric ring.
#' Based upon:
#' McKinley ET, Sui Y, Al-Kofahi Y, Millis BA, Tyska MJ, Roland JT, Santamaria-Pang A, Ohland CL, Jobin C, Franklin JL, Lau KS, Gerdes MJ, Coffey RJ. Optimized multiplex immunofluorescence single-cell analysis reveals tuft cell heterogeneity. JCI Insight. 2017 Jun 2; 2(11): e93487. 
#' https://insight.jci.org/articles/view/93487
#' 
#' Eliot McKinley 10/20/17
#' @param data data frame of binary data (positive/negative) for each marker, e.g. from MxIF.deconvolution function
#' @param markers which markers to create graphs for
#' @param file string of where to save data
#' @param color hex denoted color for positive markers, default="#0709b2" - navy blue
#' @param title whether to include title with marker names for each ring, default=TRUE
#' @keywords death star binary circular plots MxIF
#' @export
#' @examples
#' MxIF.death.star.plot(data, markers, file, color="#0709b2", title=TRUE))

MxIF.death.star.plot <- function(data, markers, file, color="#0709b2", title=TRUE){
 

  
  c=count(data[,markers], colnames(data)[markers])
  c$perc=c$freq/sum(c$freq)*100
  
  c=c[order(c$perc, decreasing = TRUE),]
  
<<<<<<< HEAD
  write.csv(c, file=paste0(file, '/binary_counts_data.csv'), row.names=FALSE)
=======
  write.csv(c, file=file, row.names=FALSE)
>>>>>>> 74e6775b8a9e5a50abc46eda02bde2fbe9225869
  
  c$w=cumsum(c$perc)
  c$wm=c$w-c$perc
  
  c.m=melt(c[,c(1:length(markers), which(names(c)=="perc"))], id="perc")
  c.m$var2=as.numeric(c.m$variable)+2
  
  y_labels=levels(c.m$variable)
  y_breaks = seq_along(y_labels) + 2
  
  c.m$w=rep(c$w,length(markers))
  c.m$wm=rep(c$wm,length(markers))
  c.m$width=c.m$perc

  c.m$ymin=rep(min(c.m$var2), nrow(c.m))
  c.m$ymax=rep(max(c.m$var2)+1, nrow(c.m))
  
  c.m$value[c.m$value==0]="#ffffff"
  c.m$value[c.m$value==1]=color

  library(ggplot2)
  
  if (title==TRUE){
  
  p=
    ggplot(c.m, aes(xmin=wm, xmax=w, ymin=var2, ymax=var2+1))+
    geom_rect(fill=c.m$value, color="black", size=0.1) +
    geom_rect(data=c.m, aes(xmin=wm, xmax=w, ymin=ymin, ymax=ymax), fill=NA, color="black", size=0.2)+
    scale_fill_manual(breaks = c.m$value , values=c.m$value) +
    ylim(c(1.5, max(c.m$var2) + 1))+
    coord_polar(theta="x", direction=1, start=0) +
    labs(title=paste(rev(names(data)[markers]), collapse='\n'))+
    theme(panel.background=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          legend.position="none",
          plot.title=element_text(size=rel(1)),
          plot.background=element_blank())
  }
  
  if (title==FALSE){
    
    p=
      ggplot(c.m, aes(xmin=wm, xmax=w, ymin=var2, ymax=var2+1))+
      geom_rect(fill=c.m$value, color="black", size=0.1) +
      geom_rect(data=c.m, aes(xmin=wm, xmax=w, ymin=ymin, ymax=ymax), fill=NA, color="black", size=0.2)+
      scale_fill_manual(breaks = c.m$value , values=c.m$value) +
      ylim(c(1.5, max(c.m$var2) + 1))+
      coord_polar(theta="x", direction=1, start=0) +
      theme(panel.background=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks=element_blank(),
            axis.text.y=element_blank(),
            legend.position="none",
            plot.title=element_text(size=rel(1)),
            plot.background=element_blank())
  }
  
  
   return(p)
  
}