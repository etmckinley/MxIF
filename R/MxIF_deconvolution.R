#' MxIF Model Based Deconvolution Threshold
#'
#' This function uses model based deconvolution to threshold markers into two groups (high/low). Output is a table of cells with positive(1) or negative(0) values for each marker. Also writes table of threshold values. Default is for all data. If user inputs a selected marker and a quantile, operations will only be run on the cells for the top 1-quant quantile for that marker. 
#' Based upon:
#' McKinley ET, Sui Y, Al-Kofahi Y, Millis BA, Tyska MJ, Roland JT, Santamaria-Pang A, Ohland CL, Jobin C, Franklin JL, Lau KS, Gerdes MJ, Coffey RJ. Optimized multiplex immunofluorescence single-cell analysis reveals tuft cell heterogeneity. JCI Insight. 2017 Jun 2; 2(11): e93487. 
#' https://insight.jci.org/articles/view/93487
#' 
#' Eliot McKinley 10/18/17
#' @param data data frame of cell data from Matlab based segmentation
#' @param markers which markers to run deconvolution processing on
#' @param slidename string of slidename for data output
#' @param file string of where to save data
#' @param selectedMarker string of which marker used to trim data by quantile, default =''
#' @param quant what quantile cutoff to use, defualt = ''
#' @param plots whether to output plots of steps, default =FALSE
#' @param celIDcols which columns to keep to ID cells for future processing, default= c(1,2) CellID and Pos
#' @keywords deconvolution threshold MxIF
#' @export
#' @examples
#' MxIF.deconvolution(data, markers, slidename, file, selectedMarker='Median_Cell_GFP', quant=0.8, plots=TRUE, cellIDcols=c(1,2,24))


MxIF.deconvolution <- function(data, markers, slidename, file, selectedMarker='', quant=0, plots=FALSE, cellIDcols=c(1:2)){
  #get marker names
  BM=names(data)[markers]
  
  #trim data to markers selected
  cellIDs=data[,1:2]
  
  data=(data[,BM])
  
  
  
  #if user wants selected cells
  if (quant>0){
    
    ################## take the top X% of selected marker
    
    #get only cells for top percentile of selected markers
    x=data[,selectedMarker]
    c2=quantile(x,quant)
    
    #trim all data
    data=data[x>c2,]
    cellIDs=cellIDs[x>c2,]
    #save pdf of plot
    if (plots==TRUE){
      pdf(file=paste0(file, '/', slidename, '_',selectedMarker,'_top' ,toString((1-quant)*100), '.pdf'))
      plot(density(data[,selectedMarker]),main=slidename,xlab=selectedMarker)
      dev.off()
    }
  }
  
  ############## run mclust on data
  
  library(mclust)
  N=length(BM)
  mc=vector("list",N)
  for( i in 1:length(BM)){
    cat(BM[i],"")
    y=data[,BM[i]]
    y=y[y>0]
    mc[[i]]=Mclust(y, G = 2, modelNames="V")
    names(mc)[[i]]=BM[i]
  }
  #save(mc,file="mclust.RData")
  
  ############# predict membership for all cells
  
  newdat=as.data.frame(matrix(0,nrow(data),ncol=ncol(data)))
  names(newdat)=colnames(data)
  
  for( i in 1:length(BM)){
    cat(BM[i],"")
    
    
    a=mc[[i]]
    y=data[,BM[i]]
    newdat[y>0,BM[i]]=a$classification
    y=data[,BM[i]]
    me=a$parameters$mean[1]
    x=newdat[,BM[i]]
    x[y<me]=0
    newdat[,BM[i]]=x
  }
  
  newdat2=newdat
  for(i in 1:length(BM)){
    cat(i,"")
    
    y=newdat[,BM[i]]
    x=y
    x[y==max(y)]=1
    x[y<max(y)]=0
    newdat2[,BM[i]]=x
  }
  
  
  
  
  
  ########### get the thresholds 
  thresholds=data.frame(matrix(NA, nrow = length(BM), ncol = 2))
  names(thresholds)=c("Marker","Threshold")
  for(i in 1:length(BM)){
    x=newdat2[,BM[i]]
    y=data[x==1,BM[i]]
    minBM=as.numeric(min(y))
    thresholds[i,1]=BM[i]
    thresholds[i,2]=minBM
  }
  write.csv(thresholds,file=paste0(file, '/', slidename, '_',selectedMarker,'_top' ,toString((1-quant)*100), '_thresholds.csv'),row.names=FALSE)
  
  
  #################### plot the model fitting
  
  if (plots==TRUE){
    pdf(file=paste0(file, '/', slidename, '_',selectedMarker,'_top' ,toString((1-quant)*100),'density_mclust_thresh.pdf'))
    for(i in 1:length(BM)){
      cat(i,"")
      
      
      k=i
      y=data[,BM[i]]
      y=y[y>0]
      a=mc[[k]]
      me=a$parameters$mean
      sd0=sqrt(a$parameters$variance$sigmasq)
      n=length(me)
      if(length(sd0)==1) sd0=rep(sd0,n)
      pro=a$parameters$pro
      
      plot(density(y),xlab="",ylab="",main=names(mc)[[k]])
      
      x=seq(me[n-1]-3*sd0[n-1],me[n-1]+3*sd0[n-1],length=100)
      hx=pro[n-1]*dnorm(x,mean=me[n-1],sd=sd0[n-1])
      lines(x,hx,xlab="",ylab="",col=3)
      
      x=seq(me[n]-3*sd0[n],me[n]+3*sd0[n],length=100)
      hx=pro[n]*dnorm(x,mean=me[n],sd=sd0[n])
      lines(x,hx,xlab="",ylab="",col=2)
      
      abline(v=me,lty=3)
      abline(v=me[(n-1):n],col=3:2,lty=3)
      
      biny=newdat2[,BM[i]]
      newy=data[,BM[i]]
      abline(v=min(newy[biny==1]),col=4)
      
    }
    dev.off()
  }
  newdat2=data.frame(cellIDs,newdat2)
  #save(newdat2,file=paste0(file, '/', slidename, '_',selectedMarker,'_top' ,toString((1-quant)*100),'_mclust_prediction.RData'))
  return(newdat2)
}


