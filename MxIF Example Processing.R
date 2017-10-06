library(tiff)
library(ggplot2)
library(png)
library(Rtsne)
library(magick)
library(reshape)
library(MxIF)

## If using 

DIRECTORY='/Users/etmckinley/Dropbox (VUMC)/scan_alpha/TCPS4A/SegQuant'

dir.create(paste(DIRECTORY, 'tSNE', sep="/"), showWarnings = FALSE)


Novlp.spots=list.files(path = paste(DIRECTORY, 'Novlp', sep="/"))
Labels.spots=list.files(path = paste(DIRECTORY, 'CellSegFinal', sep="/"))
Stats.spots=list.files(path = paste(DIRECTORY, 'PosStats', sep="/"))

for (i in 1:50){
posData=read.csv(paste(DIRECTORY, 'PosStats', Stats.spots[i], sep="/"), header=TRUE) #7480

posData=MxIF.cell.filter(posData) #7097

#posData=read.csv(paste(DIRECTORY, 'PosStats', Stats.spots[i], sep="/"), header=TRUE)

#tsne
dapiInd=as.numeric(grep("Median_Cell_DAPI", names(posData), value = FALSE))
markers=seq(from = 9, to = ncol(posData), by = 4)
markers=markers[!markers %in% dapiInd]


if (file.exists(paste(DIRECTORY, 'PosStats_filt', Stats.spots[i], sep="/"))){
  posData=read.csv(paste(DIRECTORY, 'PosStats_filt', Stats.spots[i], sep="/"), header=TRUE) #7480
}else {

posData=MxIF.tsne(posData, markers)

dir.create(paste(DIRECTORY, 'PosStats_filt', sep="/"), showWarnings = FALSE)
write.csv(posData, paste(DIRECTORY, 'PosStats_filt', Stats.spots[i], sep="/"), row.names = FALSE)
}

pos=gsub("Novlp_", "", Novlp.spots[i])
pos=gsub(".png", "", pos)

markers=c(seq(from = 9, to = ncol(posData)-2, by = 4),ncol(posData)-1,ncol(posData))

for (j in markers){
  p=MxIF.tsne.plot(posData,j)
  ggsave(filename=paste(DIRECTORY, '/tSNE/',pos, "_", names(posData)[j], ".png", sep=""), plot=p, height=6, width=6, bg = "transparent")
}


MxIF.tsne.animate(posData, markers, paste(DIRECTORY, '/tSNE/',pos, "_animation.gif", sep=""))

#To DO
#2 Cell Viewer by cell ID



LABELS=readTIFF(paste(DIRECTORY, 'CellSegFinal' , Labels.spots[i] ,sep="/"), as.is=TRUE)
NOVLP=readPNG(paste(DIRECTORY, 'Novlp', Novlp.spots[i] , sep="/"))

NOVLP=MxIF.cell.filter.view(posData, LABELS, NOVLP)

dir.create(paste(DIRECTORY, 'Novlp_filt', sep="/"), showWarnings = FALSE)

filtFile=gsub("Novlp", "Novlp_filt", Novlp.spots[i])
writePNG(NOVLP, target=paste(DIRECTORY, 'Novlp_filt', filtFile,sep="/"))



}

 AFRemoved=readTIFF('/Users/etmckinley/Dropbox (VUMC)/scan_alpha/TCPS4A/AFRemoved/ECAD_AFRemoved_pyr16_spot_000.tif', convert=TRUE)
# IDs=c(100,500,1000,1500)
# 
# cellsize=posData[order(-posData[,31]),]
# cellsize=cellsize[1:100,]
# IDs=as.array(cellsize$ID)
# 
# Out=MxIF.cell.ID.view(AFRemoved, LABELS, IDs)
# 
# writePNG(Out, target=paste(DIRECTORY, "test.png",sep="/"))

