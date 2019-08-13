# Plot the mean Learnability and expressivity

library(RColorBrewer)
library(grid)
library(gridBase)
library(ggplot2)
library(png)
try(setwd("~/Documents/MPI/Thom/LEX_2016/analysis/"))

final.all = read.csv("../data/ExperimentData_with_mean_Learnability.csv",stringsAsFactors = F)

d = final.all[final.all$phase=="PH2" & final.all$condition !="Seed" & final.all$seed=="Start_4",]
d = d[order(d$condition),]

imageFiles = paste0("../writeup/images/meanings/s",1:16,".png")
images= list()
for(i in imageFiles){
  images[[length(images)+1]] = readPNG(i)
}


plotLang = function(labels,filename){
  png(paste0('../results/graphs/outputLanguages/',filename))
  par(mfrow=c(4,4), mar=c(0,0,0,0))
  for( i in 1:length(images)){
    plot(0:1,0:1,type = 'n',xaxt='n',yaxt='n')
    rasterImage(images[[i]], 0,0,1,1)
    cex = 1.5
    if(nchar(labels[i])>20){
      cex=1
    }
    text(0.5,0.05,labels[i],cex=cex)
  }
  dev.off()
}


d$langNum = rep(1:4,3)
for(i in 1:nrow(d)){
  lx = read.delim(paste0("../Backups/ILMLEX_Backups_Nov2016/experiments/",d[i,]$file,".lang"),sep="\t",header = F,stringsAsFactors = F)
  plotLang(lx[,1], 
           paste0(d[i,]$condition,"_",d[i,]$langNum,".png"))
  
  
}