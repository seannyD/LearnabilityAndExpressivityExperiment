library(igraph)
#setwd("~/Documents/MPI/Thom/LEX_2016/Backups/privateAug9/LEX/results/")
setwd("~/Documents/MPI/Thom/LEX_2016/processing/")

finalDataFolder = "../Backups/ILMLEX_Backups_Nov2016/results/"

files = list.files(finalDataFolder)
files = files[grepl("PH",files)]

sl = c("Start_1.lang","Start_2.lang","Start_3.lang","Start_4.lang")

files = c(files,sl)

f2 = c()

for(f in files){
  filename = paste0(finalDataFolder,f)
  if(file.exists(filename)){
  d = read.delim(filename,
                 header=F,sep='\t',quote='', stringsAsFactors = F)
  if(nrow(d)>10){
    parent = gsub("\\.lang","",d$V8[1])
    child = f#paste(f,".lang",sep='')
    f2 = c(f2,parent,child)
  }
  } else{
    print("!!!!")
    print(f)
  }
}

f2 = unique(f2)


dx = adist(f2[!f2 %in% sl])
dx[lower.tri(dx)] = 0
which(dx<=3 & dx>0,arr.ind = T)
f2[!f2 %in% sl][c(6,7)]

mx= matrix(nrow=length(f2),ncol=length(f2))
rownames(mx) = f2
colnames(mx) = f2

col = data.frame()

for(f in f2){
  filename = paste0(finalDataFolder,f)
  if(file.exists(filename)){
    d = read.delim(filename,
                   header=F,sep='\t',quote='', stringsAsFactors = F)
    if(nrow(d)>10 & ncol(d)>2){
      parent = gsub("\\.lang","",d$V8[1])
      child = f#paste(f,".lang",sep='')
      mx[parent,child] = 1
      col = rbind(col, data.frame(f,d$V11, stringsAsFactors = F))
    }
  }
}

col$type = col$d.V11
col$type[col$d.V11=="Test Learnability (phase 1)"] = "Learnability"
col$type[col$d.V11=="Test Learnability (phase 3)"] = "Learnability"

col$colour = c("red","green",'blue')[as.numeric(factor(col$type))]

table(col$d.V11)

g = graph_from_adjacency_matrix(mx, mode='directed', diag=F)
plot(g)

V(g)$color = col[match(names(V(g)),col$f),]$colour

gc = components(g)

final.all = data.frame()

pdf("../data/LEX_ExperimentsSoFar.pdf", width=20, height=16)
  for(i in 1:gc$no){
    if(gc$csize[i]>1){
      vx= names(gc$membership)[gc$membership==i]
      g2 <- induced.subgraph(graph=g,vids=vx)
      plot(g2,layout=layout_as_tree(g2,circular=T))
      legend(1.5,1,c("E","L","E&L"),col=c("red",'green','blue'), pch=16)
      
      final = data.frame(file = V(g2)$name, stringsAsFactors = F)
      startLang = final$file[grepl("^Start_",final$file)]
      final$phase = "PH2"
      final[final$file %in% names(neighbors(g2,startLang)),]$phase = "PH1"
      final$phase[final$file==startLang]= "PH0"
      
      final = final[final$file != "PH2_6_14_100004_CH1_parent−PH3_5_19_113844_CH1_parent−PH1_3_8_105005_CH1_parent−Start_4",]
      
      
      final$seed = startLang
      final$condition = col[match(final$file, col$f),]$type
      final$condition[final$file==startLang]= "Seed"
      
      final.all = rbind(final.all,final)
      
    }
  }
dev.off()

final.all$path = paste0(finalDataFolder,final.all$file)

# Change phase numbers from 0, 1, 2 to 1,2,3:
final.all$phase[final.all$phase=="PH2"] = "PH3"
final.all$phase[final.all$phase=="PH1"] = "PH2"
final.all$phase[final.all$phase=="PH0"] = "PH1"

write.csv(final.all, "../data/Filenames.csv", row.names = F)
