

setwd("~/Documents/MPI/Thom/LEX_2016/processing/")

source("../ChooseStartingLanguages/analysis/utils.R")

getLearnabilityFromExperiment = function(startingLang,children){
  children = children[order(children$loadFile, children$meaning),]
  normLev = 1-tapply(children$word, children$loadFile, function(X){
    mean(diag(normLevDist(startingLang,X)))
  })
  return(normLev)
}

# Load data
final.all = read.csv("../data/Filenames.csv", stringsAsFactors = F)
d = read.csv("../data/ExperimentData.csv", stringsAsFactors = F)

final.all$Expressivity = NA
final.all$Systematicity = NA
final.all$Learnability = NA
final.all$Learnability.sd = NA
final.all$mean.word.length = NA
final.all$distinctiveness = NA

d.stat = data.frame()

for(i in 1:nrow(final.all)){
  px = final.all[i,]$path
  px2 = paste0(tail(strsplit(px,"/")[[1]],1),'.lang')
  if(grepl("/Start_",px)){
    px = paste0(px,".lang")
    px2 = tail(strsplit(px,"/")[[1]],1)
  }
  dx = d[d$loadFile==px,]
  dx = dx[dx$meaning>=0,]
  dx = dx[is.na(dx$stage) | dx$stage==-1,]
  lang = dx$word[order(dx$meaning)]
  
  E = length(unique(lang))/length(lang)
  S = getSystematicity2(lang)
  mean.word.length = sum(nchar(lang))/length(lang)
  distinctiveness = mean.normLevDist(lang)
  
  final.all$Expressivity[i] = E
  final.all$Systematicity[i] = S
  final.all$mean.word.length[i] = mean.word.length
  final.all$distinctiveness[i] = distinctiveness
  
  # Work out learnability
  
  children = d[d$parent==px2 & d$condition =="Learnability",]
  lx = NA
  if(nrow(children)>0){
    lx = getLearnabilityFromExperiment(lang,children)
  }
  d.stat = rbind(d.stat,
                 data.frame(
                   loadFile = dx$loadFile[1],
                   condition = dx$condition[1],
                   phase = dx$phase[1],
                   seed = dx$seed[1],
                   parent = dx$parent[1],
                   Expressivity = E,
                   Systematicity = S,
                   Learnability = lx,
                   mean.word.length = mean.word.length,
                   distinctiveness = distinctiveness
                 ))
  
  final.all$Learnability[i] = mean(lx)
  final.all$Learnability.sd[i] = sd(lx)
}


write.csv(final.all,"../data/ExperimentData_with_mean_Learnability.csv", row.names = F)

write.csv(d.stat, "../data/ExperimentData_with_all_Learnability.csv", row.names = F)


