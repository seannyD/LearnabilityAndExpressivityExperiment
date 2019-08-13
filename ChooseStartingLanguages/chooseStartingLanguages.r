rm(list=ls())
library(vegan)
# Calculate Expressivity and Systematicity of results

# Assumes that there's a 'Results Top Folder' name 'Run1', which contains the folders 'L-only', 'L+E' and 'E-only'.  Inside each of these, individual experiments are contained in folders e.g. "Language 1".  The R script should be saved to the same folder as the 'Results Top Folder'.
#The script calculates the starting and ending expressivity and systematicity.  It then plots three graphs:  The trajectories of each experiment in absolute space, the trajectories of each experiment in relative space (with mean trajectories), and the trajectories of each experiment in relative space (with summed trajectories).


# set this file location to the one that contains the ResultsTopFolder
# (the folder where this R script is saved)
setwd("~/Documents/MPI/Thom/MastersProject/results")

# change to name of Results Top Folder
ResultsTopFolder = "Run1"

getExpressivity = function(X){
	Y = strsplit(X,"_")[[1]]
	length(unique(Y))/length(Y)
}

getSystematicity = function(X){
	getSystematicity2(strsplit(X,"_")[[1]])
}

getSystematicity2 = function(Y){
	Y = Y[1:length(meaningSpace)]
	signalDist = adist(Y)
	a = matrix(rep(nchar(Y),length(Y)),ncol=length(Y))
	b = matrix(rep(nchar(Y),length(Y)),ncol=length(Y),byrow=T)
	a[b>a] = b[b>a]
	# normalise signal dist
	signalDist = signalDist/a
	return(vegan::mantel(signalDist,mDist,permutations=1,method='spearman')$statistic)
}


# 1st slot = pattern, 2nd = shape
meaningSpace = c("00","01","02","03","10","11","12","13","20","21","22","23","30","31","32","33")
mDist = adist(meaningSpace)

# list of folders to exclude
remove.files = c()

# read in data
res = data.frame(file=NA,start.lang=NA,end.lang=NA,start.E=NA,start.S=NA,end.E=NA,end.S=NA,condition=NA,start.E.cat=NA,start.S.cat=NA)
for(cond.folder in c("L-only","L+E","E-only")){

	for(folder in dir(paste(ResultsTopFolder,cond.folder,sep='/'))){
		files = dir(paste(ResultsTopFolder,cond.folder,folder,sep='/'))
		filename = files[grep('.lang$',files)]
		filename.path = paste(ResultsTopFolder,cond.folder,folder,sep='/')
		if(!filename.path %in% remove.files){
		
		l = read.delim(paste(filename.path,filename,sep='/'),header=F)
		lang = paste(l[,1],collapse="_")
		end.E = getExpressivity(lang)
		end.S = getSystematicity(lang)
		
		starting.f = paste("startingLanguages/START",gsub(".+START([a-z0-9\\-]+)\\..+","\\1",filename),".lang",sep='')
		sl = read.delim(starting.f,header=F)
		slang = paste(sl[,1],collapse="_")
		start.E = getExpressivity(slang)
		start.S = getSystematicity(slang)
		
		start.E.cat = strsplit(starting.f,'-')[[1]][3]
		start.S.cat = strsplit(starting.f,'-')[[1]][5]
		
		dx = data.frame(file=filename,start.lang=slang,end.lang=lang,start.E=start.E,start.S=start.S,end.E=end.E,end.S=end.S,condition=cond.folder,start.E.cat=start.E.cat,start.S.cat=start.S.cat)
		
		res = rbind(res,dx)
		}
		}
	}
res = res[!is.na(res$file),]

plot(res$start.E,res$start.S)

res = res[res$start.E.cat==11,]
res = res[res$condition == 'L-only',]



res$levDist =NA
for(i in 1:nrow(res)){
  start = strsplit(res[i,]$start.lang,"_")[[1]]
  end = strsplit(res[i,]$end.lang,"_")[[1]]
  ad = adist(start,end)
  res[i,]$levDist = mean(diag(ad)/apply(rbind(nchar(start),nchar(end)),2,max))
}

plot(res$start.S, res$levDist)
text(res$start.S, res$levDist, res$file)


choice = c("STARTa-e-11-s-8-.lang" , "STARTa-e-11-s-2-.lang" )

for(i in 1:length(choice)){
  newName = paste("Start_",i,'.lang',sep='')
  file.copy(paste("startingLanguages/",choice[i],sep=''), paste("~/Documents/MPI/Thom/LEX_2016/ChooseStartingLanguages/candidates/",newName,sep=''))
}


# Find lang with E = cat 11, systematiciy = 0.2, 0.3



syls = c("lu","mo","na","ne","ka","pi",'hu','ki','la')
numOfSyllables = length(syls)
makeWord <- function(syls, wordLengthRange=2:4){
  return(paste(sample(syls,sample(wordLengthRange,1),replace=TRUE),sep="",collapse=""))
}

numWordsInLang = 16
#signals = array(dim=c(27,4))
makeSignals <- function(syls,numSignals, expressivity=11){
  sigs = replicate(expressivity, makeWord(syls))
  sigs2 = sample(c(sigs,sample(sigs,numSignals-expressivity,replace=T)))
  return(paste(sigs2,collapse='_'))
}

#set.seed(1111)
#r = data.frame(lang=replicate(5000,makeSignals(syls,16,11)),stringsAsFactors = F)
#r$S = sapply(r$lang, getSystematicity)
#hist(r$S)
#max(r$S)


corruptSignals = function(syls,numSignals,expressivity){
  dim1 = replicate(4,makeWord(syls,1:2))
  dim2 = replicate(4,makeWord(syls,1:2))
  sigs = as.vector(outer(dim1,dim2,paste,sep=''))
  while(length(unique(sigs))>expressivity){
    rx = sample(1:length(sigs),2)
    sigs[rx[1]] = sigs[rx[2]]
  }
  return(paste(sigs,collapse="_"))
}

set.seed(1111)
r2 = data.frame(lang=replicate(500,corruptSignals(syls,16,11)),stringsAsFactors = F)
r2$S = sapply(r2$lang, getSystematicity)
r2$E = sapply(r2$lang, getExpressivity)

r2 = r2[r2$E==0.6875,]

hist(r2$S)
max(r2$S)

tol = 0.01
set.seed(456)
targets = c(0.2,0.3)
newNames = paste("~/Documents/MPI/Thom/LEX_2016/ChooseStartingLanguages/candidates/Start_",3:(2+length(targets)),'.lang',sep='')

for(i in 1:length(targets)){
  sel3 = sample(r2[r2$S > (targets[i]-tol) & r2$S<(targets[i]+tol),]$lang,1)
  templateF = "~/Documents/MPI/Thom/LEX_2016/ChooseStartingLanguages/candidates/Start_1.lang"
  template = read.delim(templateF,stringsAsFactors=F,sep='\t',header=F)
  template[,1] = strsplit(sel3,"_")[[1]]
  write.table(template,file=newNames[i],col.names=F,row.names=F,quote=F,sep='\t')
}

