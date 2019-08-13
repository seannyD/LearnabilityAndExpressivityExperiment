library(vegan)
# Calculate Expressivity and Systematicity

getExpressivity = function(X){
  Y = strsplit(X,"_")[[1]]
  length(unique(Y))/length(Y)
}

normLevDist = function(x,y=x){
  signalDist = adist(x,y)
  a = matrix(rep(nchar(x),length(x)),ncol=length(x))
  b = matrix(rep(nchar(y),length(y)),ncol=length(y),byrow=T)
  a[b>a] = b[b>a]
  # normalise signal dist
  return(signalDist/a)
}

mean.normLevDist = function(x,y=x){
  n = normLevDist(x,y)
  mean(n[upper.tri(n)])
}

getSystematicity = function(X){
  getSystematicity2(strsplit(X,"_")[[1]])
}

getSystematicity2 = function(Y){
  Y = Y[1:length(meaningSpace)]
  signalDist = normLevDist(Y)
#  signalDist = adist(Y)
#  a = matrix(rep(nchar(Y),length(Y)),ncol=length(Y))
#  b = matrix(rep(nchar(Y),length(Y)),ncol=length(Y),byrow=T)
#  a[b>a] = b[b>a]
#  # normalise signal dist
#  signalDist = signalDist/a
  return(vegan::mantel(signalDist,mDist,permutations=1,method='spearman')$statistic)
}

loadLanguage = function(filename){
  l = read.delim(paste(filename,sep='/'),header=F)
  return(paste(l[,1],collapse="_"))
}

loadLanguageFromResults = function(filename){
  l = read.delim(filename,header=F,stringsAsFactors = F)
  startLang = l[1,8]
  l = l[,4][order(l[,1])]
  return(list(paste(l,collapse="_"),startLang,length(l)))
}

compare2Langs = function(l1,l2){
  l1 = unlist(strsplit(l1,"_")[[1]])
  l2 = unlist(strsplit(l2,"_")[[1]])
  dx = normLevDist(l1,l2)
  return(mean(diag(dx)))
}

# 1st slot = pattern, 2nd = shape
meaningSpace = c("00","01","02","03","10","11","12","13","20","21","22","23","30","31","32","33")
mDist = adist(meaningSpace)


