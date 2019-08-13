
# RepPart - Lang 2, Match 10th 11am, Chain 2
#           Lang 4, Chain 2 March 8th 11am

setwd("~/Documents/MPI/Thom/LEX_2016/ChooseStartingLanguages/analysis/")

source('utils.R')

filebase= "../data/Phase1/experiments/"

startingLanguages.files = c("Start_1.lang","Start_2.lang","Start_3.lang","Start_4.lang")
startingLanguages = list()
startingLanguages.S = list()
startingLanguages.E = list()
for(f in startingLanguages.files){
  lang = loadLanguage(paste(filebase,f,sep=''))
  startingLanguages.E[[f]] = getExpressivity(lang)
  startingLanguages.S[[f]] = getSystematicity(lang)
  startingLanguages[[f]] = lang
}

filebase= "../data/Phase1/results/"

files = list.files(filebase)#,pattern = "*\\.lang")
files = files[!grepl("^Start",files)]

d = data.frame(startLang.name = NA, endLang.name=NA,startLang=NA,endLang=NA,E=NA,S=NA,L=NA)

for(f in files){
  
  lx = loadLanguageFromResults(paste(filebase,f,sep=''))
  if(lx[[3]]==16){
    lang = lx[[1]]
    startLang.name = lx[[2]]
    #lang = loadLanguage(paste(filebase,f,sep=''))
    end.E = getExpressivity(lang)
    end.S = getSystematicity(lang)
    #startLang.name = strsplit(f,'-')[[1]][2]
    startLang = startingLanguages[startLang.name][[1]][1]
    
    end.L = 1 - compare2Langs(startLang,lang)
    d = rbind(d,c(startLang.name,f, startLang, lang, end.E, end.S, end.L))
  }
}
d = d[-1,]
for(i in 5:7){
  d[,i] = as.numeric(d[,i])
}

d$start.E = unlist(startingLanguages.E[d$startLang.name])
d$start.S = unlist(startingLanguages.S[d$startLang.name])

d = d[!d$endLang.name %in% c('PH1_3_10_095345_CH1_parent-Start_1'),]#,'PH1_3_10_102825_CH1_parent-Start_1.lang'),]

plot(d$S,d$L, ylim=c(0,1),xlim=c(0,1), col=as.numeric(as.factor(d$startLang.name)),pch=16)
abline(v=startingLanguages.S, col=1:4)

summary(lm(L ~ start.S, data=d))

summary(lm(start.S~L+E, data=d))

plot(d$start.S,d$L, ylim=c(0,1),xlim=c(0,1), col=as.numeric(as.factor(d$startLang.name)),pch=16)

plot(d$L,d$E, ylim=c(0,1),xlim=c(0,1), col=as.numeric(as.factor(d$startLang.name)),pch=16)
abline(h=unlist(startingLanguages.E),col=rgb(0,0,0,0.3))

tapply(d$L,d$startLang.name,sd)
tapply(d$E,d$startLang.name,sd)

