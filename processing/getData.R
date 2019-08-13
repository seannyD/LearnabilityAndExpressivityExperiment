setwd("~/Documents/MPI/Thom/LEX_2016/processing/")

# List of filenames used
final.all = read.csv("../data/Filenames.csv", stringsAsFactors = F)

loadSeedFile = function(f){
  dx = read.delim(paste0(f,".lang"),sep="\t", stringsAsFactors = F,
                 header = F)
  d = data.frame(
    meaning=0:(nrow(dx)-1), # meanings are from 0 to 15
    word = dx$V1,
    loadFile=paste0(f,".lang"))
  for(x in c('dictionary','dictionary2','stage',
             'date',"linkFile",'linkFile2',
             'parent',"role",'phase',
             "time2","order")){
    d[,x] = NA
  }
  d = d[,c("meaning",'dictionary','dictionary2','stage',
           'word','date',"linkFile",'linkFile2',
           'parent',"role",'phase',
           "time2","order",'loadFile')]
  return(d)
}

loadFile = function(f){
  d = read.delim(f,sep="\t", stringsAsFactors = F,
                 header = F)
  names(d)[1:8] = c("meaning",'dictionary','stage','word','date',
               "linkFile",'linkFile2','parent')
  names(d)[grepl("Part",d[1,])] = "role"
  names(d)[which(grepl("broc(coli)*_",d[1,]))[1]] = "dictionary2"
  ecol = grepl("Expressivity",d[1,]) |
    grepl("Learnability",d[1,])
  names(d)[ecol] = "phase"
  
  tcol = grepl("[0-9][0-9][0-9]Z",d[1,])
  
  if(sum(tcol)>0){
    names(d)[tcol] = "time2"
  } else{
    d$time2 = NA
  }
  
  d$order = 1:nrow(d)
  d$loadFile = f
  d = d[order(d$meaning),]
  
  d = d[,
        c("meaning",'dictionary','dictionary2','stage',
          'word','date',"linkFile",'linkFile2',
          'parent',"role",'phase',
          "time2","order",'loadFile')]
  
  return(d)
}

res = data.frame()

for(i in 1:nrow(final.all)){
  f = final.all$path[i]
  if(final.all$phase[i]=="PH1"){
    resx = loadSeedFile(f)
  } else{
    resx = loadFile(f)
  }
  resx$phase = final.all$phase[i]
  resx$condition = final.all$condition[i]
  resx$seed = final.all$seed[i]
  res = rbind(res,resx)
}


write.csv(res,"../data/ExperimentData.csv", row.names = F)


# Learnability.chain.starts = c(
#   "PH1_3_8_105005_CH1_parent-Start_4",
#   "PH1_3_8_105030_CH2_parent-Start_4",
#   "PH1_3_10_102825_CH4_parent-Start_4",
#   "PH1_3_24_094856_CH4_parent-Start_4"
# )
# 
# Learnability.chain1 = c(
#   "PH1_3_8_105005_CH1_parent-Start_4",
#   "PH3_5_19_113844_CH1_parent-PH1_3_8_105005_CH1_parent-Start_4",
#   "PH2_6_14_100004_CH1_parent-PH3_5_19_113844_CH1_parent-PH1_3_8_105005_CH1_parent-Start_4", #??
#   "PH3_8_23_103135_CH1_parent-PH1_3_8_105005_CH1_parent-Start_4",
#   "PH3_8_26_115808_CH1_parent-PH1_3_8_105005_CH1_parent-Start_4"
# )
# 
# Learnability.chain2 = c(
#   "PH1_3_8_105030_CH2_parent-Start_4",
#   "PH2_5_26_112353_CH1_parent-PH1_3_8_105030_CH2_parent-Start_4", #??
#   "PH2_6_28_114809_CH1_parent-PH1_3_8_105030_CH2_parent-Start_4",  
#         # PH2_6_28_122845_CH1_parent-PH1_3_10_102825_CH4_parent-Start_4
#   "PH3_8_23_130125_CH1_parent-PH1_3_8_105030_CH2_parent-Start_4",
#   "PH3_8_26_115915_CH2_parent-PH1_3_8_105030_CH2_parent-Start_4"
# )
# 
# Learnability.chain3 = c(
#   
# )