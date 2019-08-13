# Plot the mean Learnability and expressivity

library(RColorBrewer)
library(grid)
library(gridBase)
library(ggplot2)
library(shape)
setwd("~/Documents/MPI/Thom/LEX_2016/analysis/")

myArrow <- function(x0, y0, x1, y1, cut = 1, ...){
  x.new <- (1 - cut) * x0 + cut * x1
  y.new <- (1 - cut) * y0 + cut * y1
  segments(x0, y0, x1, y1, ...)
  arrows(x0, y0, x.new, y.new, ...)
}

final.all = read.csv("../data/ExperimentData_with_mean_Learnability.csv")

### 
# Plot the data

final = final.all[final.all$file=="Start_4" | final.all$seed=="Start_4" & !is.na(final.all$Learnability),]

# Starting position
startE = final[final$file=="Start_4",]$Expressivity
startL = final[final$file=="Start_4",]$Learnability

conditions = c("Learning-only","Learning + Use","Use-only")

final$condition = factor(final$condition, 
                levels=c("Seed","Learnability","Learnability + Expressivity","Expressivity"),
                labels = c("Seed",conditions))



cols = brewer.pal(3,"Dark2")
names(cols) = conditions
shapes = c(0,2,5)
names(shapes) = conditions
bigShapes = c(15,17,18)
names(bigShapes) = conditions

pdf("../results/graphs/MeanPlots.pdf",
    width = 5, height = 4)
plot.new() 

# setup layout
gl <- grid.layout(nrow=1, ncol=2,widths = c(1,0.3))
# grid.show.layout(gl)

# setup viewports
vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
# init layout
pushViewport(viewport(layout=gl))
# access the first position
pushViewport(vp.1)

par(xpd=F,new=TRUE, fig=gridFIG(), mar=c(3.3,4.3,0.5,1))
plot(c(0,1),c(0,1), type='n',
     xlab="",
     ylab="Learnability")
title(xlab="Expressivity", line=2)
# Learnability
for(cond in conditions){
  learn = final$condition==cond & final$phase=="PH2"
  points(final[learn,]$Expressivity,
         final[learn,]$Learnability, 
         pch=shapes[cond], col=cols[cond])
  meanE = mean(final[learn,]$Expressivity)
  meanL = mean(final[learn,]$Learnability)
  points(meanE,meanL,
         pch=bigShapes[cond], cex=2, col=cols[cond])
  myArrow(startE,startL, meanE, meanL, cut = 0.5, col=cols[cond])
}

points(startE, startL, 
       pch=16, col="black", cex=2)

legend(0.0,0.35,legend=c("Starting language",conditions),
       col=c("black",cols),pch=16,bg = 'white')


# done with the first viewport
popViewport()

# move to the next viewport
pushViewport(vp.2)

###
# Systematicity

final$condition.short = factor(final$condition,
                               levels=c("Seed",conditions),
                               labels = c("Seed","L-only","L+U","U-only") )

cols2 = cols
names(cols2) =  c("L-only","L+U","U-only")

expPlot = ggplot(final[final$condition %in% conditions & final$phase!="PH3",],
       aes(y=Systematicity)) +
  geom_point(aes(x=0.5,colour = condition.short, fill=condition.short,shape=condition.short),
             position = position_dodge(0.1)) +
  scale_colour_manual(values=cols2) + 
  scale_fill_manual(values=cols2) + 
  scale_shape_manual(values=c(15,17,18)) +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title.y =element_text(size=12),
        #axis.title.x =element_text(size=12,margin=margin(t=10)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  #geom_hline(yintercept = final[final$condition=="Seed",]$Systematicity) +
  annotate("point",x=0.5,y = final[final$condition=="Seed",]$Systematicity, colour="black",fill="black", size=3)+
  xlab("")

print(expPlot, newpage = FALSE)

# done with this viewport
popViewport(1)

dev.off()


# Ultra compact version of graph

pdf("../results/graphs/MeanPlots_compact.pdf",
    width=3.5,height=3)
par(mar =c(2,4,0.2,0)+0.1)
plot(c(0.5,1),c(0.4,1), type='n',
     xaxt='n',yaxt='n',
     xlab="",
     ylab="")
title(xlab="Expressivity (proportion of unique labels)       ", line=1)
title(ylab="Learnability\n(similarity to next generation output)", line=2)
# Learnability
for(cond in conditions){
  learn = final$condition==cond & final$phase=="PH2"
  points(final[learn,]$Expressivity,
         final[learn,]$Learnability, 
         pch=shapes[cond], col=cols[cond])
  meanE = mean(final[learn,]$Expressivity)
  meanL = mean(final[learn,]$Learnability)
  points(meanE,meanL,
         pch=bigShapes[cond], cex=2, col=cols[cond])
  myArrow(startE,startL, meanE, meanL, cut = 0.5, col=cols[cond])
}

points(startE, startL, 
       pch=16, col="black", cex=2)
axis(1,padj=-1.2)
axis(2,hadj=0.8,las=2)
legend(0.0,0.35,legend=c("Starting language",conditions),
       col=c("black",cols),pch=16,bg = 'white')
text(0.58,startL,"Seed\nlanguage")
text(0.9,startL,"Use only",col=cols["Use-only"])
text(0.6,0.9,"Learning only",col=cols["Learning-only"])
text(0.95,0.9,"L+U",col=cols["Learning + Use"])
dev.off()
