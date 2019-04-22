# sourced and followed from https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)
require(minqa)

hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
hdp=within(hdp, {
  Married=factor(Married, levels=0:1, labels=c("no", "yes"))
  DID=factor(DID)
  HID=factor(HID)
})

#check out the data
head(hdp)
str(hdp)
summary(hdp)

#examine if there are any clean linear relationships with the following
ggpairs(hdp[,c("IL6", "CRP", "LengthofStay", "Experience")])

#examine if there is a relationship with Cancer Stage & Length of Stay
ggplot(hdp, aes(x=CancerStage, y=LengthofStay)) + 
  stat_sum(aes(size=..n.., group=1))+
  scale_size_area(max_size = 10)

#"To alleviate overplotting and see the values better, we add a small amount of random noise (primarily to the x axis) as well as set the alpha transparency. Although the jittered dots are helpful for seeing the raw data, it can be difficult to get a precise sense of the distribution. For that, we add violin plots. Violin plots are just kernel density plots reflected around the plotting axis. We plot the violin plots on top of the jittered points with a transparency so that you can stil see the raw data, but the violin plots are dominant. Because both IL6 and CRP tend to have skewed distributions, we use a square root scale on the y axis. The distributions look fairly normal and symmetric, although you can still see the long right tail, even using a square root scale (note that only the scale was shifted, the values themselves are not transformed, which is important because this lets you see and interpret the actual scores, rather than the square root of the scores)."
tmp<-melt(hdp[c("CancerStage", "IL6", "CRP")], id.vars="CancerStage")
ggplot(tmp, aes(x=CancerStage, y=value)) +
  geom_jitter(alpha=.1) +
  geom_violin(alpha=.75) +
  facet_grid(variable ~ .) +
  scale_y_sqrt()

#examine the relationship of length of stay, IL6, CRP, and experience on remission binary (0 or 1)
tmp<-melt(hdp[,c("remission", "IL6", "CRP", "LengthofStay", "Experience")],
          id.vars="remission")
ggplot(tmp, aes(factor(remission), y=value, fill=factor(remission))) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")
