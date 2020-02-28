library("easypackages")
libraries("MASS","ggplot2","vcd","psych","fitdistrplus","ismev","gumbel")
dir.create("../Results",showWarnings = F)
dir.create("../Results/General",showWarnings = F)
dir.create("../Results/Simple",showWarnings = F)
dir.create("../Results/Simple/Graphs",showWarnings = F)
#========== Read the table of the depth probabilities====================================

df1=read.csv("../Data/Table2.csv")
Synthetic=c()
for (i in 1:nrow(df1)){
Synthetic=c(Synthetic,rep(df1$Depth[i],df1$Probability[i]*1000))  
}
fit_g<-gum.fit(Synthetic)
fit_g<-fitdistr(Synthetic, "gumble") 


dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))
fit_g <- fitdist(Synthetic, "gumbel", 
                     start=list(a=10, b=10))
Shp=fit_g$estimate[1]
Scl=fit_g$estimate[2]


fit <- fitdistr(Synthetic, densfun="normal") 
plotdist(Synthetic, histo = TRUE, demp = TRUE)
descdist(Synthetic, discrete=FALSE, boot=500)
fit_w  <- fitdist(Synthetic, "weibull")
fit_g  <- fitdist(Synthetic, "gamma")
fit_ln <- fitdist(Synthetic, "lnorm")

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)



fit_w$estimate


set.seed(123)
NewSynth=dgumbel(500000, shape=Shp, scale = Scl)
fit_w1  <- fitdist(NewSynth, "weibull")
denscomp(fit_w1)
summary(NewSynth)
NewSynthNew=NewSynth[NewSynth>1]
df1=data.frame(NewSynthNew=NewSynthNew)



ggplot(df1, aes(x=NewSynthNew )) + geom_histogram()

  



summary(NewSynth)

summary(fit_w)
denscomp(fit_w)