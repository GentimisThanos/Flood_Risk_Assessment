
library("easypackages")
libraries("MASS","ggplot2","vcd","psych","stats")
dir.create("../Results",showWarnings = F)
dir.create("../Results/Interpolation",showWarnings = F)
dir.create("../Results/Interpolation/Graphs",showWarnings = F)

#========== Creating Random depths with an exponential of lambda 3 ========================
set.seed(123)
Random_Depths =  rexp(n = 500, rate = 3)*3 # Creates n many random depths
df3=data.frame(Random_Depths=Random_Depths)
df4=read.csv("../Data/Table1.csv",header=T) # Reading the damage table
#========== Adds the damage using interpolations of the normal distributions indicated in the damage table===
for (i in 1:nrow(df3)){
  temp=subset(df4,df4$dpt==floor(df3$Random_Depths[i])|df4$dpt==ceiling(df3$Random_Depths[i]))
  l1=lm(dmg~dpt,data=temp)
  y1=df3$Random_Depths[i]*l1$coefficients[2]+l1$coefficients[1]
  l2=lm(sd~dpt,data=temp)
  y2=df3$Random_Depths[i]*l2$coefficients[2]+l2$coefficients[1]
  df3$Damage[i]=rnorm(1,mean=y1,sd=y2)
}
write.csv(df3,"../Results/Interpolation/Synthetic_Data.csv") # Saves the data as a csv file

#========= Creating a histogram with the corresponding dencity plot of the random depths ===
g3=ggplot(df3, aes(x=Random_Depths)) + 
  geom_histogram(aes(y=..density..))+
  geom_density(alpha=.2, fill="#FF6666")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))+
  theme_light()
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist.pdf",g3)
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist.jpg",g3)
l1=as.data.frame(describe(df3))
write.csv(l1,"../Results/Interpolation/Summary_Synthetic_Data.csv")


g4=ggplot(df3, aes(x=Random_Depths, y=Damage)) + 
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x, size = 1, color="blue")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color="red")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^3), size = 1, color="purple")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))
ggsave("../Results/Interpolation/Graphs/Random_Depth_vs_Damage.pdf",g4,scale=3)
ggsave("../Results/Interpolation/Graphs/Random_Depth_vs_Damage.jpg",g4,scale=3)

#========= Curve fitting 
logEstimate <- lm(Damage~log(Random_Depths),data=df3)
logEstimate$coefficients
attach(df3)
model3 <- lm(Damage~ poly(Random_Depths,3))
model2 <- lm(Damage~ poly(Random_Depths,2))
model1=lm(Damage~Random_Depths)
summary(model1)
summary(model2)
summary(model3)

theta.0 <- min(df3$Damage) * 0.5  
model.0 <- lm(log(Damage - theta.0) ~ Random_Depths, data=df3)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
model <- nls(Damage ~ alpha * exp(beta * Random_Depths) + theta , data = df3, start = start,control = list(maxiter = 500),
            trace = TRUE)

