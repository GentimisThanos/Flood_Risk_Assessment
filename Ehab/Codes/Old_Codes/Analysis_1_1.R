library("easypackages")
libraries("MASS","ggplot2","vcd","psych")
dir.create("../Results",showWarnings = F)
dir.create("../Results/General",showWarnings = F)
dir.create("../Results/Simple",showWarnings = F)
dir.create("../Results/Simple/Graphs",showWarnings = F)
#========== Read the table of the depth probabilities====================================
Elevation=c(-3.4,-3.9,-4.2,-4.7)
Depth=c(3.4,2.9,2.6,2.1)
Prop=c(0.002,0.01,0.02,0.1)
Count=Prop*500
df1=data.frame(Depth=Depth,Prop=Prop,Count=Count)
#========== Create dataset with Depths based on given table and graphing it =============
Depthnew=c(rep(Depth[1],Count[1]),rep(Depth[2],Count[2]),rep(Depth[3],Count[3]),rep(Depth[4],Count[4]))
df2=data.frame(Depthnew=Depthnew)
g1=ggplot(df2, aes(x=Depthnew)) + 
  geom_histogram(aes(y=..count..))+
  scale_x_discrete(name="Depth", breaks=Depth,limits=Depth)+
  scale_y_discrete(name="Occurences in 500 years", breaks=Count,limits=Count)+ 
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))

ggsave("../Results/General/Depth_Table.pdf",g1)
ggsave("../Results/General/Depth_Table.jpg",g1)
#========== Fitting the best exponential distribution and testing the fit ================
fit1 <- fitdistr(Depthnew, "exponential")
sink("../Results//General/Exponential_Fit.doc")
paste0("The parameter of the explonentila with the best fit is: ",fit1$estimate)
print(ks.test(Depthnew, "pexp", fit1$estimate))
print("Since the p-value is so small we reject the null hypothesis and we are forced to concede that this Data DOES NOT follow an exponential distribution")
sink()
#========== Estimating the best exponential fit ==========================================
g2=ggplot(df1, aes(x=Depth, y=Prop)) + 
  geom_point()+
  scale_x_continuous(name="Depth", breaks=c(0,1,Depth),labels=c(0,1,Depth),limits=c(0,4))+
  geom_smooth(method = "lm", formula = y ~ exp(-4*x), se = FALSE,colour="Red")+
  geom_smooth(method = "lm", formula = y ~ exp(-3*x), se = FALSE,colour="Green")+
  geom_smooth(method = "lm", formula = y ~ exp(-5*x), se = FALSE,colour="Blue")+
  annotate("text", x = 3, y = 0.075, colour = "red", label = "lambda = 4")+
  annotate("text", x = 3, y = 0.085, colour = "Green", label = "lambda = 3")+
  annotate("text", x = 3, y = 0.065, colour = "Blue", label = "lambda = 5")+ 
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))
ggsave("../Results/General/Fitted_Exponentials.pdf",g2,scale=3)
ggsave("../Results/General/Fitted_Exponentials.jpg",g2,scale=3)

#========== Creating Random depths with an exponential of lambda 3 ========================
set.seed(123)
Random_Depths =  rexp(n = 10000, rate = 3)*3 # Creates n many random depths
df3=data.frame(Random_Depths=Random_Depths)
df4=read.csv("../Data/Table1.csv",header=T) # Reading the damage table
#========== Adds the damage using the normal distributions indicated in the damage table===
for (i in 1:nrow(df3)){
  temp=subset(df4,df4$dpt==round(df3$Random_Depths[i],digits=0))
  df3$Damage[i]=rnorm(1,mean=temp[1,2],sd=temp[1,3])
}
write.csv(df3,"../Results/Simple/Synthetic_Data.csv") # Saves the data as a csv file

#========= Creating a histogram with the corresponding dencity plot of the random depths ===
g3=ggplot(df3, aes(x=Random_Depths)) + 
  geom_histogram(aes(y=..density..))+
  geom_density(alpha=.2, fill="#FF6666")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))+
  theme_light()
ggsave("../Results/Simple/Graphs/Random_Depth_hist.pdf",g3)
ggsave("../Results/Simple/Graphs/Random_Depth_hist.jpg",g3)
l1=as.data.frame(describe(df3))
write.csv(l1,"../Results/Simple/Summary_Synthetic_Data.csv")

g4=ggplot(df3, aes(x=Random_Depths, y=Damage)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))
ggsave("../Results/Simple/Graphs/Random_Depth_vs_Damage.pdf",g4)
ggsave("../Results/Simple/Graphs/Random_Depth_vs_Damage.jpg",g4)

# Before computing damage make a loop that goes through different freeboard 
# save all different damages. (elevation - freeboard)
# Use gumbel https://stackoverflow.com/questions/36087240/fitting-a-gumbel-distribution-with-fitdistrplus
# On the five point table provided
# 500,000 simulations.
