library("easypackages")
libraries("MASS","ggplot2","vcd","psych","survival","SPREDA","extRemes","evd")
dir.create("../Results",showWarnings = F)
dir.create("../Results/Interpolation",showWarnings = F)
dir.create("../Results/Interpolation/Graphs",showWarnings = F)
#========== Read the table of the depth probabilities====================================
df1=read.csv("../Data/Table2.csv")
Synthetic=c()
for (i in 1:nrow(df1)){
Synthetic=c(Synthetic,rep(df1$Depth[i],df1$Probability[i]*1000))  
}
#====================== Fit gumbel distribution ============================================
fit_gumbel <- fevd(x=Synthetic, type="Gumbel", method="MLE")
#summary(fit_gumbel)
muG <- fit_gumbel $results$par[1]
sigmaG <- fit_gumbel $results$par[2]
set.seed(123)
Random_Depths=rgumbel(510000, muG,sigmaG)


dgumbel(3.8,muG,sigmaG)

df2=data.frame(Random_Depths=Random_Depths)
df2=subset(df2,df2$Random_Depths<=3.78)
df3=df2[1:50000,]
df3=as.data.frame(df3)
colnames(df3)=c("Random_Depths")
l1=describe(df2$Random_Depths)
write.csv(l1,"../Results/Interpolation/Summary_Synthetic_Data.csv")
#========= Creating a histogram with the corresponding dencity plot of the random depths ===
g3=ggplot(df2, aes(x=Random_Depths)) + 
  geom_histogram(aes(y=..density..))+
  geom_density(alpha=.2, fill="#FF6666")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))+
  theme_light()
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist.pdf",g3)
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist.jpg",g3)
#========= Save the descriptive statistics for the synthetic Data ==========================
l1=as.data.frame(describe(df2))
write.csv(l1,"../Results/Interpolation/Summary_Synthetic_Data.csv")
#========= Read damage table of damages ====================================================
df3=read.csv("../Data/Table1.csv",header=T) # Reading the damage table

#========= Creates four types of damage levels based on the elevation ======================
Freeboard=seq(-4.5,-2,0.5)
for (j in 1:length(Freeboard)){
  temp=df2
  temp$Random_Depths=-temp$Random_Depths-Freeboard[j]
#========== Adds the damage using interpolations of the normal distributions indicated in the damage table===
  for (i in 1:nrow(temp)){
    temp1=subset(df3,df3$dpt==floor(temp$Random_Depths[i])|df3$dpt==ceiling(temp$Random_Depths[i]))
    l2=lm(dmg~dpt,data=temp1)
    y1=temp$Random_Depths[i]*l2$coefficients[2]+l2$coefficients[1]
    l3=lm(sd~dpt,data=temp1)
    y2=temp$Random_Depths[i]*l3$coefficients[2]+l3$coefficients[1]
    temp$Damage[i]=rnorm(1,mean=y1,sd=y2)
  }
  write.csv(temp,file=paste0("../Results/Interpolation/Damages/Freeboard_",Freeboard[j],".csv"))
  g4=ggplot(temp, aes(x=Random_Depths, y=Damage)) + 
    geom_point()+
    geom_smooth()+
    theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))
  ggsave(paste0("../Results/Interpolation/Graphs/Random_Depth_vs_Damage_Freeboard_",Freeboard[j],".pdf"),g4)
  ggsave(paste0("../Results/Interpolation/Graphs/Random_Depth_vs_Damage_Freeboard_",Freeboard[j],".jpg"),g4)
  l1=as.data.frame(describe(temp))
  write.csv(l1,paste0("../Results/Interpolation/Damages/Summary_Freeboard_",Freeboard[j],".csv")) 
}



