library("easypackages")
libraries("MASS","ggplot2","vcd","psych","survival","SPREDA","extRemes","evd","fitdistrplus","ismev")
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
fit_ps <- fitdist(Synthetic, "pois",method="mme")
#summary(fit_gumbel)
lamda <- fit_ps$estimate

set.seed(123)
Random_Depths=rpois(5100, lamda)

df22=data.frame(Random_Depths=Random_Depths)
df22=subset(df22,df22$Random_Depths<=11)
df2=df22[1:5000,]
df2=as.data.frame(df2)
colnames(df2)=c("Random_Depths")
l1=describe(df2$Random_Depths)
write.csv(l1,"../Results/Interpolation/Summary_Synthetic_Data_Pois.csv")
#========= Creating a histogram with the corresponding dencity plot of the random depths ===
g3=ggplot(df2, aes(x=Random_Depths)) + 
  geom_histogram(aes(y=..density..))+
  #geom_density(alpha=.2, fill="#FF6666")+
  theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))+
  theme_light()
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist_Pois.pdf",g3)
ggsave("../Results/Interpolation/Graphs/Random_Depth_hist_Pois.jpg",g3)

#========= Read damage table of damages ====================================================
df3=read.csv("../Data/Table1.csv",header=T) # Reading the damage table

#========= Creates four types of damage levels based on the elevation ======================
Freeboard=seq(3,5,0.5)
for (j in 1:length(Freeboard)){
  temp=df2
  temp$Random_Depths=temp$Random_Depths-Freeboard[j]
#========== Adds the damage using interpolations of the normal distributions indicated in the damage table===
  for (i in 1:nrow(temp)){
    temp1=subset(df3,df3$dpt==floor(temp$Random_Depths[i])|df3$dpt==ceiling(temp$Random_Depths[i]))
    l2=lm(dmg~dpt,data=temp1)
    y1=temp$Random_Depths[i]*l2$coefficients[2]+l2$coefficients[1]
    l3=lm(sd~dpt,data=temp1)
    y2=temp$Random_Depths[i]*l3$coefficients[2]+l3$coefficients[1]
    temp$Damage[i]=rnorm(1,mean=y1,sd=y2)
  }
  write.csv(temp,file=paste0("../Results/Interpolation/Damages/Freeboard_",Freeboard[j],"_Pois.csv"))
  g4=ggplot(temp, aes(x=Random_Depths, y=Damage)) + 
    geom_point()+
    geom_smooth()+
    theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))
  ggsave(paste0("../Results/Interpolation/Graphs/Random_Depth_vs_Damage_Freeboard_",Freeboard[j],"_Pois.pdf"),g4)
  ggsave(paste0("../Results/Interpolation/Graphs/Random_Depth_vs_Damage_Freeboard_",Freeboard[j],"_Pois.jpg"),g4)
  l1=as.data.frame(describe(temp))
  write.csv(l1,paste0("../Results/Interpolation/Damages/Summary_Freeboard_",Freeboard[j],"_Pois.csv")) 
}

warnings()
#take care of negative depths =, review table of damages
