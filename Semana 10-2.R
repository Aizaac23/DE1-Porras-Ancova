library(psych)
caso$Bloque<-factor(caso$Bloque)
caso$trat<-factor(caso$trat)
describeBy(x = caso$ganancia,group = caso$trat)
describeBy(x = caso$ganancia,group = caso$Bloque)

library(ggplot2)
g1<-ggplot(caso, aes(x = trat, y = ganancia,fill=trat)) + 
  geom_boxplot()
g1
g2<-ggplot(caso, aes(x = Bloque, y = ganancia,fill=Bloque)) + 
  geom_boxplot()
g2

g3<-ggplot(data = caso, aes(x = inicial, y = ganancia)) + 
  geom_point(aes(colour=  trat))
g3


#Cuadro ANCOVA
modelox<-lm(inicial~Bloque+trat,data=caso)
anvax<-anova(modelox)
anvax
modeloy<-lm(ganancia~Bloque+trat,data=caso)
anvay<-anova(modeloy)
anvay

tcy<-sum(caso$ganancia)^2/30
scto<-sum(caso$ganancia^2)-tcy
sctra<-sum(tapply(caso$ganancia,caso$trat,sum)^2)/5-tcy
scblo<-sum(tapply(caso$ganancia,caso$Bloque,sum)^2)/6-tcy

tcxy<-sum(caso$ganancia)*sum(caso$inicial)/30
sptoxy<-sum(caso$ganancia*caso$inicial)-tcxy
sptraxy<-sum(tapply(caso$ganancia,caso$trat,sum)*tapply(caso$inicial,caso$trat,sum))/5-tcxy
spbloxy<-sum(tapply(caso$ganancia,caso$Bloque,sum)*tapply(caso$inicial,caso$Bloque,sum))/6-tcxy
scexy<-sptoxy-sptraxy-spbloxy # hasta acá es lo mismo que 10.2
spxy<-c(spbloxy,sptraxy,scexy)

SCySP<-cbind(anvay[,1],anvax[,2],spxy,anvay[,2])

SCEaju<-SCySP[3,4]-SCySP[3,3]^2/SCySP[3,2]
SCTraju<-sum(SCySP[2:3,4])-sum(SCySP[2:3,3])^2/sum(SCySP[2:3,2])
SCDif<-SCTraju-SCEaju
GLEAj<-anvay[3,1]-1
CMEaj<-SCEaju/GLEAj
CMTraj<-SCDif/anvax[2,1]

ancova<-cbind(SCySP,c(0,0,SCEaju),c(0,0,GLAj),c(0,0,CMEaj))

#Hipótesis para evaluar beta
Fcal1<-(ancova[3,3]^2/ancova[3,2])/ancova[3,7]
qf(0.05,1,19,lower.tail = F)
pf(Fcal1,1,19,lower.tail = F)

#Hipótesis para evaluar las medias ajustadas
Fcal2<-CMTraj/CMEaj
qf(0.05,5,19,lower.tail = F)
pf(Fcal2,5,19,lower.tail = F)
