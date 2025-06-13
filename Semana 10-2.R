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
scexy<-sptoxy-sptraxy-spbloxy
