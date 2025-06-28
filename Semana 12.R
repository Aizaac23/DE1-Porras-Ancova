library(agricolae)
trt <-c("a1b1","a1b2","a2b1","a2b2","a3b1","a3b2")
repe <-rep(3,6)
#Experimento Factorial en DCA
r1<-design.crd(trt,repe,serie=2,seed=12)
r1$book[,-1]
#Experimento Factorial en DBCA
design.rcbd(trt, r=3, seed = 12)


