setwd("~/PhD/Variability in causal effects/CaseStudy")
library(haven)

data.pred1 <- read_sas("ICE/casestudy/cfive333.sas7bdat", 
                       NULL)
data.pred1<-as.data.frame(data.pred1)
#data.pred1<-data.pred1[seq(1,length(data.pred1[,1]),10),]
colnames(data.pred1)[which(colnames(data.pred1)=="memaxdemaxratio")]<-"memaxdemaxratioT"
data.pred1$memaxdemaxratio<-data.pred1$mu+data.pred1$e
data.pred1$Y0<-data.pred1$memaxdemaxratio-data.pred1$ICE*data.pred1$fattyliver
data.pred1$Y1<-data.pred1$Y0+data.pred1$ICE
data.pred1<-data.pred1[,-c(which(colnames(data.pred1)%in%c("s0","age","SBP","mu")))]
data.pred1$sim<-1

data.pred2 <- read_sas("ICE/casestudy/cfive2021.sas7bdat")
data.pred2<-as.data.frame(data.pred2)
#data.pred2<-data.pred2[seq(1,length(data.pred2[,1]),10),]
colnames(data.pred2)[which(colnames(data.pred2)=="memaxdemaxratio")]<-"memaxdemaxratioT"
data.pred2$memaxdemaxratio<-data.pred2$mu+data.pred2$e
data.pred2$Y0<-data.pred2$memaxdemaxratio-data.pred2$ICE*data.pred2$fattyliver
data.pred2$Y1<-data.pred2$Y0+data.pred2$ICE
data.pred2<-data.pred2[,-c(which(colnames(data.pred2)%in%c("s0","age","SBP","mu")))]
data.pred2$sim<-2

data.pred3 <- read_sas("ICE/casestudy/cfive3031993.sas7bdat")
data.pred3<-as.data.frame(data.pred3)
#data.pred3<-data.pred3[seq(1,length(data.pred3[,1]),10),]
colnames(data.pred3)[which(colnames(data.pred3)=="memaxdemaxratio")]<-"memaxdemaxratioT"
data.pred3$memaxdemaxratio<-data.pred3$mu+data.pred3$e
data.pred3$Y0<-data.pred3$memaxdemaxratio-data.pred3$ICE*data.pred3$fattyliver
data.pred3$Y1<-data.pred3$Y0+data.pred3$ICE
data.pred3<-data.pred3[,-c(which(colnames(data.pred3)%in%c("s0","age","SBP","mu")))]
data.pred3$sim<-3

data.pred4 <- read_sas("ICE/casestudy/cfive23092011.sas7bdat")
data.pred4<-as.data.frame(data.pred4)
#data.pred4<-data.pred4[seq(1,length(data.pred4[,1]),10),]
colnames(data.pred4)[which(colnames(data.pred4)=="memaxdemaxratio")]<-"memaxdemaxratioT"
data.pred4$memaxdemaxratio<-data.pred4$mu+data.pred4$e
data.pred4$Y0<-data.pred4$memaxdemaxratio-data.pred4$ICE*data.pred4$fattyliver
data.pred4$Y1<-data.pred4$Y0+data.pred4$ICE
data.pred4<-data.pred4[,-c(which(colnames(data.pred4)%in%c("s0","age","SBP","mu")))]
data.pred4$sim<-4

#Rhat
n.chain.s<-length(as.vector(unlist(data.pred1[which(data.pred1$ID==4),1])))
data.ICE4<-cbind(head(unlist(data.pred1[which(data.pred1$ID==4),2]),n.chain.s/2),
                 tail(unlist(data.pred1[which(data.pred1$ID==4),2]),n.chain.s/2),
                 head(unlist(data.pred2[which(data.pred1$ID==4),2]),n.chain.s/2),
                 tail(unlist(data.pred2[which(data.pred1$ID==4),2]),n.chain.s/2),
                 head(unlist(data.pred3[which(data.pred1$ID==4),2]),n.chain.s/2),
                 tail(unlist(data.pred3[which(data.pred1$ID==4),2]),n.chain.s/2),
                  head(unlist(data.pred4[which(data.pred1$ID==4),2]),n.chain.s/2),
                 tail(unlist(data.pred4[which(data.pred1$ID==4),2]),n.chain.s/2))

library(rstan)
Rhat(data.ICE4)
ess_bulk(data.ICE4)
ess_tail(data.ICE4)

#subset
data4<-rbind(data.pred1[which(data.pred1$ID==4),],data.pred2[which(data.pred2$ID==4),],data.pred3[which(data.pred3$ID==4),],data.pred4[which(data.pred4$ID==4),])
data7<-rbind(data.pred1[which(data.pred1$ID==7),],data.pred2[which(data.pred2$ID==7),],data.pred3[which(data.pred3$ID==7),],data.pred4[which(data.pred4$ID==7),])


#Traceplots
par(mfrow=c(1,2))
plot(data4[which(data4$sim==1),2],col=1,type="l",ylim=c(-7.5,7.5),ylab="ICE",main="ID=4")
lines(data4[which(data4$sim==2),2],col=2)
lines(data4[which(data4$sim==5),2],col=4)
lines(data4[which(data4$sim==3),2],col=3)

legend(500, -5.5,c(1,2,3,4),col=c(1,2,3,4),ncol=4,title="chain",lwd=2)

plot(data7[which(data7$sim==1),2],col=1,type="l",ylim=c(-7.5,7.5),ylab="ICE",main="ID=7")
lines(data7[which(data7$sim==2),2],col=2)
lines(data7[which(data7$sim==5),2],col=4)
lines(data7[which(data7$sim==3),2],col=3)
legend(500, -5.5,c(1,2,3,4),col=c(1,2,3,4),ncol=4,title="chain",lwd=2)

#Posterior distributions
data0<-rbind(data.pred1[,c(1,15,13,14)],data.pred2[,c(1,15,13,14)],data.pred3[,c(1,15,13,14)],data.pred5[,c(1,15,13,14)])
data0<-data0[order(data0$sim,data0$Iteration),]
data0$nID <- cumsum(!duplicated(data0[,c(1,2)]))
data0<-data0[,-c(1)]

data<-rbind(data.pred1[,c(1,15,8,12)],data.pred2[,c(1,15,8,12)],data.pred3[,c(1,15,8,12)],data.pred5[,c(1,15,8,12)])
data<-data[order(data$sim,data$Iteration),]
data$nID <- cumsum(!duplicated(data[,c(1,2)]))
data<-data[,-c(1)]

dataI<-rbind(data.pred1[,c(1,15,2)],data.pred2[,c(1,15,2)],data.pred3[,c(1,15,2)],data.pred5[,c(1,15,2)])
dataI<-dataI[order(dataI$sim,dataI$Iteration),]
dataI$nID <- cumsum(!duplicated(dataI[,c(1,2)]))
dataI<-dataI[,-c(1)]

dataC1<-rbind(data.pred1[,c(1,15,10,12)],data.pred2[,c(1,15,10,12)],data.pred3[,c(1,15,10,12)],data.pred5[,c(1,15,10,12)])
dataC1<-dataC1[order(dataC1$sim,dataC1$Iteration),]
dataC1$nID <- cumsum(!duplicated(dataC1[,c(1,2)]))
dataC1<-dataC1[,-c(1)]

dataC2<-rbind(data.pred1[,c(1,15,5,12)],data.pred2[,c(1,15,5,12)],data.pred3[,c(1,15,5,12)],data.pred5[,c(1,15,5,12)])
dataC2<-dataC2[order(dataC2$sim,dataC2$Iteration),]
dataC2$nID <- cumsum(!duplicated(dataC2[,c(1,2)]))
dataC2<-dataC2[,-c(1)]

dataC3<-rbind(data.pred1[,c(1,15,6,12)],data.pred2[,c(1,15,6,12)],data.pred3[,c(1,15,6,12)],data.pred5[,c(1,15,6,12)])
dataC3<-dataC3[order(dataC3$sim,dataC3$Iteration),]
dataC3$nID <- cumsum(!duplicated(dataC3[,c(1,2)]))
dataC3<-dataC3[,-c(1)]

dataC4<-rbind(data.pred1[,c(1,15,11,12)],data.pred2[,c(1,15,11,12)],data.pred3[,c(1,15,11,12)],data.pred5[,c(1,15,11,12)])
dataC4<-dataC4[order(dataC4$sim,dataC4$Iteration),]
dataC4$nID <- cumsum(!duplicated(dataC4[,c(1,2)]))
dataC4<-dataC4[,-c(1)]

dataC5<-rbind(data.pred1[,c(1,15,7,12)],data.pred2[,c(1,15,7,12)],data.pred3[,c(1,15,7,12)],data.pred5[,c(1,15,7,12)])
dataC5<-dataC5[order(dataC5$sim,dataC5$Iteration),]
dataC5$nID <- cumsum(!duplicated(dataC5[,c(1,2)]))
dataC5<-dataC5[,-c(1)]

rm(data.pred2,data.pred3,data.pred4)

#ACE and P(ICE>0)
ACEs<-sapply(unique(dataI$nID),function(x){mean(dataI[dataI$nID==x,]$ICE)})
Ps<-sapply(unique(dataI$nID),function(x){length(which(dataI[dataI$nID==x,]$ICE>0))/length(which(dataI$nID==x))})


#ICE distribution
mykernel<-function(w,h,x){
    return((1/(length(x)*h))*sum(dnorm((w-x)/h)))
}

ti = seq(-1,2,0.05)
dataICE<-dataI$ICE
bw<-density(dataICE)$bw
yICE<-sapply(ti,function(w){mykernel(w,bw,dataICE)})
ICE<-aggregate(ICE~nID,dataI,function(x){sapply(ti,function(w){mykernel(w,bw,x)})})[,2]

rm(dataI)


#Potential outcome densities
t = seq(2,15,0.1)

dataY0<-data0$Y0
bw<-density(dataY0)$bw
yY0<-sapply(t,function(w){mykernel(w,bw,dataY0)})
Y0<-aggregate(Y0~nID,data0,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataY1<-data0$Y1
bw<-density(dataY1)$bw
yY1<-sapply(t,function(w){mykernel(w,bw,dataY1)})
Y1<-aggregate(Y1~nID,data0,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(data0)

#Sampling densities
bwT<-density(na.omit(data.pred1[data.pred1$fattyliver==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYA0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$fattyliver==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$fattyliver==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratio))$bw
yYA1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$fattyliver==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$AGEb==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYAGE0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$AGEb==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$AGEb==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYAGE1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$AGEb==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$SEX==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYSEX0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$SEX==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})
bwT<-density(na.omit(data.pred1[data.pred1$SEX==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYSEX1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$SEX==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$diab==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYDIAB0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$diab==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})
bwT<-density(na.omit(data.pred1[data.pred1$diab==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYDIAB1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$diab==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$SBPb==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYSBP0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$SBPb==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})
bwT<-density(na.omit(data.pred1[data.pred1$SBPb==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYSBP1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$SBPb==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

bwT<-density(na.omit(data.pred1[data.pred1$hrx==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYHRX0T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$hrx==0 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})
bwT<-density(na.omit(data.pred1[data.pred1$hrx==1 & data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))$bw
yYHRX1T<-sapply(t,function(w){mykernel(w,bwT,na.omit(data.pred1[data.pred1$hrx==1 &data.pred1$Iteration==data.pred1$Iteration[1],]$memaxdemaxratioT))})

rm(data.pred1)

#Posterior predictive distributions

#Fatty liver strata
dataA<-data[data$fattyliver==0,]
dataYA0<-data[data$fattyliver==0,]$memaxdemaxratio
bw<-density(dataYA0)$bw
yYA0<-sapply(t,function(w){mykernel(w,bw,dataYA0)})
YA0<-aggregate(memaxdemaxratio~nID,dataA,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

t = seq(2,15,0.1)

dataA<-data[data$fattyliver==1,]
dataYA1<-data[data$fattyliver==1,]$memaxdemaxratio
bw<-density(dataYA1)$bw
yYA1<-sapply(t,function(w){mykernel(w,bw,dataYA1)})
YA1<-aggregate(memaxdemaxratio~nID,dataA,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(data)

#AGE strata
dataAGE<-dataC1[dataC1$AGEb==0,]
dataYAGE0<-dataAGE$memaxdemaxratio
bw<-density(dataYAGE0)$bw
yYAGE0<-sapply(t,function(w){mykernel(w,bw,dataYAGE0)})
YAGE0<-aggregate(memaxdemaxratio~nID,dataAGE,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataAGE<-dataC1[dataC1$AGEb==1,]
dataYAGE1<-dataAGE$memaxdemaxratio
bw<-density(dataYAGE1)$bw
yYAGE1<-sapply(t,function(w){mykernel(w,bw,dataYAGE1)})
YAGE1<-aggregate(memaxdemaxratio~nID,dataAGE,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(dataC1)

#SEX strata
dataSEX<-dataC2[dataC2$SEX==0,]
dataYSEX0<-dataC2[dataC2$SEX==0,]$memaxdemaxratio
bw<-density(dataYSEX0)$bw
yYSEX0<-sapply(t,function(w){mykernel(w,bw,dataYSEX0)})
YSEX0<-aggregate(memaxdemaxratio~nID,dataSEX,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataSEX<-dataC2[dataC2$SEX==1,]
dataYSEX1<-dataC2[dataC2$SEX==1,]$memaxdemaxratio
bw<-density(dataYSEX1)$bw
yYSEX1<-sapply(t,function(w){mykernel(w,bw,dataYSEX1)})
YSEX1<-aggregate(memaxdemaxratio~nID,dataSEX,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(dataC2)

#DIAB strata
dataDIAB<-dataC3[dataC3$diab==0,]
dataYDIAB0<-dataC3[dataC3$diab==0,]$memaxdemaxratio
bw<-density(dataYDIAB0)$bw
yYDIAB0<-sapply(t,function(w){mykernel(w,bw,dataYDIAB0)})
YDIAB0<-aggregate(memaxdemaxratio~nID,dataDIAB,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataDIAB<-dataC3[dataC3$diab==1,]
dataYDIAB1<-dataC3[dataC3$diab==1,]$memaxdemaxratio
bw<-density(dataYDIAB1)$bw
yYDIAB1<-sapply(t,function(w){mykernel(w,bw,dataYDIAB1)})
YDIAB1<-aggregate(memaxdemaxratio~nID,dataDIAB,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(dataC3)

#SBP strata
dataSBP<-dataC4[dataC4$SBPb==0,]
dataYSBP0<-dataC4[dataC4$SBPb==0,]$memaxdemaxratio
bw<-density(dataYSBP0)$bw
yYSBP0<-sapply(t,function(w){mykernel(w,bw,dataYSBP0)})
YSBP0<-aggregate(memaxdemaxratio~nID,dataSBP,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataSBP<-dataC4[dataC4$SBPb==1,]
dataYSBP1<-dataC4[dataC4$SBPb==1,]$memaxdemaxratio
bw<-density(dataYSBP1)$bw
yYSBP1<-sapply(t,function(w){mykernel(w,bw,dataYSBP1)})
YSBP1<-aggregate(memaxdemaxratio~nID,dataSBP,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(dataC4)

#HRX strata
dataHRX<-dataC5[dataC5$hrx==0,]
dataYHRX0<-dataC5[dataC5$hrx==0,]$memaxdemaxratio
bw<-density(dataYHRX0)$bw
yYHRX0<-sapply(t,function(w){mykernel(w,bw,dataYHRX0)})
YHRX0<-aggregate(memaxdemaxratio~nID,dataHRX,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

dataHRX<-dataC5[dataC5$hrx==1,]
dataYHRX1<-dataC5[dataC5$hrx==1,]$memaxdemaxratio
bw<-density(dataYHRX1)$bw
yYHRX1<-sapply(t,function(w){mykernel(w,bw,dataYHRX1)})
YHRX1<-aggregate(memaxdemaxratio~nID,dataHRX,function(x){sapply(t,function(w){mykernel(w,bw,x)})})[,2]

rm(dataC5)
