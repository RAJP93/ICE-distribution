library(tikzDevice)
outfixed3 <- read.csv("~/PhD/Variability in causal effects/outfixed3-0907.csv")
outfixed2 <- read.csv("~/PhD/Variability in causal effects/outfixed2newFull.csv")
outfixed1 <- read.csv("~/PhD/Variability in causal effects/outfixed1-2307.csv")

#Case 1
f1<-function(x){dnorm(x,mean=-15, sd=10)}

tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/ExampleDist1.tex",width=3.5,height=3.5)

# plot((density(unlist(dat1[1,]))),xlim=c(-75,50),ylim=c(0,0.075),col="lightgray",main="", xlab="",ylab="")
# for (i in 2:length(dat1[,1])){
#     lines((density(unlist(dat1[i,]))),col="lightgray")
# }
# lines((density(unlist(dat1))),col=rgb(27/255,158/255,119/255),lwd=2)
# lines(seq(-75,50,1),sapply (seq(-75,50,1), f1),lwd=2,col="yellow")

dat1<-outfixed1[outfixed1$sim==1,-c(1:21,1022:1025)]
d<-density(unlist(dat1[1,]))
keep.y0<-approx(d$x, d$y, xout = seq(-75,50,2))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*2)
plot(seq(-75,50,2),keep.y,main="",col="lightgray",xlim=c(-75,50),ylim=c(0,0.075), type="l", xlab="",ylab="")
k<-1

for(i in 2:1000){
    d<-density(unlist(dat1[i,]))
    keep.y0<-approx(d$x, d$y, xout = seq(-75,50,2))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-keep.y0/sum(keep.y0*2)
    lines(seq(-75,50,2),keep.y,main="",col="lightgray")
}

d<-density(unlist(dat1))
keep.y0<-approx(d$x, d$y, xout = seq(-75,50,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
lines(seq(-75,50,1),keep.y,main="",col=rgb(27/255,158/255,119/255),lwd=2)

lines(seq(-75,50,1),sapply (seq(-75,50,1), f1),lwd=2,col="yellow")
dev.off()


mean<-list()
sd<-list()
skew<-list()
kurt<-list()
thr<-list()
srbi<-list()
q5<-list()
q25<-list()
q50<-list()
q75<-list()
q95<-list()

for(i in 1:100){
    dat<-(outfixed1[outfixed1$sim==i,c(3:17)])
    datR<-(outfixed1[outfixed1$sim==i,-c(1:21,1022:1025)])
    
    #Non-central moments equal to weighted sum of non-central moments of the Gaussian components
    mu4<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^4+6*x[c(6:10)]^2*x[c(11:15)]^2+3*x[c(11:15)]^4))}))
    mu3<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^3+3*x[c(6:10)]*x[c(11:15)]^2))}))
    mu2<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^2+x[c(11:15)]^2))}))
    mu1<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]))}))
    sigma<-sqrt(mu2-mu1^2)
    #https://mathworld.wolfram.com/CentralMoment.html
    kurt[[i]]<-(-3*mu1^4+6*mu1^2*mu2-4*mu1*mu3+mu4)/(sigma^4)
    skew[[i]]<-(2*mu1^3-3*mu1*mu2+mu3)/(sigma^3)
    
    mean[[i]]<-mu1
    sd[[i]]<-sigma
    thr[[i]]<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*c(1-pnorm(0,mean=x[6],sd=x[11]),1-pnorm(0,mean=x[7],sd=x[12]),1-pnorm(0,mean=x[8],sd=x[13]), 1-pnorm(0,mean=x[9],sd=x[14]),1-pnorm(0,mean=x[10],sd=x[15])))}))
    srbi[[i]]<-(skew[[i]]^2+1)/kurt[[i]]
    
    q5[[i]]<-(apply(datR,1,function(x){quantile(x,0.05)}))
    q25[[i]]<-(apply(datR,1,function(x){quantile(x,0.25)}))
    q50[[i]]<-(apply(datR,1,function(x){quantile(x,0.50)}))
    q75[[i]]<-(apply(datR,1,function(x){quantile(x,0.75)}))
    q95[[i]]<-(apply(datR,1,function(x){quantile(x,0.95)}))
    
    
}

mean(unlist(lapply(mean,function(x){mean(x)})))
mean(unlist(lapply(sd,function(x){mean(x)})))
mean(unlist(lapply(skew,function(x){mean(x)})))
mean(unlist(lapply(kurt,function(x){mean(x)})))
mean(unlist(lapply(thr,function(x){mean(x)})))
mean(unlist(lapply(srbi,function(x){mean(x)})))
mean(unlist(lapply(q5,function(x){mean(x)})))
mean(unlist(lapply(q25,function(x){mean(x)})))
mean(unlist(lapply(q50,function(x){mean(x)})))
mean(unlist(lapply(q75,function(x){mean(x)})))
mean(unlist(lapply(q95,function(x){mean(x)})))


# mean(unlist(lapply(mean,function(x){as.numeric(quantile(x,0.025)< -15&quantile(x,0.975)> -15)})))
# mean(unlist(lapply(sd,function(x){as.numeric(quantile(x,0.025)<56.4652&quantile(x,0.975)>56.4652)})))
# mean(unlist(lapply(skew,function(x){as.numeric(quantile(x,0.025)<2.9388&quantile(x,0.975)>2.9388)})))
# mean(unlist(lapply(kurt,function(x){as.numeric(quantile(x,0.025)<21.5073&quantile(x,0.975)>21.5073)})))
# mean(unlist(lapply(thr,function(x){as.numeric(quantile(x,0.025)<0.265082&quantile(x,0.975)>0.265082)})))

mean(unlist(lapply(mean,function(x){as.numeric(quantile(x,0.025)< -15&quantile(x,0.975)> -15)})))
mean(unlist(lapply(sd,function(x){as.numeric(quantile(x,0.025)<10&quantile(x,0.975)>10)})))
mean(unlist(lapply(skew,function(x){as.numeric(quantile(x,0.025)<0&quantile(x,0.975)>0)})))
mean(unlist(lapply(kurt,function(x){as.numeric(quantile(x,0.025)<3&quantile(x,0.975)>3)})))
mean(unlist(lapply(thr,function(x){as.numeric(quantile(x,0.025)<0.0668072&quantile(x,0.975)>0.0668072)})))
mean(unlist(lapply(q5,function(x){as.numeric(quantile(x,0.025)<qnorm(0.05,mean=-15,sd=10)&quantile(x,0.975)>qnorm(0.05,mean=-15,sd=10))})))
mean(unlist(lapply(q25,function(x){as.numeric(quantile(x,0.025)<qnorm(0.25,mean=-15,sd=10)&quantile(x,0.975)>qnorm(0.25,mean=-15,sd=10))})))
mean(unlist(lapply(q50,function(x){as.numeric(quantile(x,0.025)<qnorm(0.50,mean=-15,sd=10)&quantile(x,0.975)>qnorm(0.50,mean=-15,sd=10))})))
mean(unlist(lapply(q75,function(x){as.numeric(quantile(x,0.025)<qnorm(0.75,mean=-15,sd=10)&quantile(x,0.975)>qnorm(0.75,mean=-15,sd=10))})))
mean(unlist(lapply(q95,function(x){as.numeric(quantile(x,0.025)<qnorm(0.95,mean=-15,sd=10)&quantile(x,0.975)>qnorm(0.95,mean=-15,sd=10))})))

tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/Dist1.tex",width=3.5,height=3.5)

#Fig 4A

#Specify trunc to work around extreme values that ruin the kernel density estimation
trunc<-100

dat<-as.numeric(unlist(outfixed1[outfixed1$sim==1,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
dat<-dat[which(abs(dat)<trunc)]
d<-density(dat)
keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
plot(seq(-trunc,trunc,1),keep.y,main="",col=rgb(27/255,158/255,119/255),xlim=c(-75,50),ylim=c(0,0.075), type="l", xlab="",ylab="")
k<-1

for(i in 2:100){
    dat<-as.numeric(unlist(outfixed1[outfixed1$sim==i,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
    dat<-dat[which(abs(dat)<trunc)]
    d<-density(dat)
    keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-rbind(keep.y, keep.y0/sum(keep.y0*1))
    lines(seq(-trunc,trunc,1),keep.y[i,],col=rgb(27/255,158/255,119/255))
    
}
lines(seq(-trunc,trunc,1), unlist(apply(keep.y,2,function(x){mean(x)})),col="black",lwd=2)

lines(seq(-trunc,trunc,1),sapply (seq(-trunc,trunc,1), f1),lwd=2,col="yellow")

dev.off()

#Case 2
f2<-function(x){dlnorm(x+exp(4.25)+15,meanlog=4, sdlog=sqrt(0.5))}

# tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/ExampleDist2.tex",width=3.5,height=3.5)
# 
# dat1<-outfixed2[outfixed2$sim==5,-c(1:21,1022:1025)]
# plot((density(unlist(dat1[1,]))),xlim=c(-100,150),ylim=c(0,0.03),col="lightgray",main="",xlab="",ylab="")
# for (i in 2:length(dat1[,1])){
#     lines((density(unlist(dat1[i,]))),col="lightgray")
# }
# lines((density(unlist(dat1))),col=rgb(217/255,95/255,2/255),lwd=2)
# lines(seq(-100,150,1),sapply (seq(-100,150,1), f2),lwd=2,col="yellow")
# dev.off()


tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/ExampleDist2.tex",width=3.5,height=3.5)

# plot((density(unlist(dat1[1,]))),xlim=c(-100,150),ylim=c(0,0.075),col="lightgray",main="", xlab="",ylab="")
# for (i in 2:length(dat1[,1])){
#     lines((density(unlist(dat1[i,]))),col="lightgray")
# }
# lines((density(unlist(dat1))),col=rgb(27/255,158/255,119/255),lwd=2)
# lines(seq(-100,150,1),sapply (seq(-100,150,1), f1),lwd=2,col="yellow")

dat1<-outfixed2[outfixed2$sim==1,-c(1:21,1022:1025)]
d<-density(unlist(dat1[1,]))
keep.y0<-approx(d$x, d$y, xout = seq(-100,150,5))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*5)
plot(seq(-100,150,5),keep.y,main="",col="lightgray",xlim=c(-100,150),ylim=c(0,0.03), type="l", xlab="",ylab="")
k<-1

for(i in 2:1000){
    d<-density(unlist(dat1[i,]))
    keep.y0<-approx(d$x, d$y, xout = seq(-100,150,5))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-keep.y0/sum(keep.y0*5)
    lines(seq(-100,150,5),keep.y,main="",col="lightgray")
}

d<-density(unlist(dat1))
keep.y0<-approx(d$x, d$y, xout = seq(-100,150,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
lines(seq(-100,150,1),keep.y,main="",col=rgb(217/255,95/255,2/255),lwd=2)

lines(seq(-100,150,1),sapply (seq(-100,150,1), f2),lwd=2,col="yellow")
dev.off()

mean.2<-list()
sd.2<-list()
skew.2<-list()
kurt.2<-list()
thr.2<-list()
srbi.2<-list()
q5.2<-list()
q25.2<-list()
q50.2<-list()
q75.2<-list()
q95.2<-list()

for(i in 1:100){
    dat<-(outfixed2[outfixed2$sim==i,c(3:17)])
    datR<-(outfixed2[outfixed2$sim==i,-c(1:21,1022:1025)])
    
    #Non-central moments equal to weighted sum of non-central moments of the Gaussian components
    mu4<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^4+6*x[c(6:10)]^2*x[c(11:15)]^2+3*x[c(11:15)]^4))}))
    mu3<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^3+3*x[c(6:10)]*x[c(11:15)]^2))}))
    mu2<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^2+x[c(11:15)]^2))}))
    mu1<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]))}))
    sigma<-sqrt(mu2-mu1^2)
    #https://mathworld.wolfram.com/CentralMoment.html
    kurt.2[[i]]<-(-3*mu1^4+6*mu1^2*mu2-4*mu1*mu3+mu4)/(sigma^4)
    skew.2[[i]]<-(2*mu1^3-3*mu1*mu2+mu3)/(sigma^3)
    
    mean.2[[i]]<-mu1
    sd.2[[i]]<-sigma
    thr.2[[i]]<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*c(1-pnorm(0,mean=x[6],sd=x[11]),1-pnorm(0,mean=x[7],sd=x[12]),1-pnorm(0,mean=x[8],sd=x[13]), 1-pnorm(0,mean=x[9],sd=x[14]),1-pnorm(0,mean=x[10],sd=x[15])))}))
    srbi.2[[i]]<-(skew.2[[i]]^2+1)/kurt.2[[i]]
    
    q5.2[[i]]<-(apply(datR,1,function(x){quantile(x,0.05)}))
    q25.2[[i]]<-(apply(datR,1,function(x){quantile(x,0.25)}))
    q50.2[[i]]<-(apply(datR,1,function(x){quantile(x,0.50)}))
    q75.2[[i]]<-(apply(datR,1,function(x){quantile(x,0.75)}))
    q95.2[[i]]<-(apply(datR,1,function(x){quantile(x,0.95)}))
    
    
}

mean(unlist(lapply(mean.2,function(x){mean(x)})))
mean(unlist(lapply(sd.2,function(x){mean(x)})))
mean(unlist(lapply(skew.2,function(x){mean(x)})))
mean(unlist(lapply(kurt.2,function(x){mean(x)})))
mean(unlist(lapply(thr.2,function(x){mean(x)})))
mean(unlist(lapply(srbi.2,function(x){mean(x)})))
mean(unlist(lapply(q5.2,function(x){mean(x)})))
mean(unlist(lapply(q25.2,function(x){mean(x)})))
mean(unlist(lapply(q50.2,function(x){mean(x)})))
mean(unlist(lapply(q75.2,function(x){mean(x)})))
mean(unlist(lapply(q95.2,function(x){mean(x)})))


mean(unlist(lapply(mean.2,function(x){as.numeric(quantile(x,0.025)< -15&quantile(x,0.975)> -15)})))
mean(unlist(lapply(sd.2,function(x){as.numeric(quantile(x,0.025)<56.4652&quantile(x,0.975)>56.4652)})))
mean(unlist(lapply(skew.2,function(x){as.numeric(quantile(x,0.025)<2.9388&quantile(x,0.975)>2.9388)})))
mean(unlist(lapply(kurt.2,function(x){as.numeric(quantile(x,0.025)<21.5073&quantile(x,0.975)>21.5073)})))
mean(unlist(lapply(thr.2,function(x){as.numeric(quantile(x,0.025)<0.265082&quantile(x,0.975)>0.265082)})))

# mean(unlist(lapply(mean.2,function(x){as.numeric(quantile(x,0.025)< -15&quantile(x,0.975)> -15)})))
# mean(unlist(lapply(sd.2,function(x){as.numeric(quantile(x,0.025)<43.3153&quantile(x,0.975)>43.3153)})))
# mean(unlist(lapply(skew.2,function(x){as.numeric(quantile(x,0.025)<1.31317&quantile(x,0.975)>1.31317)})))
# mean(unlist(lapply(kurt.2,function(x){as.numeric(quantile(x,0.025)<4.59746&quantile(x,0.975)>4.59746)})))
# mean(unlist(lapply(thr.2,function(x){as.numeric(quantile(x,0.025)<0.265082&quantile(x,0.975)>0.265082)})))
# mean(unlist(lapply(q5.2,function(x){as.numeric(quantile(x,0.025)<0.265082&quantile(x,0.975)>0.265082)})))

mean(unlist(lapply(q5.2,function(x){as.numeric(quantile(x,0.025)< -68.0424&quantile(x,0.975)>-68.0424)})))
mean(unlist(lapply(q25.2,function(x){as.numeric(quantile(x,0.025)< -51.2173&quantile(x,0.975)>-51.2173)})))
mean(unlist(lapply(q50.2,function(x){as.numeric(quantile(x,0.025)< -30.5073&quantile(x,0.975)>-30.5073)})))
mean(unlist(lapply(q75.2,function(x){as.numeric(quantile(x,0.025)<2.85935&quantile(x,0.975)>2.85935)})))
mean(unlist(lapply(q95.2,function(x){as.numeric(quantile(x,0.025)<89.5975&quantile(x,0.975)>89.5975)})))

tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/Dist2.tex",width=3.5,height=3.5)

#Fig 4A

#Specify trunc to work around extreme values that ruin the kernel density estimation
trunc<-150

dat<-as.numeric(unlist(outfixed2[outfixed2$sim==1,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
dat<-dat[which(abs(dat)<trunc)]
d<-density(dat)
keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
plot(seq(-trunc,trunc,1),keep.y,main="",col=rgb(217/255,95/255,2/255),xlim=c(-100,150),ylim=c(0,0.03), type="l", xlab="",ylab="")
k<-1

for(i in 2:100){
    dat<-as.numeric(unlist(outfixed2[outfixed2$sim==i,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
    dat<-dat[which(abs(dat)<trunc)]
    d<-density(dat)
    keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-rbind(keep.y, keep.y0/sum(keep.y0*1))
    lines(seq(-trunc,trunc,1),keep.y[i,],col=rgb(217/255,95/255,2/255))
    
}
lines(seq(-trunc,trunc,1), unlist(apply(keep.y,2,function(x){mean(x)})),col="black",lwd=2)

lines(seq(-trunc,trunc,1),sapply (seq(-trunc,trunc,1), f2),lwd=2,col="yellow")

dev.off()

#Case 3
f3<-function(x){0.6*dnorm(x+(1),mean=-30,sd=10)+0.4*dnorm(x+(1),mean=10,sd=5)}

tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/ExampleDist3.tex",width=3.5,height=3.5)

# dat1<-outfixed3[outfixed3$sim==5,-c(1:21,1022:1025)]
# plot((density(unlist(dat1[1,]))),xlim=c(-80,50),ylim=c(0,0.06),col="lightgray",main="",xlab="",ylab="")
# for (i in 2:length(dat1[,1])){
#     lines((density(unlist(dat1[i,]))),col="lightgray")
# }
# lines((density(unlist(dat1))),col=rgb(117/255,112/255,179/255),lwd=2)
# lines(seq(-100,150,1),sapply (seq(-100,150,1), f3),lwd=2,col="yellow")


dat1<-outfixed3[outfixed3$sim==1,-c(1:21,1022:1025)]
d<-density(unlist(dat1[1,]))
keep.y0<-approx(d$x, d$y, xout = seq(-80,50,4))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*4)
plot(seq(-80,50,4),keep.y,main="",col="lightgray",xlim=c(-80,50),ylim=c(0,0.06), type="l", xlab="",ylab="")
k<-1

for(i in 2:1000){
    d<-density(unlist(dat1[i,]))
    keep.y0<-approx(d$x, d$y, xout = seq(-80,50,4))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-keep.y0/sum(keep.y0*4)
    lines(seq(-80,50,4),keep.y,main="",col="lightgray")
}

d<-density(unlist(dat1))
keep.y0<-approx(d$x, d$y, xout = seq(-80,50,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
lines(seq(-80,50,1),keep.y,main="",col=rgb(117/255,112/255,179/255),lwd=2)

lines(seq(-80,50,1),sapply (seq(-80,50,1), f3),lwd=2,col="yellow")
dev.off()



mean.3<-list()
sd.3<-list()
skew.3<-list()
kurt.3<-list()
thr.3<-list()
srbi.3<-list()
q5.3<-list()
q25.3<-list()
q50.3<-list()
q75.3<-list()
q95.3<-list()

for(i in 1:100){
    dat<-(outfixed3[outfixed3$sim==i,c(3:17)])
    datR<-(outfixed3[outfixed3$sim==i,-c(1:21,1022:1025)])
    
    #Non-central moments equal to weighted sum of non-central moments of the Gaussian components
    mu4<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^4+6*x[c(6:10)]^2*x[c(11:15)]^2+3*x[c(11:15)]^4))}))
    mu3<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^3+3*x[c(6:10)]*x[c(11:15)]^2))}))
    mu2<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]^2+x[c(11:15)]^2))}))
    mu1<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*(x[c(6:10)]))}))
    sigma<-sqrt(mu2-mu1^2)
    #https://mathworld.wolfram.com/CentralMoment.html
    kurt.3[[i]]<-(-3*mu1^4+6*mu1^2*mu2-4*mu1*mu3+mu4)/(sigma^4)
    skew.3[[i]]<-(2*mu1^3-3*mu1*mu2+mu3)/(sigma^3)
    
    mean.3[[i]]<-mu1
    sd.3[[i]]<-sigma
    thr.3[[i]]<-unlist(apply(dat,1,function(x){sum(x[c(1:5)]*c(1-pnorm(0,mean=x[6],sd=x[11]),1-pnorm(0,mean=x[7],sd=x[12]),1-pnorm(0,mean=x[8],sd=x[13]), 1-pnorm(0,mean=x[9],sd=x[14]),1-pnorm(0,mean=x[10],sd=x[15])))}))
    srbi.3[[i]]<-(skew.3[[i]]^2+1)/kurt.3[[i]]
    
    q5.3[[i]]<-(apply(datR,1,function(x){quantile(x,0.05)}))
    q25.3[[i]]<-(apply(datR,1,function(x){quantile(x,0.25)}))
    q50.3[[i]]<-(apply(datR,1,function(x){quantile(x,0.50)}))
    q75.3[[i]]<-(apply(datR,1,function(x){quantile(x,0.75)}))
    q95.3[[i]]<-(apply(datR,1,function(x){quantile(x,0.95)}))
    
    
}

mean(unlist(lapply(mean.3,function(x){mean(x)})))
mean(unlist(lapply(sd.3,function(x){mean(x)})))
mean(unlist(lapply(skew.3,function(x){mean(x)})))
mean(unlist(lapply(kurt.3,function(x){mean(x)})))
mean(unlist(lapply(thr.3,function(x){mean(x)})))
mean(unlist(lapply(srbi.3,function(x){mean(x)})))
mean(unlist(lapply(q5.3,function(x){mean(x)})))
mean(unlist(lapply(q25.3,function(x){mean(x)})))
mean(unlist(lapply(q50.3,function(x){mean(x)})))
mean(unlist(lapply(q75.3,function(x){mean(x)})))
mean(unlist(lapply(q95.3,function(x){mean(x)})))


mean(unlist(lapply(mean.3,function(x){as.numeric(quantile(x,0.025)< -15&quantile(x,0.975)> -15)})))
mean(unlist(lapply(sd.3,function(x){as.numeric(quantile(x,0.025)<sqrt(454)&quantile(x,0.975)>sqrt(454))})))
mean(unlist(lapply(skew.3,function(x){as.numeric(quantile(x,0.025)<0.0942782&quantile(x,0.975)>0.0942782)})))
mean(unlist(lapply(kurt.3,function(x){as.numeric(quantile(x,0.025)<1.5404&quantile(x,0.975)>1.5404)})))
mean(unlist(lapply(thr.3,function(x){as.numeric(quantile(x,0.025)<0.386208&quantile(x,0.975)>0.386208)})))

mean(unlist(lapply(q5.3,function(x){as.numeric(quantile(x,0.025)< -44.8299&quantile(x,0.975)>-44.8299)})))
mean(unlist(lapply(q25.3,function(x){as.numeric(quantile(x,0.025)< -33.1043&quantile(x,0.975)>-33.1043)})))
mean(unlist(lapply(q50.3,function(x){as.numeric(quantile(x,0.025)< -21.3258&quantile(x,0.975)>-21.3258)})))
mean(unlist(lapply(q75.3,function(x){as.numeric(quantile(x,0.025)<7.40802&quantile(x,0.975)>7.40802)})))
mean(unlist(lapply(q95.3,function(x){as.numeric(quantile(x,0.025)<14.7518&quantile(x,0.975)>14.7518)})))

tikz("D:/Documents/PhD/Variability in causal effects/TikzFig/Dist3.tex",width=3.5,height=3.5)

#Fig 4A

#Specify trunc to work around extreme values that ruin the kernel density estimation
trunc<-150

dat<-as.numeric(unlist(outfixed3[outfixed3$sim==1,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
dat<-dat[which(abs(dat)<trunc)]
d<-density(dat)
keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
keep.y0[which(is.na(keep.y0))]<-0
keep.y<-keep.y0/sum(keep.y0*1)
plot(seq(-trunc,trunc,1),keep.y,main="",col=rgb(117/255,112/255,179/255),xlim=c(-80,50),ylim=c(0,0.06), type="l", xlab="",ylab="")
k<-1

for(i in 2:100){
    dat<-as.numeric(unlist(outfixed3[outfixed3$sim==i,-c(1:21,1022:1025)])) #[seq(1,1000,10),]
    dat<-dat[which(abs(dat)<trunc)]
    d<-density(dat)
    keep.y0<-approx(d$x, d$y, xout = seq(-trunc,trunc,1))$y
    keep.y0[which(is.na(keep.y0))]<-0
    keep.y<-rbind(keep.y, keep.y0/sum(keep.y0*1))
    lines(seq(-trunc,trunc,1),keep.y[i,],col=rgb(117/255,112/255,179/255))
    
}
lines(seq(-trunc,trunc,1), unlist(apply(keep.y,2,function(x){mean(x)})),col="black",lwd=2)

lines(seq(-trunc,trunc,1),sapply (seq(-trunc,trunc,1), f3),lwd=2,col="yellow")

dev.off()


