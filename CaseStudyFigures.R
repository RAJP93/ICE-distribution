library(tikzDevice)

load("~/PhD/Variability in causal effects/CaseStudy/Data5Comp032022b.RData")

clr1<-rgb(27/255,158/255,119/255)
clr2<-rgb(217/255,95/255,2/255)
clr3<-rgb(117/255,112/255,179/255)
clr4<-rgb(231/255,41/255,138/255)
clr5<-rgb(102/255,166/255,30/255)
clr6<-rgb(230/255,171/255,2/255)
clr7<-rgb(166/255,118/255,29/255)

clr1b<-rgb(27/255,158/255,119/255,0.2)
clr2b<-rgb(217/255,95/255,2/255,0.2)
clr3b<-rgb(117/255,112/255,179/255,0.2)
clr4b<-rgb(231/255,41/255,138/255,0.2)
clr5b<-rgb(102/255,166/255,30/255,0.2)
clr6b<-rgb(230/255,171/255,2/255,0.2)
clr7b<-rgb(166/255,118/255,29/255,0.2)


clr4c<-rgb(231/255,41/255,138/255,0.5)
clr5c<-rgb(102/255,166/255,30/255,0.5)

clr6c<-rgb(230/255,171/255,2/255,0.5)
clr6d<-rgb(230/255,171/255,2/255,0.7)

blackc<-rgb(0,0,0,0.5)

#Traceplots
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/traceplot.tex",width = 8, height = 4)
#tikz(file = "D:/Documents/PhD/Infering_idividual_causal/TikZFigures/Fig9LN.tex", width = 7, height = 6)

par(mfrow=c(1,2))
# plot(data4[which(data4$sim==1),2],col=clr1b,type="l",ylim=c(-7.5,7.5),ylab="Z1",xlab="ICE")
# lines(data4[which(data4$sim==2),2],col=clr2b)
# lines(data4[which(data4$sim==5),2],col=clr3b)
# lines(data4[which(data4$sim==3),2],col=clr4b)

plot(data4[which(data4$chain==1),1],col=clr1b,type="l",ylim=c(-7.5,7.5),ylab="Z1",xlab="ICE")
lines(data4[which(data4$chain==2),1],col=clr2b)
lines(data4[which(data4$chain==4),1],col=clr3b)
lines(data4[which(data4$chain==3),1],col=clr4b)

legend(500, -4.5,c(1,2,3,4),col=c(clr1b,clr2b,clr3b,clr4b),ncol=4,title="chain",lwd=2)

# plot(data7[which(data7$sim==1),2],col=clr1b,type="l",ylim=c(-7.5,7.5),ylab="Z1",xlab="ICE")
# lines(data7[which(data7$sim==2),2],col=clr2b)
# lines(data7[which(data7$sim==5),2],col=clr3b)
# lines(data7[which(data7$sim==3),2],col=clr4b)

plot(data7[which(data7$chain==1),1],col=clr1b,type="l",ylim=c(-7.5,7.5),ylab="Z1",xlab="ICE")
lines(data7[which(data7$chain==2),1],col=clr2b)
lines(data7[which(data7$chain==4),1],col=clr3b)
lines(data7[which(data7$chain==3),1],col=clr4b)
legend(500, -4.5,c(1,2,3,4),col=c(clr1b,clr2b,clr3b,clr4b),ncol=4,title="chain",lwd=2)
dev.off()

#ICE distribution
#png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/ice.png",width = 6.5, height = 6, units = 'in', res = 600)
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/iceNOC.tex",width = 4, height = 4)
par(mfrow=c(1,1))
#plot(ti,yICE,col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
#plot(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
plot(ti,colMeans(ICENEW2),col=clr6,lwd=2,type="l",ylim=c(0,3),ylab="pdf", xlab="ICE",ann=F)

polygon(c(rev(ti), ti), c(rev(apply(ICENEW2,2,function(x){quantile(x,0.975)})), apply(ICENEW2,2,function(x){quantile(x,0.025)})), col = clr6b, border = NA)
#lines(ti,colMeans(ICE),lwd=2)
#lines(ti,apply(ICE,2,function(x){quantile(x,0.5)}),lty=2)
#lines(ti,yICE,col="darkgreen",lwd=2,type="l")
lines(ti,colMeans(ICENEW2),col=clr6,lwd=2)
dev.off()

#ICE comparison
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/icecomparisonNOC.tex",width = 4, height = 4)
par(mfrow=c(1,1))
# plot(ti,colMeans(ICE),col=clr6c,lwd=2,type="l",ylim=c(0,3),ylab="pdf",xlab="ICE",ann=F)
# polygon(c(rev(ti), ti), c(rev(apply(ICE2,2,function(x){quantile(x,0.975)})), apply(ICE2,2,function(x){quantile(x,0.025)})), col = clr6b, border = NA)
# lines(ti2,colMeans(ICENFR),col=clr6,lwd=2,type="l",ylim=c(0,3),ylab="ICE",lty=2)
# lines(ti,sapply(ti,function(x){dnorm(x,0.4157, sqrt(0.6232))}),lwd=2,lty=4,col=clr6)
# lines(ti2,colMeans(ICENC),col=clr6,lwd=2,type="l",ylim=c(0,3),ylab="ICE",lty=3)
plot(ti,colMeans(ICENEW2),col="white",lwd=2,type="l",ylim=c(0,3),ylab="pdf",xlab="ICE",ann=F)
polygon(c(rev(ti), ti), c(rev(apply(ICENEW2,2,function(x){quantile(x,0.975)})), apply(ICENEW2,2,function(x){quantile(x,0.025)})), col = clr6b, border = NA)
lines(ti,colMeans(ICENOFRNEW2),col=clr6,lwd=2,type="l",ylim=c(0,3),ylab="ICE",lty=2)
lines(ti,sapply(ti,function(x){dnorm(x,0.4157, sqrt(0.6232))}),lwd=2,lty=4,col=clr6)
lines(ti,colMeans(ICE2),col=clr6,lwd=2,type="l",ylim=c(0,3),ylab="ICE",lty=3)
lines(ti,colMeans(ICENEW2),col=clr6c,lwd=2,type="l",ylim=c(0,3),ylab="pdf",xlab="ICE",ann=F)
legend(0.6, 2.5,c("model","confounding heterogeneity","gaussian residual","gaussian LMM"),col=c(clr6d,clr6,clr6,clr6),lty=c(1,3,2,4),lwd=2,cex=0.65,seg.len=2.5)
#legend(0.5, 2.5,c("model","gaussian residual","confounding homogeneity","gaussian LMM"),col=clr6,lty=c(1,2,3,4),lwd=2,cex=0.65,seg.len=2.5)
dev.off()


#Potential outcome densities
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/potential.tex",width = 4, height = 4)
par(mfrow=c(1,1))
plot(t,yY0,col=clr4,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(Y0,2,function(x){quantile(x,0.975)})), apply(Y0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yY1,col=clr5,lwd=2,type="l",ylim=c(0,0.35))

polygon(c(rev(t), t), c(rev(apply(Y1,2,function(x){quantile(x,0.975)})), apply(Y1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yY1,col=clr5,lwd=2,type="l",ylim=c(0,0.35))
lines(t,yY0,col=clr4,lwd=2,type="l",ylim=c(0,0.35))
legend(10, 0.25,c(expression("Y"^0),expression("Y"^1)),col=c(clr4,clr5),lwd=2)

dev.off()


#Exposure strata
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/fattyliver.tex",width = 8, height = 4)

par(mfrow=c(1,2))

plot(t,yYA0,col=clr4c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA0,2,function(x){quantile(x,0.975)})), apply(YA0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYA0T,col=blackc,lwd=2,type="l")
lines(t,yYA0,col=clr4c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr4c,blackc),lwd=2,cex=1)

plot(t,yYA1,col=clr5c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA1,2,function(x){quantile(x,0.975)})), apply(YA1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYA1T,col=blackc,lwd=2,type="l")
lines(t,yYA1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2,cex=1)
dev.off()

#Confounder strata

#Age
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/age.tex",width = 8, height = 4)
par(mfrow=c(1,2))
#plot(t,yYAGE0,col=clr4,lwd=2,type="l")??

plot(t,colMeans(YAGE0),col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YAGE0,2,function(x){quantile(x,0.975)})), apply(YAGE0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYAGE0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYAGE0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYAGE1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YAGE1,2,function(x){quantile(x,0.975)})), apply(YAGE1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYAGE1T,col=blackc,lwd=2,type="l")
lines(t,yYAGE1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5,blackc),lty=c(1,1),lwd=2)
dev.off()

#Sex
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/sex.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYSEX0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSEX0,2,function(x){quantile(x,0.975)})), apply(YSEX0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYSEX0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSEX0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYSEX1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSEX1,2,function(x){quantile(x,0.975)})), apply(YSEX1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYSEX1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSEX1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5,blackc),lty=c(1,1),lwd=2)
dev.off()

#Diab
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/diab.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYDIAB0,col=clr4c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YDIAB0,2,function(x){quantile(x,0.975)})), apply(YDIAB0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYdiab0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYDIAB0,col=clr4c,lwd=2,type="l")
legend(9, 0.275,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYDIAB1,col=clr5c,lwd=2,type="l",ylim=c(0,0.33),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YDIAB1,2,function(x){quantile(x,0.975)})), apply(YDIAB1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYdiab1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYDIAB1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()

#SBP
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/sbp.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYSBP0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSBP0,2,function(x){quantile(x,0.975)})), apply(YSBP0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYSBPb0T,col=blackc,lty=1,lwd=2,type="l")
lines(t,yYSBP0,col=clr4c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYSBP1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSBP1,2,function(x){quantile(x,0.975)})), apply(YSBP1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYSBPb1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSBP1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()

#HRX
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/hrx.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYHRX0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YHRX0,2,function(x){quantile(x,0.975)})), apply(YHRX0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYhrx0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYHRX0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYHRX1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YHRX1,2,function(x){quantile(x,0.975)})), apply(YHRX1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
#!
lines(t,yYhrx1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYHRX1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()


##########
###NOFR###
##########

load("~/PhD/Variability in causal effects/CaseStudy/DataNOFR.RData")

tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/fattyliverNFR.tex",width = 8, height = 4)

par(mfrow=c(1,2))

plot(t,yYA0,col=clr4c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA0,2,function(x){quantile(x,0.975)})), apply(YA0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYA0T,col=blackc,lwd=2,type="l")
lines(t,yYA0,col=clr4c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr4c,blackc),lwd=2,cex=1)

plot(t,yYA1,col=clr5c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA1,2,function(x){quantile(x,0.975)})), apply(YA1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYA1T,col=blackc,lwd=2,type="l")
lines(t,yYA1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2,cex=1)
dev.off()

##########
###NOC###
##########

load("~/PhD/Variability in causal effects/CaseStudy/DataNOC.RData")

tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/fattyliverNOC.tex",width = 8, height = 4)

par(mfrow=c(1,2))

plot(t,yYA0,col=clr4c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA0,2,function(x){quantile(x,0.975)})), apply(YA0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYA0T,col=blackc,lwd=2,type="l")
lines(t,yYA0,col=clr4c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr4c,blackc),lwd=2,cex=1)

plot(t,yYA1,col=clr5c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YA1,2,function(x){quantile(x,0.975)})), apply(YA1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYA1T,col=blackc,lwd=2,type="l")
lines(t,yYA1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2,cex=1)
dev.off()


#Age
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/ageNOC.tex",width = 8, height = 4)
par(mfrow=c(1,2))
#plot(t,yYAGE0,col=clr4,lwd=2,type="l")??

plot(t,colMeans(YAGE0),col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YAGE0,2,function(x){quantile(x,0.975)})), apply(YAGE0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYAGE0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYAGE0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYAGE1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YAGE1,2,function(x){quantile(x,0.975)})), apply(YAGE1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYAGE1T,col=blackc,lwd=2,type="l")
lines(t,yYAGE1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5,blackc),lty=c(1,1),lwd=2)
dev.off()

#Sex
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/sexNOC.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYSEX0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSEX0,2,function(x){quantile(x,0.975)})), apply(YSEX0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYSEX0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSEX0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYSEX1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSEX1,2,function(x){quantile(x,0.975)})), apply(YSEX1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYSEX1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSEX1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5,blackc),lty=c(1,1),lwd=2)
dev.off()

#Diab
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/diabNOC.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYDIAB0,col=clr4c,lwd=2,type="l",ylim=c(0,0.35),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YDIAB0,2,function(x){quantile(x,0.975)})), apply(YDIAB0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYdiab0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYDIAB0,col=clr4c,lwd=2,type="l")
legend(9, 0.275,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYDIAB1,col=clr5c,lwd=2,type="l",ylim=c(0,0.33),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YDIAB1,2,function(x){quantile(x,0.975)})), apply(YDIAB1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYdiab1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYDIAB1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()

#SBP
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/sbpNOC.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYSBP0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSBP0,2,function(x){quantile(x,0.975)})), apply(YSBP0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYSBPb0T,col=blackc,lty=1,lwd=2,type="l")
lines(t,yYSBP0,col=clr4c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYSBP1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YSBP1,2,function(x){quantile(x,0.975)})), apply(YSBP1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
lines(t,yYSBPb1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYSBP1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()

#HRX
tikz(file="D:/Documents/PhD/Variability in causal effects/TikzFig/hrxNOC.tex",width = 8, height = 4)
par(mfrow=c(1,2))
plot(t,yYHRX0,col=clr4c,lwd=2,type="l",ylim=c(0,0.4),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YHRX0,2,function(x){quantile(x,0.975)})), apply(YHRX0,2,function(x){quantile(x,0.025)})), col = clr4b, border = NA)
lines(t,yYhrx0T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYHRX0,col=clr4c,lwd=2,type="l")
legend(9, 0.3,c("model","observed"),col=c(clr4c,blackc),lty=c(1,1),lwd=2)

plot(t,yYHRX1,col=clr5c,lwd=2,type="l",ylim=c(0,0.325),ylab="pdf",xlab="Y", ann=F)
polygon(c(rev(t), t), c(rev(apply(YHRX1,2,function(x){quantile(x,0.975)})), apply(YHRX1,2,function(x){quantile(x,0.025)})), col = clr5b, border = NA)
#!
lines(t,yYhrx1T,col=blackc,lwd=2,type="l",lty=1)
lines(t,yYHRX1,col=clr5c,lwd=2,type="l")
legend(9, 0.25,c("model","observed"),col=c(clr5c,blackc),lty=c(1,1),lwd=2)
dev.off()

