#Traceplots
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/traceplot.png",width = 12.5, height = 6, units = 'in', res = 600)
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
dev.off()

#ICE distribution
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/ice.png",width = 6.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,1))
#plot(ti,yICE,col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
plot(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")

polygon(c(rev(ti), ti), c(rev(apply(ICE[-c(5001:10000),],2,function(x){quantile(x,0.975)})), apply(ICE[-c(5001:10000),],2,function(x){quantile(x,0.025)})), col = 'lightgreen', border = NA)
#lines(ti,colMeans(ICE),lwd=2)
#lines(ti,apply(ICE,2,function(x){quantile(x,0.5)}),lty=2)
#lines(ti,yICE,col="darkgreen",lwd=2,type="l")
lines(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2)
dev.off()

#ICE comparison
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/icecomparisonCI.png",width = 6.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,1))
#plot(ti,yICE,col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
plot(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
polygon(c(rev(ti), ti), c(rev(apply(ICE[-c(5001:10000),],2,function(x){quantile(x,0.975)})), apply(ICE[-c(5001:10000),],2,function(x){quantile(x,0.025)})), col = 'lightgreen', border = NA)
lines(ti,colMeans(ICENFR),col="chartreuse4",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
lines(ti,colMeans(ICENC),col="darkolivegreen4",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
lines(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2)
lines(ti,sapply(ti,function(x){dnorm(x,0.4157, sqrt(0.6232))}),lwd=2,col="grey")
legend(0.5, 2.5,c("model","gaussian residual","confounding homogeneity","gaussian LMM"),col=c("darkgreen","chartreuse4","darkolivegreen4","grey"),lwd=2)
dev.off()

png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/icecomparison.png",width = 6.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,1))
plot(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2,type="l",ylim=c(0,2.25),ylab="ICE")
lines(ti,colMeans(ICENFR),col="chartreuse4",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
lines(ti,colMeans(ICENC),col="darkolivegreen4",lwd=2,type="l",ylim=c(0,3),ylab="ICE")
lines(ti,colMeans(ICE[-c(5001:10000),]),col="darkgreen",lwd=2)
lines(ti,sapply(ti,function(x){dnorm(x,0.4157, sqrt(0.6232))}),lwd=2,col="grey")
legend(0.4, 2.0,c("model","gaussian residual","confounding homogeneity","gaussian LMM"),col=c("darkgreen","chartreuse4","darkolivegreen4","grey"),lwd=2)
dev.off()

#Potential outcome densities
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/potential.png",width = 6.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,1))
plot(t,yY0,col="indianred4",lwd=2,type="l",ylim=c(0,0.35),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(Y0,2,function(x){quantile(x,0.975)})), apply(Y0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yY1,col="cyan4",lwd=2,type="l",ylim=c(0,0.35))

polygon(c(rev(t), t), c(rev(apply(Y1,2,function(x){quantile(x,0.975)})), apply(Y1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yY1,col="cyan4",lwd=2,type="l",ylim=c(0,0.35))
lines(t,yY0,col="indianred4",lwd=2,type="l",ylim=c(0,0.35))
legend(10, 0.25,c(expression("Y"^0),expression("Y"^1)),col=c("indianred4","cyan4"),lwd=2)

dev.off()


#Exposure strata
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/fattyliver.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))

plot(t,yYA0,col="indianred4",lwd=2,type="l",ylim=c(0,0.35),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YA0,2,function(x){quantile(x,0.975)})), apply(YA0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYA0,col="indianred4",lwd=2,type="l")
lines(t,yYA0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYA1,col="cyan4",lwd=2,type="l",ylim=c(0,0.35),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YA1,2,function(x){quantile(x,0.975)})), apply(YA1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYA1,col="cyan4",lwd=2,type="l")
lines(t,yYA1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()

#Confounder strata
par(mfrow=c(1,2))

#Age
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/age.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))
plot(t,yYAGE0,col="indianred4",lwd=2,type="l",ylim=c(0,0.4),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YAGE0,2,function(x){quantile(x,0.975)})), apply(YAGE0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYAGE0,col="indianred4",lwd=2,type="l")
lines(t,yYAGE0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYAGE1,col="cyan4",lwd=2,type="l",ylim=c(0,0.325),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YAGE1,2,function(x){quantile(x,0.975)})), apply(YAGE1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYAGE1,col="cyan4",lwd=2,type="l")
lines(t,yYAGE1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()

#Sex
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/sex.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))
plot(t,yYSEX0,col="indianred4",lwd=2,type="l",ylim=c(0,0.4),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YSEX0,2,function(x){quantile(x,0.975)})), apply(YSEX0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYSEX0,col="indianred4",lwd=2,type="l")
lines(t,yYSEX0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYSEX1,col="cyan4",lwd=2,type="l",ylim=c(0,0.325),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YSEX1,2,function(x){quantile(x,0.975)})), apply(YSEX1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYSEX1,col="cyan4",lwd=2,type="l")
lines(t,yYSEX1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()

#Diab
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/diab.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))
plot(t,yYDIAB0,col="indianred4",lwd=2,type="l",ylim=c(0,0.35),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YDIAB0,2,function(x){quantile(x,0.975)})), apply(YDIAB0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYDIAB0,col="indianred4",lwd=2,type="l")
lines(t,yYDIAB0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYDIAB1,col="cyan4",lwd=2,type="l",ylim=c(0,0.33),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YDIAB1,2,function(x){quantile(x,0.975)})), apply(YDIAB1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYDIAB1,col="cyan4",lwd=2,type="l")
lines(t,yYDIAB1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()

#SBP
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/sbp.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))
plot(t,yYSBP0,col="indianred4",lwd=2,type="l",ylim=c(0,0.4),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YSBP0,2,function(x){quantile(x,0.975)})), apply(YSBP0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYSBP0,col="indianred4",lwd=2,type="l")
lines(t,yYSBP0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYSBP1,col="cyan4",lwd=2,type="l",ylim=c(0,0.325),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YSBP1,2,function(x){quantile(x,0.975)})), apply(YSBP1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYSBP1,col="cyan4",lwd=2,type="l")
lines(t,yYSBP1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()

#HRX
png(file="D:/Documents/PhD/Variability in causal effects/CaseStudy/alison/alison/figures/hrx.png",width = 12.5, height = 6, units = 'in', res = 600)
par(mfrow=c(1,2))

plot(t,yYHRX0,col="indianred4",lwd=2,type="l",ylim=c(0,0.4),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YHRX0,2,function(x){quantile(x,0.975)})), apply(YHRX0,2,function(x){quantile(x,0.025)})), col = 'indianred1', border = NA)
lines(t,yYHRX0,col="indianred4",lwd=2,type="l")
lines(t,yYHRX0T,col="red",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("indianred4","red"),lwd=2)

plot(t,yYHRX1,col="cyan4",lwd=2,type="l",ylim=c(0,0.325),ylab="Y")
polygon(c(rev(t), t), c(rev(apply(YHRX1,2,function(x){quantile(x,0.975)})), apply(YHRX1,2,function(x){quantile(x,0.025)})), col = 'cyan1', border = NA)
lines(t,yYHRX1,col="cyan4",lwd=2,type="l")
lines(t,yYHRX1T,col="blue",lwd=2,type="l")
legend(10, 0.25,c("model","observed"),col=c("cyan4","blue"),lwd=2)
dev.off()
