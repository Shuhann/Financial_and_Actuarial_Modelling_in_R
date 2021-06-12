DataFin<-read.csv(file.choose(),header=TRUE)#save excel as csv first
DataFin
summary(DataFin)
mean(DataFin$GBPEUR)
sd(DataFin$GBPEUR)
mean(DataFin$CL)
sd(DataFin$CL)
mean(DataFin$CAC40)
sd(DataFin$CAC40)
mean(DataFin$GC)
sd(DataFin$GC)
cov(DataFin$GBPEUR, DataFin$CL)#covariance between GBPEUR and CL
cor(DataFin$GBPEUR, DataFin$CL)#correlation between GBPEUR and CL
cor(DataFin$GBPEUR, DataFin$CAC40)
cor(DataFin$GBPEUR, DataFin$GC)
cor(DataFin$CL, DataFin$GC)
cor(DataFin$CL, DataFin$CAC40)
cor(DataFin$GC, DataFin$CAC40)
cov(DataFin$GC, DataFin$CAC40)

plot(GBPEUR~GC,data=DataFin)#scatter plot
model1<-lm(GBPEUR~GC,data=DataFin)#regression analysis
abline(model1)#depicts the regression line
summary(model1)#summary of the analysis
coefficients(model1)


model2<-lm(GBPEUR~GC+CL+CAC40,data=DataFin)
#regression analysis on GBPEUR with independent varaibles GC,CL and CAC40
summary(model2)#summary of the analysis

#chapter2
library(FinancialMath)
FV1<-TVM(pv=2,fv=NA,n=4,i=0.05,ic=1,plot=TRUE)
FV1
#Exactly one of pv,fv,n,or i=r must be NA(unknown)
#ic=m is the interest conversion frequency per period
FV2<-TVM(pv=2,fv=NA,n=4,i=0.05,ic=8,plot=FALSE)
FV2

#Example2.0.2
Annui1<-function(r,m,A){
  return((A/r)*(1-(1+r)^(-m)))
  }
PVA1<-Annui1(r=0.05,m=6,A=100)
PVA1
Annui2<-function(r,m,A){
  return((A/r)*(1-(1+r)^(-m))*(1+r)^(m))
  }
PVA2<-Annui2(r=0.05,m=6,A=100)
PVA2

#example2.0.3
Annui1<-pmt(amt=100,maturity=12,rate=0.05)
Annui1
#Alternatively
LoanPay1<-function(r,n,PVA){
  return(r*PVA*(1/(1-1/((1+r)^(n)))))
  }
A<-LoanPay1(r=0.05,n=12,PVA=100)
A

rate<-rate(amt=100,maturity=10,pmt=20.0)
rate
 
#example2.1.1
#Consider two stocks A and B with the following characteristic 
#rA=10.5%,rB=18%,sA=15%,sB=12%
#We also assume three different possibilities for the covariance between A and B
#sdAB=-0.012;0;0.015
wA<-seq(-0.3, 1.3, by=0.05)#sequence from -0.5 to 1.5 by increments of 0.05
rA<-0.105#expected return of stock A
rB<-0.18
sigmaA<-0.15#standard deviation of A
sigmaB<-0.12
sigmaAB1<--0.012#covariance between of stocks A and B
sigmaAB2<-0
sigmaAB3<-0.015
rp<-wA*rA+(1-wA)*rB#mean return of the portfolio
sigmap1<-sqrt((wA^2)*sigmaA^2+((1-wA)^2)*sigmaB^2+2*wA*(1-wA)*sigmaAB1)
sigmap2<-sqrt((wA^2)*sigmaA^2+((1-wA)^2)*sigmaB^2+2*wA*(1-wA)*sigmaAB2)
sigmap3<-sqrt((wA^2)*sigmaA^2+((1-wA)^2)*sigmaB^2+2*wA*(1-wA)*sigmaAB3)
plot(sigmap1,rp)#opportunity set of two assets
points(sigmap2,rp,pch=2)
points(sigmap3,rp,pch=3)
legend(0.05,0.10,legend=c("cov<0","cov=0","cov>0"),pch=c(1,2,3))


plot(wA,sigmap1,xlab="wA",ylab="sigmap")
points(wA,sigmap2,pch=2)
points(wA,sigmap3,pch=3)

rA<-0.105
rB<-0.18
rC<-0.02
sigmaA<-0.15
sigmaB<-0.12
sigmaC<-0.2
sigmaAB<--0.012#covariance between assets A and B
sigmaAC<-0.002 
sigmaBC<--0.002
rp<-rep(0,101*101)
varp<-rep(0,101*101)
i<-1#to set -0.3<wA,wB<1.3
for(wA in -15:65){wA<-0.02*wA
  for(wB in -15:65){wB<-0.02*wB
  #sequence of mean and variance of the portfolio
  rp[i]<-wA*rA+wB*rB+(1-wA-wB)*rC
  varp[i]<-wA^2*sigmaA^2+wB^2*sigmaB^2+(1-wA-wB)^2*sigmaC^2+2*wA*wB*sigmaAB+2*wA*(1-wA-wB)*sigmaAC+2*wB*(1-wA-wB)*sigmaBC
  i<-i+1
  }}
sigmap<-sqrt(varp)
plot(sigmap,rp,pch=".")#opportunity set of three assets

#week4
rf<-0.05#Expected return of safe asset
amax=max((rp-rf)/sigmap)#the negative tangency of the half-line
x<-seq(0,3,by=0.01)
plot(sigmap,rp,pch=".")
lines(x,amax*x+rf)#CML

#example2.2.1
DataCAPM1<-read.csv(file.choose(),header=TRUE)
DataCAPM1
summary(DataCAPM1)
str(DataCAPM1)#structure of the objects

ExReturn<-read.csv(file.choose(),header=TRUE)
ExReturn
summary(ExReturn)
str(ExReturn)
rf=mean(DataCAPM1$risk.free)
rf
Mean=sapply(ExReturn,mean)
SD=sapply(ExReturn,sd)#Sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
cbind(Mean,SD)

plot(ERA1~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA1',col='blue')#scatter plot
lmA1<-lm(ERA1~ERMakt,data=ExReturn)#build a model between ERMarket and ERA1
abline(lmA1)#draw a line
summary(lmA1)
BetaA1<-summary(lmA1)$coefficients[2, 1]
BetaA1
summary(lmA1)$coefficients[2, 1]#in case one wishes to obtain directly the beta
AlphaA1<-summary(lmA1)$coefficients[1, 1]
AlphaA1

plot(ERA2~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA2',col='blue')#scatterplot
lmA2<-lm(ERA2~ERMakt,data=ExReturn)
abline(lmA2)
BetaA2<-summary(lmA2)$coefficients[2, 1]
AlphaA2<-summary(lmA2)$coefficients[1, 1]
cbind(BetaA2,AlphaA2)

plot(ERA3~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA3',col='blue')#scatterplot
lmA3<-lm(ERA3~ERMakt,data=ExReturn)
abline(lmA3)
BetaA3<-summary(lmA3)$coefficients[2, 1]
AlphaA3<-summary(lmA3)$coefficients[1, 1]
cbind(BetaA3,AlphaA3)

plot(ERA4~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA4',col='blue')#scatterplot
lmA4<-lm(ERA4~ERMakt,data=ExReturn)
abline(lmA4)
BetaA4<-summary(lmA4)$coefficients[2, 1]
AlphaA4<-summary(lmA4)$coefficients[1, 1]
cbind(BetaA4,AlphaA4)

plot(ERA5~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA5',col='blue')#scatterplot
lmA5<-lm(ERA5~ERMakt,data=ExReturn)
abline(lmA5)
BetaA5<-summary(lmA5)$coefficients[2, 1]
AlphaA5<-summary(lmA5)$coefficients[1, 1]
cbind(BetaA5,AlphaA5)

plot(ERA6~ERMakt,data=ExReturn,xlab='ERMakt',ylab='ER',main='BetaA6',col='blue')#scatterplot
lmA6<-lm(ERA6~ERMakt,data=ExReturn)
abline(lmA6)
BetaA6<-summary(lmA6)$coefficients[2, 1]
AlphaA6<-summary(lmA6)$coefficients[1, 1]
cbind(BetaA6,AlphaA6)

CovM<-cov(ExReturn[,1:6])
CovM

EpRm=mean(ExReturn$ERMakt)
rf=mean(DataCAPM1$risk.free)
ErA1=rf+BetaA1*(EpRm-rf)#SML
ErA2=rf+BetaA2*(EpRm-rf)
ErA3=rf+BetaA3*(EpRm-rf)
ErA4=rf+BetaA4*(EpRm-rf)
ErA5=rf+BetaA5*(EpRm-rf)
ErA6=rf+BetaA6*(EpRm-rf)
Er<-c(ErA1,ErA2,ErA3,ErA4,ErA5,ErA6)
Er