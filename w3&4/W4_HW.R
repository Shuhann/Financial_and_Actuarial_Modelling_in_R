#example1
P<-c(0.1,0.2,0.4,0.2,0.1)
Rm<-c(0.07,0.09,0.11,0.13,0.15)
ERm<-P%*%Rm
ERm
rf=0.06
ERm-rf
x<-seq(-1,1,by=0.001)
y<-0.05*x+0.06
plot(x,y)

Sp<-c(160,120,80,80,60)
LenghS<-length(Sp)
SumS<-function(LenghS){
Sum<-0
for(i in 1:length(Sp)){
Sum<-Sum+Sp[i]}
return(Sum)}
SumS(LenghS)
WeightS<-c(160,120,80,80,60)*c(1/SumS(LenghS))
WeightS#wefirstfindfundbeta
BetaS<-c(0.5,2,4,1,3)
BetaF<-BetaS%*%WeightS
BetaF
RFp<-BetaF*(ERm-rf)+rf
RFp
RS6<-2*(ERm-rf)+rf
RS6

#example2
ERA<-0.2
ERB<-0.12
sigmaA<-0.3
sigmaB<-0.15
rhoAB<-0.1
CovAB<-sigmaA*sigmaB*rhoAB
CovAB
Erp<-function(WA,ERA,ERB){
Ep<-WA*ERA+(1-WA)*ERB
return(Ep)}
sigmap<-function(WA,sigmaA,sigmaB,covAB){
sdp<-sqrt(WA^2*sigmaA^2+(1-WA)^2*sigmaB^2+2*WA*(1-WA)*covAB)
return(sdp)}
WA<-seq(-0.2,1.2,by=0.0001)
plot(WA,sigmap(WA,sigmaA,sigmaB,CovAB))
min(sigmap(WA,sigmaA,sigmaB,CovAB))
WAmin<-match(min(sigmap(WA,sigmaA,sigmaB,CovAB)),sigmap(WA,sigmaA,sigmaB,CovAB))#position
cat('The minimal variance weight is',WA[WAmin],'\n')
Erpmin<-Erp(WA[WAmin],ERA,ERB)
Erpmin

WeightA<-seq(0,1,by=0.2)
WeightB<-1-WeightA
sigp1<-sigmap(WA=WeightA,sigmaA,sigmaB,CovAB)
Erp1<-Erp(WA=WeightA,ERA,ERB)
Dataop<-data.frame(list(WeightA=WeightA,WeightB=WeightB,sigmaP=sigp1,ExpectedP=Erp1))
Dataop
plot(Dataop$sigmaP,Dataop$ExpectedP)
WA<-seq(0,1,by=0.001)
rf<-0.08
WAm<-match(max((Erp(WA,ERA,ERB)-rf)/(sigmap(WA,sigmaA,sigmaB,CovAB))),(Erp(WA,ERA,ERB)-rf)/(sigmap(WA,sigmaA,sigmaB,CovAB)))
WAm#position
WAmax<-WA[WAm]#value of weight to make Sharpe ratio maxmize
WBmax<-1-WAmax
WBmax
sigop<-sigmap(WAmax,sigmaA,sigmaB,CovAB)
sigop
Erpop<-Erp(WAmax,ERA,ERB)
Erpop
Smax<-max((Erp(WA,ERA,ERB)-rf)/(sigmap(WA,sigmaA,sigmaB,CovAB)))
Smax#maximal sharpe ratio
x<-seq(0,1,by=0.001)
y<-Smax*x+rf
lines(x,y)
Rc<-0.14
sigC<-(Rc-rf)/Smax
sigC
WeigpC<-(Rc-rf)/(Erpop-rf)
WeigpC
Weigf<-1-WeigpC
Weigf
WAc<-WeigpC*WAmax
WAc
WBc<-WeigpC*(1-WAmax)
WBc


#example3
#Q1
rA<-0.000427#expected return of stock A
rB<-0.000015
rC<-0.000285
sig2A<-0.01#standard deviation of A
sig2B<-0.0109
sig2C<-0.0199
sigmaAB<-0.0018 #covariance between assets A and B
sigmaAC<-0.0011
sigmaBC<-0.0026
rp<-function(wA,wB){
  return(wA*rA+wB*rB+(1-wA-wB)*rC)
}
rp(1/3,1/3)
varp<-function(wA,wB){
  return(wA^2*sig2A+wB^2*sig2B+(1-wA-wB)^2*sig2C+2*wA*wB*sigmaAB+2*wA*(1-wA-wB)*sigmaAC+2*wB*(1-wA-wB)*sigmaBC)
}
sqrt(varp(1/3,1/3))

#Q2
wA<-seq(0,1,by=0.001)
WBn<-(1/(rB-rC))*(rA-rC-wA*(rA-rC))
varp(wA,WBn)
min(varp(wA,WBn)) 
Wamin<-match(min(varp(wA,WBn)),varp(wA,WBn))
wA[Wamin]
wBmin<-(1/(rB-rC))*(rA-rC-wA[Wamin]*(rA-rC))
wBmin
wCmin<-1-wA[Wamin]-wBmin
wCmin


#Q3
rp<-rep(0,101*101)
varp<-rep(0,101*101)
i<-1 #to set 0.1<wA,wB<1.1
for(wA in -25:75){
  wA<-0.02*wA
  for(wB in -25:75){
    wB<-0.02*wB #sequence of mean and variance of the portfolio 
    rp[i]<-wA*rA+wB*rB+(1-wA-wB)*rC
    varp[i]<-wA^2*sig2A+wB^2*sig2B+(1-wA-wB)^2*sig2C+2*wA*wB*sigmaAB+2*wA*(1-wA-wB)*sigmaAC+2*wB*(1-wA-wB)*sigmaBC
    i<-i+1
  }
}
sigmap<-sqrt(varp)
plot(sigmap,rp,pch='.')

#Q4
rf<-0.0002
amax=max((rp-rf)/sigmap)#the negative tangency of the half line 
x<-seq(0,3,by=0.01)
lines(x,amax*x+rf)