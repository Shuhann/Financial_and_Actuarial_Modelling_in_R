SA<-c(0.1,0.02,0.12,0.2,0.38)
SB<-c(0.35,0.05,0.2,0.25,0.45)
P<-c(0.1,0.2,0.4,0.2,0.1)
ERA<-P%*%SA
ERB<-P%*%SB
ER<-c(ERA,ERB)
ExcessMeanA<-SA-c(ERA)
ExcessMeanB<-SB-c(ERB)
SigmaA<-sqrt(P%*%(ExcessMeanA)^2)
SigmaB<-sqrt(P%*%(ExcessMeanB)^2)
Sigma<-c(SigmaA,SigmaB)
Sigma
Erp<-function(WA,ERA,ERB){
Ep<-WA*ERA+(1-WA)*ERB
return(Ep)}
Erp(0.50,ERA,ERB)
CovAB<-P%*%(ExcessMeanA*ExcessMeanB)
CovAB
Sigmap<-function(WA,SigmaA,SigmaB,CovAB){
sdp<-sqrt(WA^2*SigmaA^2+(1-WA)^2*SigmaB^2+2*WA*(1-WA)*CovAB)
return(sdp)
}
sigp<-Sigmap(0.5,SigmaA,SigmaB,CovAB)
sigp
WA<-seq(0,1.4,by=0.001)
plot(WA,Sigmap(WA,0.09838699,0.11445523,0.0096))
min(Sigmap(WA,0.09838699,0.11445523,0.0096))
WAmin<-match(min(Sigmap(WA,0.09838699,0.11445523,0.0096)),Sigmap(WA,0.09838699,0.11445523,0.0096))
WAmin
WA[WAmin]
Erp(WA[WAmin],ERA,ERB)


#example2
Y<-c(2005,2006,2007,2008,2009)
rA<-c(-0.1,0.15,0.25,0.05,0.2)
rB<-c(-0.05,0.15,0.2,-0.1,0.3)
Returns<-data.frame(Year=Y,ReturnA=rA,ReturnB=rB)
Returns
MeanA<-mean(rA)
MeanB<-mean(rB)
Mean<-c(MeanA,MeanB)
Mean
WA<-c(0.5,0.5,0.5,0.5,0.5)
RetP<-rA*WA+rB*WA
RetP
MeanP<-mean(RetP)
MeanP
Returns$Returnp=RetP#add a new column
Returns

SdA<-sd(rA)
SdB<-sd(rB)
Sdp<-sd(RetP)
StanDev<-c(SdA,SdB,Sdp)
StanDev

Retp<-rep(0,101)
rp<-rep(0,101)
Sdp<-rep(0,101)
i<-1#to set-0.2<WA<1.2
for(WA in -20:120){WA<-0.01*WA
rp[i]<-WA*MeanA+(1-WA)*MeanB
Sdp[i]<-sqrt((1/4)*((WA*(-0.1)+(1-WA)*(-0.05)-(WA*MeanA+(1-WA)*MeanB))^2+
                      (WA*(0.15)+(1-WA)*(0.15)-(WA*MeanA+(1-WA)*MeanB))^2+
                      (WA*(0.25)+(1-WA)*(0.20)-(WA*MeanA+(1-WA)*MeanB))^2+
                      (WA*(0.05)+(1-WA)*(-0.1)-(WA*MeanA+(1-WA)*MeanB))^2+
                      (WA*(0.20)+(1-WA)*(0.3)-(WA*MeanA+(1-WA)*MeanB))^2))
i<-i+1
}
plot(Sdp,rp,col='red',pch='.')


#example3
rA<-0.2#expected return of stock A
rB<-0.15
rC<-0.1
sigA<-0.36#standard deviation of A
sigB<-0.1225
sigC<-0.0625

sigAB<-0.084#covariance between assets A and B
sigAC<-0.105
sigBC<-0.07
rp<-0.16
wA<-seq(0,1,by=0.0001)
Stap<-function(wA,rA,rB,rC,rp,sigA,sigB,sigC,sigAB,sigAC,sigBC){
Ap<-sqrt(wA^2*sigA+((1/(rB-rC))*(rp-wA*rA-(1-wA)*rC))^2*sigB+
           (1-wA-(1/(rB-rC))*(rp-wA*rA-(1-wA)*rC))^2*sigC+
           2*wA*(1/(rB-rC))*(rp-wA*rA-(1-wA)*rC)*sigAB+
           2*wA*(1-wA-(1/(rB-rC))*(rp-wA*rA-(1-wA)*rC))*sigAC+
           2*(1/(rB-rC))*(rp-wA*rA-(1-wA)*rC)*(1-wA-(1/(rB-rC))*(rp-wA*rA-(1-wA)*rC))*sigBC)
return(Ap)
}
min(Stap(wA,0.2,0.15,0.1,0.16,0.36,0.1225,0.0625,0.084,0.105,0.07))
WAmin<-match(min(Stap(wA,0.2,0.15,0.1,0.16,0.36,0.1225,0.0625,0.084,0.105,0.07)),
             Stap(wA,0.2,0.15,0.1,0.16,0.36,0.1225,0.0625,0.084,0.105,0.07))
WAmin
Weig<-c(wA[WAmin],(1/(rB-rC))*(rp-wA[WAmin]*rA-(1-wA[WAmin])*rC),
        (1-wA[WAmin]-(1/(rB-rC))*(rp-wA[WAmin]*rA-(1-wA[WAmin])*rC)))
Weig
Epmin1<-(wA[WAmin]*rA+(1/(rB-rC))*(rp-wA[WAmin]*rA-(1-wA[WAmin])*rC)*rB+
           (1-wA[WAmin]-(1/(rB-rC))*(rp-wA[WAmin]*rA-(1-wA[WAmin])*rC))*rC)
Epmin1


#example4
Annui1<-function(r,PV,V,m){+return(r*(PV-V)*(1/(1-(1+r)^(-m))))
}
A1<-Annui1(r=0.02,PV=30000,V=3500,m=5)
A1

Annui1<-function(r,V,A,m){
return(V+(A/r)*(1-(1+r)^(-m)))
}
PVA1<-Annui1(r=0.05,V=3500,A=A1,m=5)
PVA1