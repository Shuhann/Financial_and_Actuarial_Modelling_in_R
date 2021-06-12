SA<-c(0.15,0.2,0.05,0.1)
SB<-c(0.1,0.4,0.3,-0.1)
SC<-c(0.25,0.15,0.05,0.1)
SD<-c(0.25,0.5,0.2,0.654)
P<-c(0.1,0.2,0.4,0.3)
ERA<-P%*%SA
ERB<-P%*%SB
ERC<-P%*%SC
ERD<-P%*%SD
ER<-c(ERA,ERB,ERC,ERD)
ER
ExcessMeanA<-SA-c(ERA)#it understands that answer is vector
ExcessMeanB<-SB-c(ERB)
ExcessMeanC<-SC-c(ERC)
ExcessMeanD<-SD-c(ERD)
sigmaA<-sqrt(P%*%(ExcessMeanA)^2)
sigmaB<-sqrt(P%*%(ExcessMeanB)^2)
sigmaC<-sqrt(P%*%(ExcessMeanC)^2)
sigmaD<-sqrt(P%*%(ExcessMeanD)^2)
sigma<-c(sigmaA,sigmaB,sigmaC,sigmaD)
sigma
Erp<-function(WA,ERA,ERB){
Ep<-WA*ERA+(1-WA)*ERB
return(Ep)
}
Erp(c(0.5,0.6,0.95),c(ERA),c(ERB))
covAB<-P%*%(ExcessMeanA*ExcessMeanB)
covAB
sigmap<-function(WA,sigmaA,sigmaB,covAB){
sdp<-sqrt(WA^2*sigmaA^2+(1-WA)^2*sigmaB^2+2*WA*(1-WA)*covAB)
return(sdp)
}
sigp<-sigmap(c(0.5,0.6,0.95),c(sigmaA),c(sigmaB),c(covAB))
sigp
WA<-seq(0,1.2,by=0.0001)
plot(WA,sigmap(WA,0.05678908,0.1989975,0.0016))
min(sigmap(WA,0.05678908,0.1989975,0.0016))
WAmin<-match(min(sigmap(WA,0.05678908,0.1989975,0.0016)),sigmap(WA,0.05678908,0.1989975,0.0016))
cat('The minimal variance weight is',WA[WAmin],'\n')
#Alternatively  one can  use
match(min(sigmap(WA,0.05678908,0.1989975,0.0016)),
sigmap(WA,0.05678908,0.1989975,0.0016))
WA[9591]


WA<-seq(0,1.2,by=0.0001)
plot(sigmap(WA,0.05678908,0.1989975,0.0016),Erp(WA,0.105,0.18),pch='.')#opportunity set
rf<-0.09
a<-(Erp(0.4,0.1050,0.18)-rf)/(sigmap(0.4,0.05678908,0.1989975,0.0016))#CAL
x<-seq(0,1,by=0.0001)
lines(x,a*x+rf)#CAL
rf<-0.09
amax=max(Erp(WA,0.1050,0.18)-rf)/(sigmap(WA,0.05678908,0.1989975,0.0016))
x<-seq(0,1.2,by=0.0001)
lines(x,amax*x+rf,col='red')#SML
WAmin1<-match(max((Erp(WA,0.1050,0.18)-rf)/(sigmap(WA,0.05678908,0.1989975,0.0016))),
(Erp(WA,0.1050,0.18)-rf)/(sigmap(WA,0.05678908,0.1989975,0.0016)))
WAmin1
cat('The weight of stock A in the market portfolio is',WA[WAmin1])
sigpmin<-sigmap(WA[WAmin1],0.05678908,0.1989975,0.0016)
sigpmin
ErPM<-Erp(WA[WAmin1],0.1050,0.18)
ErPM

SigmaP<-0.04
Wp<-SigmaP/sigpmin
Wp
Wf<-1-Wp
Wf
WAminC<-Wp*WA[WAmin1]
WBminC<-Wp*(1-WA[WAmin1])
WminC<-c(WAminC,WBminC)
WminC
BetaA<-(ERA-rf)/(ErPM-rf)
BetaB<-(ERB-rf)/(ErPM-rf)
Betas<-c(BetaA,BetaB)
Betas
BetaC<-0.4
ERCc<-BetaC*(ErPM-rf)+rf
ERCc
ERc<-WAminC*ERA+WBminC*ERB+Wf*rf#complete portfolio return
ERc
