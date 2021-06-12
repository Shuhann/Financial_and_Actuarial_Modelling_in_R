#simulate stock price
library(derivmkts)
library(ggplot2)
s<-simprice(s0=69,v=0.35,r=0.05,tt=0.5,d=0,trials=10,
            periods=180,jump=FALSE,long=TRUE,seed=1)
#v:annualized standard deviation of the continuously-compounded return
#tt:Time to maturity in years
#d:Dividend yield, annualized, continuously-compounded
ggplot(s,aes(x=period,y=price,color=trial))+
  geom_line()


#example4.2.4
#(a)
mu2<-0.1
SigmaS<-0.2
T<-3
MeanSt<-50*exp(mu2*T)
MeanSt

###x<-exp(log(50)+(mu2-SigmaS^2/2)*T)
###x

#(b)
T1<-9/12
mu<-log(50)+(mu2-SigmaS^2/2)*T1
Sig<-SigmaS*sqrt(T1)
pnorm(log(70),mu,Sig,lower.tail=FALSE)
#(c)
T1<-2
mu<-(mu2-SigmaS^2/2)*T1
Sig<-SigmaS*sqrt(T1)
pnorm(0,mu,Sig,lower.tail=TRUE)

#example4.2.6
library(fOptions)
GBSOption(TypeFlag="c",S=1416,X=1450,Time=8/12,r=0.05,b=0.05,sigma=0.42)
#alternatively
BlackScholes<-function(S,K,r,T,sigma,type){
  if(type=="Call"){
    d1<-(log(S/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
    d2<-d1-sigma*sqrt(T)
    value<-S*pnorm(d1)-K*exp(-r*T)*pnorm(d2)
    return(value)
    }
  if(type=="Put"){
    d1<-(log(S/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
    d2<-d1-sigma*sqrt(T)
    value<-(K*exp(-r*T)*pnorm(-d2)-S*pnorm(-d1))
    return(value)
    }
  }
callprice<-BlackScholes(1416,1450,0.05,8/12,0.42,"Call")
putprice<-BlackScholes(1416, 1450, 0.05, 8/12, 0.42, "Put")
value<-cbind(callprice,putprice)
value

#Alternatively
library(derivmkts)
s=1416;k=1450;v=0.42;r=0.05;tt=2/3;d=0;
bscall(s,k,v,r,tt,d)
bsput(s,k,v,r,tt,d)


#example4.2.7
GBSOption(TypeFlag="p",S=21.6,X=25,Time=3/12,r=0.01,b=0.01,sigma=0.35)
#Alternatively
s=21.6;k=25;v=0.35;r=0.01;tt=3/12;d=0;
bsput(s,k,v,r,tt,d)
#alternatively
BlackScholes_put<-function(S,K,r,T,sigma,type){
    d1<-(log(S/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
    d2<-d1-sigma*sqrt(T)
    value<-(K*exp(-r*T)*pnorm(-d2)-S*pnorm(-d1))
    return(value)
  }
putprice<-BlackScholes_put(21.6,25, 0.01, 3/12, 0.35)
putprice


#4.3Greeks
#example4.3.3
s=1416;k=1450;v=0.42;r=0.05;tt=2/3;d=0;
Greeks<-greeks(bscall(s,k,v,r,tt,d))
Greeks
#for the plot
k=1450;v=0.42;r=0.05;tt=2/3;d=0;
s<-seq(.5,2000,by=.5)
Call1<-greeks(bscall(s,k,v,r,tt,d))
z<-Call1
par(mfrow=c(4,2))
for(j in rownames(z)){#loop over greeks
  plot(s,z[j,],main=paste(j),ylab=j,type='l',col="red")
  }


#example4.3.5
s=33;k=30;v=0.20;r=0.05;tt=4/12;d=0;
Greeks<-greeks(bscall(s,k,v,r,tt,d))
Greeks
#the price of the American call is that of a european call since there are no dividend.
#the price of the American put option is given by:
BSAmericanApproxOption("p",S=33,X=30,Time=4/12,r=0.05,b=0.05,sigma=0.2,
                       title=NULL,description=NULL)

#alternatively use the following
BAWAmericanApproxOption("p",S=33,X=30,Time=4/12,r=0.05,b=0.05,sigma=0.2,
                        title=NULL,description=NULL)

#price of European put
Greeks1<-greeks(bsput(s,k,v,r,tt,d))
Greeks1