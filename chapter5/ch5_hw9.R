#Q1
library(derivmkts)
s=52;k=50;v=0.3;r=0.12;tt=3/12;d=0;
Greeks<-greeks(bsput(s,k,v,r,tt,d))
Greeks

#Q2
library(fOptions)
S=70;r=0.1;X=65;sigma=0.32;time1=3/12;time2=8/12;D=1
RollGeskeWhaleyOption(S,X,time1,time2,r,D,sigma,
                      title=NULL,description=NULL)

S=70;r=0.1;X=65;sigma=0.32;time1=3/12;time2=8/12;D=10#change
RollGeskeWhaleyOption(S,X,time1,time2,r,D,sigma,
                      title=NULL,description=NULL)
#exercise the option earlier

#Q3
library(actuar)
pn<-c(0.3,0.3,0.2,0.2)
fx1<-c(0,0.2,0.5,0,0.3)
Fs<-aggregateDist('convolution',model.freq=pn,
                  model.sev=fx1,x.scale=1)
summary(Fs)
Fs(0)
#the pf is given by:
c(Fs(0),diff(Fs(1*0:30)))
plot(Fs)


#Q4
s=20.5;r=0.12;k=20;v=0.3;tt=168/365;d=0.027
Greeks<-greeks(bscall(s,k,v,r,tt,d))
Greeks


#Q5
fx1<-c(0,1/4,1/4,0,1/4,1/12,0,1/12,0,1/12)
Fs<-aggregateDist('recursive',model.freq='poisson',
                  model.sev=fx1,lambda=5)
summary(Fs)
y<-c(Fs(0),diff(Fs(1*0:30)))
y[8+1]
y[25+1]