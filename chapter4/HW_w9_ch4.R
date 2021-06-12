#Q1
s=52;k=50;v=0.3;r=0.12;tt=3/12;d=0;
Greeks<-greeks(bsput(s,k,v,r,tt,d))
Greeks
#Thepriceis£1.579663437

#Q2
S=70;r=0.1;X=65;sigma=0.32;time1=3/12;Time2=8/12;D=1
RollGeskeWhaleyOption(S,X,time1,Time2,r,D,sigma,
title=NULL,description=NULL)

#Q4
s=20.5;k=20;v=0.3;r=0.12;tt=168/365;d=0.027;
Greeks<-greeks(bscall(s,k,v,r,tt,d))
Greeks
#The price is £2.334519319