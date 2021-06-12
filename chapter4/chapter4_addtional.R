#w9addtional
#Q1
s=30;k=29;v=0.25;r=0.05;tt=4/12;d=0;
s1<-s-0.5*exp(-(1.5/12)*0.05)
Greeks<-greeks(bscall(s1,k,v,r,tt,d))
Greeks

Greeks1<-greeks(bsput(s1,k,v,r,tt,d))
Greeks1
#(a)
#The price of the option is 2.205997125 if it's a European call.
#(b)
#The price of the option is 1.223554031 if it's a European put.
#(c)
#for the European call we should hold 0.6205 share(delta)
#for the European put we selling short 0.3795 share(delta)

S=30;X=29;time1=1.5/12;Time2=4/12;r=0.05;D=0.5;sigma=0.25
RollGeskeWhaleyOption(S,X,time1,Time2,r,D,sigma,title=NULL,description=NULL)
#The price of the option is 2.2166 if it's a American call.


#w10 additional
#Q1
s=1416;k=1450;v=0.42;r=0.05;tt=2/3;d=0;
Greeks<-greeks(bscall(s,k,v,r,tt,d))
Greeks
#for the plot
k=1450;v=0.42;r=0.05;tt=2/3;d=0;
s<-seq(.5,2000,by=.5)
Call1<-greeks(bscall(s,k,v,r,tt,d))
z<-Call1
rownames(z)
par(mfrow=c(2,2))
plot(s,z['Delta',],main=paste('Delta'),ylab='Delta',
     type='l',col='red')
plot(s,z['Premium',],main=paste('Premium'),ylab='Premium',
     type='l',col='red')
plot(s,z['Vega',],main=paste('Vega'),ylab='Vega',
     type='l',col='red')
plot(s,z['Theta',],main=paste('Theta'),ylab='Theta',
     type='l',col='red')