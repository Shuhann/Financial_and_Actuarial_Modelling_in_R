#w9 additional
#Q2
fx1<-c(0,0.3,0.7)
pn<-c(0.4,0.4,0.2)
Fs<-aggregateDist('convolution',model.freq=pn,
                   model.sev=fx1,x.scale=20)#cdf
summary(Fs)
Fs(0)
#the pf is given by:
c(Fs(0),diff(Fs(20*0:10)))
plot(Fs)


#Q3
fx<-discretize(ppois(x,6),method='lower',
               from=0,to=100,step=1)
fx1<-c(0,1/3,1/3,0,1/3)
Fs<-aggregateDist('convolution',model.freq=fx,
model.sev=fx1,x.scale=1)
summary(Fs)
Fs(0)
x<-c(Fs(0),diff(Fs(1*0:100)))
x[4+1]
x[15+1]

#Alternatively,one has
fx1<-c(0,1/3,1/3,0,1/3)
Fs<-aggregateDist('recursive',model.freq='poisson',
model.sev=fx1,lambda=6,x.scale=1)
summary(Fs)
Fs(0)
y<-diff(Fs)
y
y[4+1]
y[15+1]

#W10 additional
#Q2
c<-1
Lam<-2
f<-function(x){return(x*3*exp(-3*x))}
Exp1<-integrate(f,0,Inf)$value
Thet<-c/(Lam*Exp1)-1
Thet
psi<-ruin(claims='e',par.claims=list(rate=3),wait='e',par.wait=list(rate=Lam),pre=c)
u<-seq(0,100,by=0.01)
plot(u,psi(u),type='l')
psi(2)


#Q3
fx<-c(0,1.9/3,0.8/3,0.3/3)
Fs<-aggregateDist('recursive',model.freq='poisson',model.sev=fx,lambda=3)
summary(Fs)
Fs(0)
y<-c(Fs(0),diff(Fs(1*0:20)))
y[3]