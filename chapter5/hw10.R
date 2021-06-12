#Q1
library(actuar)
pn<-c(0.5,0.4,0,0.1)
fx<-c(0,0.9,0,0,0,0,0,0,0,0,0.1)
Fs<-aggregateDist("convolution",model.freq=pn,
                  model.sev=fx)
summary(Fs)
Fs(0)
y<-c(Fs(0),diff(Fs(1*0:5)))
y
1-(y[1]+y[2]+y[3]+y[4])


#Q2
pn<-c(0.5,0.2,0.2,0.1)
fx2<-discretize(pnorm(x,100,3),method="lower",from=0,to=150,step=0.5)
Fs<-aggregateDist("convolution",model.freq=pn,model.sev=fx2,x.scale = 0.5)
summary(Fs)
Fs(0)
y<-c(Fs(0),diff(Fs(0.5*0:150)))
z<-function(n){
  tmp<-0
  for(i in 1:n)(tmp<-tmp+y[i])
  return(tmp)
}
1-z(201)
#or
1-Fs(201)



#Q3
Lam<-1
Ex<-1*0.2+2*0.3+3*0.5
Ex
Ex2<-(1)^2*0.2+(2)^2*0.3+(3)^2*0.5
Ex2
Ca<-2.99
Bounds<-2*(Ca-Lam*Ex)/(Lam*Ex2)
Bounds
mgfy<-function(x)(0.2*exp(x)+0.3*exp(2*x)+0.5*exp(3*x))
R<-adjCoef(mgf.claim=mgfy(x),mgf.wait=mgfexp(x,Lam),prem=Ca,upper=Bounds)
R

 

#Q4
Lam<-4
f<-function(x)(x*(1/2)*(2*exp(-2*x)+3*exp(-3*x)))#x*f(x)
g<-function(x)(x^2*(1/2)*(2*exp(-2*x)+3*exp(-3*x)))#(x^2)*f(x)
Ex<-integrate(f,0,Inf)$value
Ex
Ex2<-integrate(g,0,Inf)$value
Ex2
C<-3
Bounds<-2*(C-Lam*Ex)/(Lam*Ex2)
Bounds
mgfx<-function(x){if(x<2)
{return(1/(2-x)+(3/2)/(3-x))}
  else{return(0)}
}
R<-adjCoef(mgf.claim=mgfx(x),mgf.wait=mgfexp(x,4),prem=C,upper=min(Bounds,2))
R
 

 

#Q5
Lam<-2
Thet<-0.4
f<-function(x)(x*(1/2)*(3*exp(-3*x)+7*exp(-7*x))) 
Ex<-integrate(f,0,Inf)$value#first moment
c<-(1+Thet)*Lam*Ex
psi<-ruin(claims="e",par.claims=list(rate=c(3,7),w=0.5),#mixture of exponential dist.
          wait="e",par.wait=list(rate=Lam),pre=c)
u<-seq(0,100,by=0.01)
plot(u,psi(u),type="l")
psi(1)

#Q6
Thet<-4/11
c<-2
f<-function(x)(x*(exp(-3*x)+(10/3)*exp(-5*x)))
Ex<-integrate(f,0,Inf)$value#first moment
Lam<-c/((1+Thet)*Ex)
Lam
psi<-ruin(claims="e",par.claims=list(rate=c(3,5),w=c(1/3,2/3)),#mixture of exponential dist.
          wait="e",par.wait=list(rate=Lam),pre=c)
u<-seq(0,100,by=0.01)
plot(u,psi(u),type="l")
psi(2)
