#example5.1.8
fx<-discretize(pgamma(x,3,1),method="upper",
               from=0,to=15,step=0.5)
fx
fx1<-discretize(pgamma(x,3,1),method="unbiased",
               from=0,to=15,step=0.5,lev=levgamma(x,3,1))
fx1

#example5.1.9
fx2<-discretize(pgamma(x,3,1),method="unbiased",
                from=0,to=25,step=0.5,lev=levgamma(x,3,1))
Fs2<-aggregateDist("recursive",model.freq="poisson",
                   model.sev=fx2,lambda=5,x.scale=0.5)
Fs2

summary(Fs2)
knots(Fs2)
plot(Fs2,do.points=FALSE,verticals=TRUE,xlim=c(0,80))
mean(Fs2)
quantile(Fs2)

VaR(Fs2)
CTE(Fs2)


#example5.1.13
library(actuar)
fx<-c(0,4/5,1/5)
Fs<-aggregateDist("recursive",model.freq="poisson",
                  model.sev=fx,lambda=3)
summary(Fs)
Fs(0)
y<-c(Fs(0),diff(Fs(1*0:9)))
y
1-(y[1]+y[2]+y[3]+y[4]+y[5]+y[6])


#5.2ruin theory

#example5.2.12
c<-1
Lam<-2
f<-function(x){return(x*3*exp(-3*x))}
Exp1<-integrate(f,0,Inf)$value
Exp1
#alternatively
f1<-function(x){return(x*dexp(x,3))}#density
Exp2<-integrate(f1,0,Inf)$value
Exp2#same with Exp1

Thet<-c/(Lam*Exp1)-1
Thet
psi<-ruin(claims="e",par.claims=list(rate=3),wait="e",par.wait
          =list(rate=Lam),pre=c)
u<-seq(0,100,by=0.01)
plot(u,psi(u),type="l")
psi(2)


#example5.2.15#R

f<-function(x)(x*dexp(x,1))
g<-function(x)(x^2*dexp(x,1))
Ex<-integrate(f,0,Inf)$value#first moment
Ex2<-integrate(g,0,Inf)$value#second moment
Thet<-0.2
Bound<-(2*Thet*Ex)/(Ex2)
adjCoef(mgf.claim=mgfexp(x),mgf.wait=mgfexp(x,2),
        premium.rate=2.4,upper=Bound)#without the"upper=Bound",one has an error

#example5.2.16#R
Lam<-4
Ex<-1*0.6+2*0.4
Ex
Ex2<-(1)^2*0.6+(2)^2*0.4
Ex2
Ca<-7
Bounds<-2*(Ca-Lam*Ex)/(Lam*Ex2)
Bounds
mgfy<-function(x)(0.6*exp(x)+0.4*exp(2*x))
adjCoef(mgf.claim=mgfy(x),mgf.wait=mgfexp(x,4),prem=Ca,upper=Bounds)

#example5.2.17
Lam<-4
f<-function(x)(x*(1/2)*(3*exp(-3*x)+7*exp(-7*x)))#x*f(x)
g<-function(x)(x^2*(1/2)*(3*exp(-3*x)+7*exp(-7*x)))#(x^2)*f(x)
Ex<-integrate(f,0,Inf)$value
Ex
Ex2<-integrate(g,0,Inf)$value
Ex2
C<-2
Bounds<-2*(C-Lam*Ex)/(Lam*Ex2)
mgfx<-function(x){if(x<3)
  {return((1/2)*(3 /(3-x) + 7/(7-x)))}
  else{return(0)}
  }
R<-adjCoef(mgf.claim=mgfx(x),mgf.wait=mgfexp(x,4),prem=C,upper=min(Bounds,3))
R
RBoun<-exp(-R*2)#bound of probability of ruin
RBoun

#EXAMPLE5.2.19
Lam<-2
f<-function(x)(x*dexp(x,1))
g<-function(x)(x^2*dexp(x,1))
Ex<-integrate(f,0,Inf)$value
Ex2<-integrate(g,0,Inf)$value
Thet<-0.2
Theth<-0.3
Cp<-function(x)(((1+Thet)-(1+Theth)*(1-x))*Lam*Ex)
mgfy<-function(x,y)(mgfexp(x*y))
Ad1<-adjCoef(mgfy,mgf.wait=mgfexp(x,2),
             premium.rate=Cp,upper=1,reins="prop",from=0,to=1)
Ad1(c(0.75,0.8,0.9,1)) 
plot(Ad1, xlab='x',ylab = 'R(x)',main='Adjustment Coefficient')

#example5.2.20
Lam<-1
f<-function(x)(x*dexp(x,1))
g<-function(x)(x^2*dexp(x,1))
Ex<-integrate(f,0,Inf)$value
Ex2<-integrate(g,0,Inf)$value
Thet<-0.2
Theth<-0.25
Cp<-function(x)(((1+Thet)-(1+Theth)*(1-x))*Lam*Ex)
mgfy<-function(x,y)(mgfexp(x*y))
Ad2<-adjCoef(mgfy,mgf.wait=mgfexp(x,1),
             premium.rate=Cp,upper=1,reins="prop",from=0,to=1)
Ad2(0.8)