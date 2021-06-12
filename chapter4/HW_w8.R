#Q1
#recall we use the BS model
S=50;mu=0.15;sigma=0.25;t=1
mu<-log(S)+(mu-sigma^2/2)*t
VarS<-sigma^2*t
x<-c(mu,VarS)
x
#it is a lognormal distribution with mean 0.15 and variance 0.0625.

#Q2
S=30;mu=0.16;sigma=0.3;k=35;t=6/12
mu1<-log(S)+(mu-sigma^2/2)*t
sig<-sigma*sqrt(t)
#(a)the call is exercised if S<k
pnorm(log(k),mu1,sig,lower.tail=FALSE)
plnorm(k,mu1,sig,lower.tail=FALSE)

#(2)the put is exercised if S>k
1-pnorm(log(k),mu1,sig,lower.tail=FALSE)
#OR
plnorm(k,mu1,sig,lower.tail=TRUE)
#OR
pnorm(log(k),mu1,sig)


#(3)find new mu and consider S(2+3)/S(2)
S=30;mu=0.16;sigma=0.3;k=35;t=3
mu2<-(mu-sigma^2/2)*t
sig<-sigma*sqrt(t)
plnorm(2,mu2,sig,lower.tail=TRUE)
#alternatively one can use
pnorm(log(2),mu2,sig)

#Q3
library(fOptions)
s=40;r=0.12;v=0.3;k=38;tt=3/12;b=0.12;d=0
GBSOption(TypeFlag='c',s,k,tt,r,b,v)

greeks(bscall(s,k,v,r,tt,d))
#close but not the same


#Q4
#(a&b)
S=50;mu=0.18;sigma=0.3;t=2
mu<-log(S)+(mu-sigma^2/2)*t
VarS<-sigma^2*t
x<-c(mu,VarS)
x
#It is a lognormal distribution with mean 4.182023 and variance 0.180000

#(c)
lower.bound<-exp(mu-qnorm(0.975,0,1)*sqrt(VarS))
upper.bound<-exp(mu+qnorm(0.975,0,1)*sqrt(VarS))
print(c(lower.bound,upper.bound))

#Q5
s=35;r=0.05;v=0.25;k=40;tt=3/52;b=0.05;d=0
GBSOption(TypeFlag='c',s,k,tt,r,b,v)
#alternatively
library(derivmkts)
greeks(bscall(s,k,v,r,tt,d))
#price of European call is 0.011742
#price of American call is 0.011742
GBSOption(TypeFlag='p',s,k,tt,r,b,v)
greeks(bsput(s,k,v,r,tt,d))
#The price of the European put is 4.896523452

35+GBSOption(TypeFlag='p',s,k,tt,r,b,v)@price-+GBSOption(TypeFlag='c',s,k,tt,r,b,v)@price-k*exp(-r*tt)

BSAmericanApproxOption("p",s,k,tt,r,b,v,
                       title=NULL,description=NULL)
#The price of the European put is 5
