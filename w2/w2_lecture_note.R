#1.4 control statement-if/for/while
#if
r<-2.5
x<-'radius'
if(x='radius')
{z<-2*pi*r
   }
z
#if...else...
r<-2.5#define radius
x<-"area" 
if(x=="radius")
{z<-2*pi*r}else{
    z<-pi*r^2
    }
z

#for
#factorial of n
fac<-function(n){tmp<-1  
 for(i in 1:n){
    tmp<-tmp*i}
return(tmp)}
fac(5)
fac(4)
fac(0)#adjust

#while_way
fac1<-function(n){
  tmp<-1
  i<-1
  while(i<=n){
    tmp<-tmp*i
    i<-i+1
  }
  return(tmp)
}
fac1(5)
fac1(4)
fac1(0)

#if_way
fac2<-function(n){
  if(n==0){return(1)}
  else if(n<0){return('n should be non negative')}
  else{
    tmp<-1
    for(i in 1:n){tmp<-tmp*i}
    return(tmp)}
  }
fac2(5)
fac2(0)
fac2(-5)

#sum from 1 to n
#for_way
sum1<-function(n){
  tmp<-0
  for(i in 1:n){tmp<-tmp+i}
  return(tmp)}
sum1(5)
#while_way
sum2<-function(n){tmp<-0
i<-1
while (i<=n) {tmp<-tmp+i
i=i+1
}
return(tmp)}
sum2(5)

#1.5 GRAPHICS plot()
x<-rnorm(12)#generate random numbers from a standard normal distribution
x
plot(x)
y<-rnorm(12) 
y
plot(x,y)

x<-seq(-3,3,by=0.1)
y<-3*x^3+2*x^2-4
z<-exp(3*x)
t<-x^2-5*x+7
plot(x,y,type="l")#l:line
points(x,z)#add points of z=exp(3*x)
lines(x,z,col="red")
points(x,t)#add points of z=x^2-5*x+7
lines(x,t,type="p")#p:balls

#1.6reading and writing data-write.table()/write.csv()/read.csv()
mat<-rbind(c(1,2,3.1),c(2,4,5),c(9,7,8))
mat
write.csv(mat,"matrix.csv")
write.table(mat,"matrix.txt")
Rmat1<-read.table("matrix.txt")
Rmat1

#1.7Basic statistics
x<-seq(10,20)
mean(x)
ave(x)
mean((x-ave(x))^2)#to calculate sample variance
var(x)#unbiased variance
mean<-var(x)*10/11#sample variance
mean

rnorm(100)
# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# rnorm(n, mean = 0, sd = 1)
mean(rnorm(100))
mean(rnorm(1000))
mean(rnorm(10000))
mean(rnorm(100000))
mean(rnorm(1000000))
mean(rnorm(100000,3,2))
mean(rnorm(10000000,3,2))


x<-seq(-5,5,by=0.1)
plot(x,dnorm(x),"l")#plot the pdf
plot(x,pnorm(x),"l")#plot the cdf

x<-seq(0,1,by=0.01)
plot(x,dbeta(x,0.5,0.7),"l")
plot(x,pbeta(x,0.5,0.7),"l")#beta distribution

#poisson dist.
#dpois(x,l,log=FALSE): gives the density
#ppois(q,l,lower.tail=TRUE,log.p=FALSE): gives the distribution function
#qpois(p,l,lower.tail=TRUE,log.p=FALSE): generates quantile
#rpois(n,l): generates random deviates
#where
#x: vector of (non-negative integer) quantiles.
#q: vector of quantiles.
#p: vector of probabilities.
#n: number of random values to return
#l: vector of (non-negative) means.
#log,log.p:logical;if TRUE,probabilities p are given as log(p)
#lower.tail:logical;if TRUE (default), probabilities are P[X<=x], otherwise,P[X>x].
y<-c(.1,.35,.15,.25)
qpois(y,2)
qpois(y,6)

#BINOMIAL DIST.
#dbinom(x,size,prob,log=FALSE)
#pbinom(q,size,prob,lower.tail=TRUE,log.p=FALSE)
#qbinom(p,size,prob,lower.tail=TRUE,log.p=FALSE)
#rbinom(n,size,prob)
#where
#x,q:vector of quantiles
#p:vector of probabilities
#n: number of observations. If length(n)>1, the length is taken to be the number required
#size: number of trials (zero or more)
#prob: probability of success on each trial
#log,log.p: logical; if TRUE, probabilities p are given as log(p)
#lower.tail: logical; if TRUE (default), probabilities are P[X<=x], otherwise,P[X>x]


#EXPONENTIAL DIST.
# dexp(x,rate=a,log=FALSE)
# pexp(q,rate=a,lower.tail=TRUE,log.p=FALSE)
# qexp(p,rate=a,lower.tail=TRUE,log.p=FALSE)
# rexp(n,rate=a)
# where
# x,q: vector of quantiles
# p: vector of probabilities
# n: number of observations. Iflength(n)>1, the length is taken to be the number required.
#rate: vector of rates
#log,log.p:logical;if TRUE, probabilities p are given as log(p)
#lower.tail:logical;if TRUE (default), probabilities are P[X<=x], otherwise,P[X>x]
#If rate is not specified, it assumes the default value of 1

#Example1.7.8
1-pnorm(1.8,mean=0,sd=1,lower.tail=TRUE,log.p=FALSE)
pnorm(1.8,mean=0,sd=1,lower.tail=FALSE,log.p=FALSE)
qexp(0.6,4,lower.tail=TRUE,log.p=FALSE)

#Example1.7.9
pnorm(22.5,20,5)-pnorm(12,20,5)
sum(dbinom(c(10,35,50),80,prob=0.5))
pbinom(39,80,prob=0.5)#less means strictly less 
pbinom(37,80,prob=0.5)-pbinom(22,80,prob=0.5)
1-ppois(8,4)
