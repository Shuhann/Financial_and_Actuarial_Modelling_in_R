#normal dist
#dnorm(x, mean = 0, sd = 1, log = FALSE)
#gives the density

#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#gives the distribution function

#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#generates quantile

#rnorm(n, mean = 0, sd = 1)
#generates random deviates


y<-rnorm(50)
hist(y, main = "NormalDistributionHistogramme", col = "darkorange")

x<-seq(0,1,by=0.02)
y<-qnorm(x,mean=2,sd=1)#DataFlair
plot(x,y, main = "qnorm()", col = "blue")#quantile

x<-seq(-20,20,by=.1)
y<-pnorm(x,mean=5.0,sd=1.0)#distribution
plot(x,y, main = "pnorm()", col = "blue")

x<-seq(-20,20,by=.1)
y<-dnorm(x,mean=5.0,sd=1.0)
plot(x,y, main = "NormalDistributionDensity", col = "red")


#Example4.1.5
#probability S bigger or equal to 62, S~(50,18^2)
pnorm(62,mean=50,sd=18,lower.tail=FALSE)

#Example4.1.6
#X~(0,1),find P(X<0.5365)
pnorm(0.5365, mean=0, sd=1, lower.tail=TRUE)

#Example4.1.7
#Y~LND(mu, sigma^2),mu=ln1000,sigma=0.2, find P(Y<800)
pnorm(log(800),mean=log(1000),sd=0.2,lower.tail=TRUE)
#Alternatively, one could use plnorm
plnorm(800,mean=log(1000),sd=0.2,lower.tail=TRUE)




#lognormal model

library(somebm)
Bm1<-bm(x0=0,t0=0,t=1,n=100)#Brownian motion simulator
plot(Bm1)

a<-bm(x0=1,t0=1,t=2,n=1000)
plot(a)

GBM1<-gbm(x0=1,mu=0,sigma=1,t0=0,t=1,n=1000)#Geometric Brownian motion simulator
plot(GBM1)

GBM2<-gbm(x0=0.001,mu=0,sigma=1,t0=0,t=1,n=1000)
plot(GBM2)

#Alternatively
t<-seq(0,1,by=0.01) #time
Sig2<-0.01 #first,simulate a set of random deviates
x<-rnorm(n=length(t)-1,sd=sqrt(Sig2))
cumsum(x)
##now compute their cumulative sum
x<-c(0,cumsum(x))
plot(t,x,type="l",ylim=c(-2,2))#type:l(line)/b(circle)


#generate several path of the Brownian motion, we have
Sig2<-0.01
nsim<-100
t<-seq(0,1,by=0.01)
X<-matrix(0,nsim,length(t))#nsim row, length(t) column
for(i in 1:nsim)X[i,]<-c(0,cumsum(rnorm(n=length(t)-1,sd=sqrt(Sig2))))
plot(t,X[1,],xlab="time",ylab="BMvalue",ylim=c(-3,3),type="l")
for(i in 1:nsim)
  lines(t,X[i,])


#change the normal to uniform distribution
t<-seq(0,1,by=0.01)#time
Sig3<-0.01
nsim<-100 
#we will simulate the steps from a uniform distribution 
#with limits set to have the same variance(0.01) as before 
X<-matrix(0,nsim,length(t))
for(i in 1:nsim)X[i,]<-c(0,cumsum(runif(n=length(t)-1,
                                        max=sqrt(3*Sig3),min=-sqrt(3*Sig3))))
plot(t,X[1,],xlab="time",ylab="BMvalueUnifD",ylim=c(-3,3),type="l")
for(i in 1:nsim)
  lines(t,X[i,])


#example4.1.10
M<-seq(0,11)
S<-c(933,1072,1052,1084,1031,1067,1141,948,889,955,1286,1351)
length(S)
Mat1<-cbind(M,S)
Mat1

#alternatively,we can use data.frame
Mat2<-data.frame(Week=M,StockPrice=S)#a matrix of weekly price
Mat2

z1<-c()
for(i in 2:12){
  z1[i]<-log(S[i]/S[i-1])
}
z1
Mat3<-cbind(Mat2,z1)
Mat3
length(z1)
Me<-mean(z1,na.rm=TRUE)#we remove all NA value when computing the mean
Me
Vec<-sd(z1,na.rm=TRUE)
Vec
#alternatively we can use the following code to find the mean
Sum1<-function(n){
  tmp<-0
  for(i in 2:n){
    tmp<-tmp+z1[i]}
  return(tmp)}
Sum1(12)/11
#We find the mean and volatility on the return as follows
Mu1<-Me*52#since we have 52 trading weeks
Mu1
Sigma1<-Vec*sqrt(52)
Sigma1
#question(2)
Mu<-Mu1*6/52+log(1072)
Sigma2<-Sigma1*sqrt(6/52)
pnorm(log(1500),mean=Mu,sd=Sigma2,lower.tail=FALSE)
#alternatively
Mu2<-Me*6+log(1072)
Sigma3<-Vec*sqrt(6)
pnorm(log(1500),mean=Mu2,sd=Sigma3,lower.tail=FALSE)
