#Q1
g1<-function(x){
return((1/(sqrt(0.3)*sqrt(2*pi)))*exp(x-((x-3.5)^2)/(2*0.3)))
}
Ex1<-integrate(g1,-Inf,Inf)$value
Ex1

g2<-function(x){
return((1/(sqrt(0.3)*sqrt(2*pi)))*exp(-x-((x-3.5)^2)/(2*0.3))
)}
Ex2<-integrate(g2,-Inf,Inf)$value
Ex2

Ex<-(Ex1-Ex2)/2
Ex

#alternatively
#install package 'actuar'
library(actuar)
mgfnorm(1,3.5,sqrt(0.3))
mgfnorm(-1,3.5,sqrt(0.3))
(1/2)*(mgfnorm(1,3.5,sqrt(0.3))-mgfnorm(-1,3.5,sqrt(0.3)))



#Q2
plnorm(200,mean=log(250),sd=0.1,lower.tail=TRUE)
#alternatively
pnorm(log(200),mean=log(250),sd=0.1,lower.tail=TRUE)



#Q3
M<-seq(0,12)
S<-c(14.94,15.20,15.46,15.40,15.41,15.38,15.14,15.28,15.59,15.53,15.66,15.36,15.33)
length(S)
Mat1<-cbind(M,S)
Mat1

#alternatively,we can use data.frame
Mat2<-data.frame(Week=M,StockPrice=S)#a matrix of price
Mat2

z1<-c()
for(i in 2:13){
z1[i]<-log(S[i]/S[i-1])
}
z1

Mat3<-cbind(Mat2,z1)
Mat3
 

Me<-mean(z1,na.rm=TRUE)#weremoveallNAvaluewhencomputingthemean
Me
Vec1<-sd(z1,na.rm=TRUE)
Vec1

#alternatively we can use the following code to find the mean
Sum1<-function(n){
tmp<-0
for(i in 2:n){
tmp<-tmp+z1[i]}
return(tmp)}
Sum1(13)/12

#alternatively
z2<-diff(log(Mat2[,2]))#generate the return directly
z2
mean(z2)

#We find the mean and volatility on the return as follows
Mu1<-Me*252#since we have 252 trading days
Mu1
Sigma1<-Vec1*sqrt(252)
Sigma1


#(b)
Mu<-Mu1*63/252+log(15.2)
Sigma2<-Sigma1*sqrt(63/252)
pnorm(log(17),mean=Mu,sd=Sigma2,lower.tail=FALSE)
#or
plnorm(17,mean=Mu,sd=Sigma2,lower.tail=FALSE)


#(c)
Mu2<-Mu1*5/252
Sigma2<-Sigma1*sqrt(5/252)
plnorm(1,mean=Mu2,sd=Sigma2,lower.tail=FALSE)