5%%3#remainder of division
5%/%3#quotient

#Example1.2.1.present value of $2 in 5 years,r=0.03 compounded annually
T<-5
r<-0.03
(1+r)^(-T)
#alternatively
PV<-function(FV,r,T){
  f1=FV/(1+r)^T
return(f1)}
PV(2,0.03,5)

#Remark1.2.3
x<-1
x< -1 
y< -1
#One should also use parentheses, or braces, but not brackets.
#We can also use a;to type two commands on the same line.

#1.2.2
#four types of variables:Numeric(R),complex(C),logical(True/False),character
x<-log(2)#log 
x

class(x)#out put the type of x
mode(x)#out put the type of x
#confirming that x is numeric by using the function mode() or class() 

is.numeric(x)#check whether the data x is numeric
is.character(x)#check whether the data x is character 

.Machine$double.xmax #gives the largest number (before infinity) 
#a number larger than that be given as inf
4e+309  
4e+309<Inf 
#The following code checks whether a x in complex (resp. character) or not.
x<-5+7i
is.complex #check whether or not x is a complex number 
x<-5+7i
is.complex(x) 
is.character(x) 
mode(x) 
x<-"good"
is.character(x) 
mode(x)

#each type of variable has the following structure:
#scalar ,vector, matrix, list, dataframe, factor

#Scalar(can be Numeric,complex,logical,character)
x<-exp(3)
x
y<-sin(3*pi/13)
x+y
x<--3i
x
x<-i
x<-2-1i
y<-2i-1
x+y
x*y

x<-"Actuarial" 
y<-"Mathematics"
x+y
paste(x,y)
paste(x,y,sep="")#no blank space between the 2 character sequences 
paste(x,y,sep=".") 

#VECTOR function c()
x<-c(0,2,6,-1,-5,1)
x
x[4]
x[c(1,5,2)]#extract 1th,5th,2th elements
y<-c(2,3^x)
y

#The multiplication of two vectors,x*y is defined as an element-wise product 
#where as the inner product of two vectors is defined using x%*%y or sum(x*y)
x<-c(0,2,6,-1,-5,1)
y<-c(1,4,-2,-2,1,5)
x*y #element-wise product 
x%*%y #inner product
sum(x*y) #inner product
z<-c(1,2,3,4,5,6,9)
x*z #same dimension

#function seq() and rep()
x<-seq(0,5.6,by=0.8)
x
y<-seq(1,8,length=7)
y[2:5] #extracts the 2nd to the 5th elements of y
z<-rep(3,7) #repeats the scalar 3,7times
z
x<-rep(c(1,5,2),2)#repeats the vector(1,5,2),2times
x
x<-rep(c(1,5,2),each=2)
x
length(x)
y<-c(1,5,2)
sort(y,decreasing=TRUE)

#MATRIX function matrix()
x<-4:15
x
#generate a sequence x from 4 to 15
y<-matrix(x,3,4)#generate a (3,4)-matrix
y
z<-matrix(x,3,5)#dimension

set.seed(1)
U<-runif(16)
U
options(digits=4)
U
U[U>.7]
U[(U>.7)&(U<.8)]
U[(U>.7)&(U<.75)]
#numeric(0) is returned when the test is FALSE for all components
which((U>.6)&(U<.8)) #return a vector containing the subscripts of the vector 
M<-matrix(U,nrow=4,ncol=4)
M
dim(M)
M[M[,4]>0.7,]#lines of M such that the element in the last column is bigger than 0.7
M[M[,4]>0.6,]
M[,M[,4]>0.7]#columns of M such that the element in the last column is bigger than 0.7
M[,M[4,]>0.7]#columns of M such that the element in the last line is bigger than 0.7
M[M[4,]>0.7,]#lines of M such that the element in the last line is bigger than 0.7

x<-2:5
y<-9:6
cbind(x,y)#combine column vectors
rbind(x,y)#combine row vectors
cbind(x,y,y)#combine 3 column vectors
diag(c(3,4,7,8))#generate a diagonal matrix
diag(rep(1,4))#generate a identity matrix

y<-matrix(seq(1,15,by=4),2,2)
y
y<-matrix(seq(1,17,by=3),2,3)
y
y[2,3]
y[2,]
dim(y)
ncol(y)

#matrix multiplication
m1<-rbind(c(1,2,3),c(0.2,1,0.5),c(1,0.5,2))
m1
x<-c(1,3,2)
x
m1*x
x*m1#same
m1%*%x#compute the inner product
x%*%m1#not same
m2<-rbind(c(1,-0.5,1),c(-1.5,-2,0.5),c(-1,1,0.5))
m2*m1
m1*m2#same
m1%*%m2
m2%*%m1#not same

#LIST
Rate<-list(month=c("Jan","Feb","Mar"),value=c(1.21,1.245,1.402),type='monthly-interest-rate')
Rate
str(Rate)#more compact
Rate$value
Rate$value[2]           
Rate[[2]]
names(Rate)
names(Rate)[1]<-"period" #change the name of the 1st item to "period"
names(Rate)
Rate<-list(month=c(1,2,3),value=c(1.2,2.245,4),type=c(1,3,6))
Rate
MarketRate<-data.frame(Rate) #the output format is different from the list
MarketRate


#FUNCTION 
circle<-function(r){
  return(2*pi*r)
  }
circle(1)
circle(c(2,3,6))
circle(3+2i)
f<-function(x,y){return(x^2+y^3-4)}
f(2,3)
g<-function(x){return(x^2*(x^2-2*x-15))}
optim.g<-optimize(g,interval=c(0,2),maximum=TRUE)
names(optim.g)
optim.g$maximum#x
optim.g$objective#y

mean(30:60)#find the mean from 30 to 60
sum(47:168)#find sum from 47 to 168


ydate<-as.Date('2016-05-29')
ydate
str(ydate)
weekdays(ydate)#find out what day of the week this
ydate+12
ydate+0:11#create a vector of twelve days starting on 29 May
weekdays(ydate+0:11)
ym<-seq(ydate,by="2 months",length.out=4)
ym
months(ym)
quarters(ym)

#example1.3.2
f<-function(k,m,n){
  f1=sum(18:k)+
    sqrt(n*m)
  return(f1)
  }
f(30,16.6,22)