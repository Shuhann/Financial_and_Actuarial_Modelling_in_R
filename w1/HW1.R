#Example2
x<-c(1,2,3,1)
y<-c(4,5,6,3)
z<-c(7,8,9,4)
m<-cbind(x,y,z)
print(m)

#example3
options(digits=7)
F=function(P,r,T){
  f1=P*(1+r)^T
  return(f1)}
F(2350,0.05,T=c(1:10))

#example4
x<-function(k){
  g=length(seq(4,k,by=4))
  return(g)}#can't choose k<4
x(8)
x(40)

x<-function(k){
  g<-1:k
  i<-ifelse(g%%4==0,TRUE,FALSE)
  g[i]
  a=length(g[i])
  return(a)} 
x(12)
# g<-1:12
# > g
# [1]  1  2  3  4  5  6  7  8  9 10 11 12
# > i<-ifelse(g%%4==0,TRUE,FALSE)
# > i
# [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
# [8]  TRUE FALSE FALSE FALSE  TRUE
# > g[i]
# [1]  4  8 12
x(3)
x(7)
x(8)


#example5
print('First 7 letters in lowercase:')
h=head(letters,7)
print(h)
print('Last 5 letters inuppercase:')
t=tail(LETTERS,6)
print(t)
print('Letters between 15th to 21st letters in uppercase:')
e=letters[15:21]
print(e)

#example6
#Write a R program to create a vector which contains 10 random integer values
#between -50 and +50.  
#Find the mean of those number and their standard deviation
v=sample(-50:50,10,replace=TRUE)
#sample(x, size, replace = FALSE, prob = NULL)(should sampling be with replacement?)
print('10 random integer values between ???50 and +50:' )
print(v)
mean(v)
sqrt(var(v))
sd(v) #this can also be used for the standard deviation


#example7
u<-runif(25)
sum(u)
#alternatively
sum<-function(n){
  tmp<-0
  for(i in 1:n){
    tmp<-tmp+u[i]
    }
  return(tmp)
  }
sum(25)

v1<-c(2,7,9)
v2<-c(15,18,12,16)
result<-array(c(v1,v2),dim=c(4,3,3))
#array(data = NA, dim = length(data), dimnames = NULL)
result

result1<-array(c(v1,v2),dim=c(4,3,2))
result1

result2<-array(c(v1,v2),dim=c(3,4,3))
result2


#example9
v1<-runif(6 ,-4 ,4)#runif(n,min=0,max =1)
v2<-runif(12 ,-4 ,4)
v1
v2
Marray<-array(c(v1,v2),dim=c(3 ,6 ,3))
Marray
Marray[2,,3]#second row of the third matrix of the arra  
Marray[3,5,2]#element  in  the  third row and fifth column of the second matrix
print(Marray[,Marray[3,,2]>2,2])
#columns of second matrix such that the element in the last line is bigger than 2.