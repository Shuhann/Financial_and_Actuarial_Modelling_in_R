#Q1. S0=40,N=2,T=4,u=3,d=0.5,r=0.25(per annum  compounded  annually),K=40
#European call, payoff=max((1/3)*Y2-40,0), Y2=S0+S1+S2
#(a)binomial tree of stock price 
#(b)initial value of call option
#(c)the number of units stocks that should be held at each node by replicating portfolio

build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)
}
build_stock_tree(40,3,0.5,2)

QM<-function(r,m,delta_t,u,d){
  return(((1+r/m)^(m*delta_t)-d)/(u-d))
}
D<-(1+r/m)^(-m*delta_t)
Q<-QM(r,m,delta_t,u,d)

S1<-c(40,20,10)
S2<-c(40,20,60)
S3<-c(40,120,60)
S4<-c(40,120,360)
S<-cbind(S1,S2,S3,S4)
S
T=4;r=0.25;m=1;u=3;d=0.5;K=40
delta_t=T/2#don't use N
Q
VT<-c(0,0,0,0)
for(i in 1:4){
  VT[i]<-max((1/3)*(S[1,i]+S[2,i]+S[3,i])-K,0)}
VT
V<-(Q^2*VT[4]+Q*(1-Q)*VT[3]+Q*(1-Q)*VT[2]+(1-Q)^2*VT[1])*(D)^2
V
V1<-c((Q*VT[4]+(1-Q)*VT[3])*D,(Q*VT[2]+(1-Q)*VT[1])*D)
V1
Del1<-c((VT[4]-VT[3])/(S4[3]-S3[3]),(VT[2]-VT[1])/(S2[3]-S1[3]))
Del1
Del0<-(V1[1]-V1[2])/(S4[2]-S2[2])
Del0


#Q2. S0=40
#first two months:42 or 38,r=0.06(per annum with monthly compounding)
#second two months:48 or 40 / 42 or 34,r=0.1(per annum with monthly compounding)
#(a)initial value of a derivative with payoff=0.5*(max(44-ST,0))^2
#(b)derivative is American style, initial value

S1<-c(40,42,48)
S2<-c(40,42,40)
S3<-c(40,38,42)
S4<-c(40,38,34)

QM<-function(S,r,m,delta_t,Su,Sd){
  return((S*(1+r/m)^(m*delta_t)-Sd)/(Su-Sd))#??????u???d???????????????-???S
}
T=4/12;r1=0.06;r2=0.1;m=12;S=40;Su=42;Sd=38;Suu1=48;Sud1=40;
Sdu2=42;Sdd2=34;K=44;delta_t=T/2

D1<-(1+r1/m)^(-m*delta_t)
D2<-(1+r2/m)^(-m*delta_t)

Q1<-QM(S,r1,m,delta_t,Su,Sd)
Q2u<-QM(Su,r2,m,delta_t,Suu1,Sud1)
Q2d<-QM(Sd,r2,m,delta_t,Sdu2,Sdd2)
Q1
Q2u
Q2d
St<-cbind(S1,S2,S3,S4)
St
VT<-c(0,0,0,0)
for(i in 1:4){
  VT[i]<-(1/2)*(max(44-St[3,i],0))^2}
VT
VE1<-c(((1-Q2d)*VT[4]+Q2d*VT[3])*D2,((1-Q2u)*VT[2]+Q2u*VT[1])*D2)
VE1
VE0<-((1-Q1)*VE1[1]+Q1*VE1[2])*D1
VA1<-c(max((1/2)*(max(44-St[2,3],0))^2,((1-Q2d)*VT[4]+Q2d*VT[3])*D2),
       max((1/2)*(max(44-St[2,2],0))^2,((1-Q2u)*VT[2]+Q2u*VT[1])*D2))
VA1
VA0<-max((1/2)*(max(44-St[1,1],0))^2,((1-Q1)*VE1[1]+Q1*VE1[2])*D1)
VA0


#Q3 S0=30,u=1.08,d=0.9,N=2,T=8/12,r=0.12(per annum with quarterly compounding)
#(a)binomial tree of stock price 
#(b)binomial tree of European put option,K=34,initial value
#(c)binomial tree of American put option,K=34,initial value

build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
QM<-function(r,delta_t,u,d,m){
  return(((1+r/m)^(m*delta_t)-d)/(u-d))
}
Bin_Op_Pri2<-function(tree,delta_t,r,m,u,d,K){
  D<-(1+r/m)^(-m*delta_t)
  Q<-QM(r,delta_t,u,d,m)
  optionE1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  optionA1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  optionE1_tree[nrow(optionE1_tree),]=pmax(K-tree[nrow(tree),],0)
  optionA1_tree[nrow(optionA1_tree),]=pmax(K-tree[nrow(tree),],0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      optionE1_tree[i,j]=(Q*optionE1_tree[i+1,j+1]+(1-Q)*optionE1_tree[i+1,j])*D
      optionA1_tree[i,j]=max(max(K-tree[i,j],0),(Q*optionE1_tree[i+1,j+1]+(1-Q)*optionE1_tree[i+1,j])*D)
      }
  }
  return(list(optionE1_tree,optionA1_tree))
}
Bin_Op_Pri_val<-function(S,T,r,K,u,d,N,m){
  Q<-QM(r,delta_t=T/N,u,d,m)
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,m=m,u=u,d=d,K=K)
  return(list(Q=Q,stock=tree,option=option,priceE1=option[[1]][1,1],priceA1=option[[2]][1,1]))
}
results2<-Bin_Op_Pri_val(S=30,T=8/12,r=0.12,K=34,u=1.08,d=0.9,N=2,m=4)
results2