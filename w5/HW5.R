#Q1. S0=30,u=1.08,d=0.9,N=2,T=8/12,r=0.12(per annum with quarterly compounding)
#binomial tree of European put option,K=34,initial value?
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
  optionE1_tree[nrow(optionE1_tree),]=pmax(K-tree[nrow(tree),],0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      optionE1_tree[i,j]=(Q*optionE1_tree[i+1,j+1]+(1-Q)*optionE1_tree[i+1,j])*D
    }
  }
  return(optionE1_tree)
}
Bin_Op_Pri_val<-function(S,T,r,K,u,d,N,m){
  Q<-QM(r,delta_t=T/N,u,d,m)
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,m=m,u=u,d=d,K=K)
  return(list(Q=Q,stock=tree,option=option,priceE1=option[1,1]))
}
results1<-Bin_Op_Pri_val(S=30,T=8/12,r=0.12,K=34,u=1.08,d=0.9,N=2,m=4)
results1

#If you are not asked to use R to derive the evolution of the stock price 
#(and option prive)you can also write the following code to directly find the price
u<-1.08
d<-0.9
r<-0.12
S0<-30
M<-4
K<-34
delta_t<-4/12
S<-c(u*u*S0,u*d*S0,d*d*S0)
S
V<-c(max(K-S[1],0),max(K-S[2],0),max(K-S[3],0))
V
D<-(1+r/m)^(-m*delta_t)
D
Q<-((1/D)-d)/(u-d)
Q
Vu<-D*(Q*V[1]+(1-Q)*V[2])
Vu
Vd<-D*(Q*V[2]+(1-Q)*V[3])
Vd
V0<-D*(Q*Vu+(1-Q)*Vd)
V0
#alternatively you can use
V0<-D^2*(Q^2*V[1]+2*Q*(1-Q)*V[2]+(1-Q)^2*V[3])
V0


#Q2
#S0=20,u=1.1,d=0.95,r=0.08(r  annum  with  annual compounding)
#one year European put, K=20?
#Same question, risk neutral valuation

u<-1.1
d<-0.95
r<-0.08
S0<-20
m<-1
K<-20
T<-1
S<-c(u*S0,d*S0)
S
V<-c(max(K-S[1],0),max(K-S[2],0))
V
D<-(1+r/m)^(-m*T)
D
Del<-(V[1]-V[2])/(S[1]-S[2])
Del
BinOpP1<-Del*S0-(Del*S[1]-V[1])*D
BinOpP1
Q<-((1/D)-d)/(u-d)
Q
BinOpP2<-(Q*V[1]+(1-Q)*V[2])*D
BinOpP2


#Q3
#S0=40,Su=45,Sd=35,r=0.1(continuous),K=40
#initial value of European call?

S<-c(40,45,35)#(S0,uS0,dS0)
V<-c(max(S[2]-40,0),max(S[3]-40,0))
V
QM<-function(r,T){
  return((S[1]*exp(r*T)-S[3])/(S[2]-S[3]))}
Q<-QM(0.1,9/12)
Q
D1<-function(r,T){exp(-r*T)}
D<-D1(0.1,9/12)
D
BinOpP<-(Q*V[1]+(1-Q)*V[2])*D#initial value
BinOpP
Del<-(V[1]-V[2])/(S[2]-S[3])#compute the delta
Del
BinOpP1<-Del*S[1]-(Del*S[2]-V[1])*D#initial value
BinOpP1
#At time t= 0: The option seller receives the premium of C(0) =£3.76,
#and needs 40×0.5 =£20, to buy 0.5 units of the stock. 
#He then additionally borrows 20???3.76 =£16.24
#At time t=T= 4 months, there are two possible outcomes:
#(1)S1(T) =uS(0) =£45. The call option is exercised. 
#The option seller purchases some additional 0.5 units of the stock in the market at 0.5×45 =£22.5, 
#and sells his holding of now one stock to the option buyer at £40. 
#The excess of 40???22.5 =£17.5 is used to repay the loan at 16.24e0.1×3/4=£17.5, 
#so that the option seller is left with a net cash flow of 0 at time t=T.
#(2) S2(T) =dS(0) =£16. The option is not exercised. 
#The 0.5 units of the stock are sold at 0.5×35 =£17.5 
#and the proceeds are used to repay the loan in full. 
#The net payment of the option seller at time t=T is again 0.



#Q4
#S0=100,N=2,T=6/12,u=1.1,d=0.9,r1=0.08(quarterly compounding for first 3 months),
#r2=0.06(continuous for the remaining)
#initial value of European put,K=105

build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}

QM1<-function(r,delta_t,u,d,m){
  return(((1+r/m)^(m*delta_t)-d)/(u-d))
}
QM2<-function(r,delta_t,u,d){
  return((exp(r*delta_t)-d)/(u-d))
}
Bin_Op_Pri2<-function(tree,delta_t,r1,r2,m,u,d,K){
  D1<-(1+r1/m)^(-m*delta_t)
  D2<-exp(-r2*delta_t)
  Q1<-QM1(r1,delta_t,u,d,m)
  Q2<-QM2(r2,delta_t,u,d)
  Q<-c(Q1,Q2)
  D<-c(D1,D2)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),]=pmax(K-tree[nrow(tree),],0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option1_tree[i,j]=(Q[i]*option1_tree[i+1,j+1]+(1-Q[i])*option1_tree[i+1,j])*D[i]}#go back step by step
  }
  return(option1_tree)
}
Bin_Op_Pri_val<-function(S,T,r1,r2,K,u,d,N,m){
  Q<-c(QM1(r1,delta_t=T/N,u,d,m),QM2(r2,delta_t=T/N,u,d))
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r1=r1,r2=r2,m=m,u=u,d=d,K=K)
  return(list(Q=Q,stock=tree,option=option,price=option[1,1]))
}
results1<-Bin_Op_Pri_val(S=100,T=6/12,r1=0.08,r2=0.06,K=105,u=1.1,d=0.9,N=2,m=4)
results1
