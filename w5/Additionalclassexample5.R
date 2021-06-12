#Q1
#S0=20,K=22,u=1.2,d=0.8,N=2,T=8/12,r= 12.07% per annum compounded monthly,??t= 1/3.
#what is the price of European call option?

#way one
build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)
  }
binprice1<-build_stock_tree(S=20,1.2,0.8,N=2)
binprice1
Bin_Op_Pri2<-function(tree,delta_t,r,u,d,K,m){
  D=(1+r/m)^(-m*delta_t)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  Delta_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),] =pmax(tree[nrow(tree),]-K,0)
  Delta_tree[nrow(Delta_tree),]=0
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      Delta_tree[i,j]=(option1_tree[i+1,j+1]-option1_tree[i+1,j])/(tree[i,j]*(u-d))
      option1_tree[i,j]=Delta_tree[i,j]*tree[i,j]-(Delta_tree[i,j]*tree[i+1,j+1]-option1_tree[i+1,j+1])*D}}
  return(list(option1_tree,Delta_tree))}
Bin_Op_Prival<-function(S,T,r,K,u,d,N,m){
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,u=u,d=d,K=K,m=m)
  return(list(stock=tree,option=option,price=option[[1]][1,1]))}
results1<-Bin_Op_Prival(S=20,T=8/12,r=0.1207,u=1.2,d=0.8,K=22,N=2,m=12)
results1


#risk neutral valuation
build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
     tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
binprice1<-build_stock_tree(S=20,1.2,0.8,N=2)
binprice1
QM<-function(r,delta_t,u,d,m){
  return(((1+r/m)^(m*delta_t)-d)/(u-d))}
Bin_Op_Pri2<-function(tree,delta_t,r,u,d,K,m){
  D<-(1+r/m)^(-m*delta_t)
  Q<-QM(r,delta_t,u,d,m)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),]=pmax(tree[nrow(tree),]-K,0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option1_tree[i,j] = (Q*option1_tree[i+1,j+1]+(1-Q)*option1_tree[i+1,j])*D}
  }
  return(option1_tree)}
Bin_Op_Pri_val<-function(S,T,r,K,u,d,N,m){
  Q<-QM(r,delta_t=T/N,u,d,m)
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,u=u,d=d,K=K,m=m)
  return(list(Q=Q,stock=tree,option=option,price=option[1,1]))}
results1<-Bin_Op_Pri_val(S=20,T=8/12,r=0.1207,u=1.2,d=0.8,K=22,N=2,m=12)
results1


#Q2
#S0=30,N=2,T=4/12,u=1.08,d=0.9,r=0.05(per  annum  with  continuous  compounding)
#K=30,payoff=(max(K-ST,0))^2
#method1
build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)
}
binprice1<-build_stock_tree(S=30,1.08,0.9,N=2)
binprice1 
Bin_Op_Pri2<-function(tree,delta_t,r,u,d,K){
  D<-exp(-r*delta_t)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  Delta_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),] =(pmax(K-tree[nrow(tree),],0))^2
  Delta_tree[nrow(Delta_tree),]=0
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      Delta_tree[i,j]=(option1_tree[i+1,j+1]-option1_tree[i+1,j])/(tree[i,j]*(u-d))
      option1_tree[i,j]=Delta_tree[i,j]*tree[i,j]-(Delta_tree[i,j]*tree[i+1,j+1]-option1_tree[i+1,j+1])*D
    }
    }
  return(list(option1_tree,Delta_tree))
  }
Bin_Op_Prival<-function(S,T,r,K,u,d,N){
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,u=u,d=d,K=K)
  return(list(stock=tree,option=option,price=option[[1]][1,1]))}
results1<-Bin_Op_Prival(S=30,T=4/12,r=0.05,u=1.08,d=0.9,K=30,N=2)
results1


#risk neutral
build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
binprice1<-build_stock_tree(S=30,1.08,0.9,N=2)
binprice1
QM<-function(r,delta_t,u,d){
  return((exp(r*delta_t)-d)/(u-d))
  }
Bin_Op_Pri2<-function(tree,delta_t,r,u,d,K){
  D<-exp(-r*delta_t)
  Q<-QM(r,delta_t,u,d)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),]=(pmax(K-tree[nrow(tree),],0))^2
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option1_tree[i,j]=(Q*option1_tree[i+1,j+1]+(1-Q)*option1_tree[i+1,j])*D}#go back step by step
  }
  return(option1_tree)}
Bin_Op_Pri_val<-function(S,T,r,K,u,d,N){
  Q<-QM(r,delta_t=T/N,u,d)
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,u=u,d=d,K=K)
  return(list(Q=Q,stock=tree,option=option,price=option[1,1]))}
results1<-Bin_Op_Pri_val(S=30,T=4/12,r=0.05,u=1.08,d=0.9,K=30,N=2)
results1