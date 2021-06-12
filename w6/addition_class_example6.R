#Q1.
#S0=40,u=1.08,d=0.9,r1=0.05(perannum  with  weekly  compounding),m=52
#r2=0.06(er  annum  withcontinuous compounding for the remaining two-month period)
#payoff=(max(K-ST,0))^2,K=45
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
  option1_tree[nrow(option1_tree),]=(pmax(K-tree[nrow(tree),],0))^2
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
results1<-Bin_Op_Pri_val(S=40,T=4/12,r1=0.05,r2=0.06,K=45,u=1.08,d=0.9,N=2,m=52)
results1


#Q2
#S0=55,N=2,T=6/12,u=1.1,d=0.8,r=0.08(per annum with quarterly compounding),m=4
#initial value of a European put of K=50?
#initial value of a American put of K=50?Should it be exercised earlier?

build_stock_tree<-function(S,u,d,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
QM1<-function(r,m,delta_t,u,d){
  return(((1+r/m)^(m*delta_t)-d)/(u-d))}
Bin_Op_Pri2<-function(tree,delta_t,r,m,u,d,K){
  D1=(1+r/m)^(-m*delta_t)
  Q<-QM1(r,m,delta_t,u,d)
  optionE1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  optionA1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  optionAE_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  optionAE_tree[nrow(optionAE_tree),]=(pmax(K-tree[nrow(tree),],0))
  optionA1_tree[nrow(optionA1_tree),]=(pmax(K-tree[nrow(tree),],0))
  optionE1_tree[nrow(optionE1_tree),]=(pmax(K-tree[nrow(tree),],0))
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      optionAE_tree[i,j]=K-tree[i,j]
      optionA1_tree[i,j]=max(K-tree[i,j],(Q*optionA1_tree[i+1,j+1]+
                                            (1-Q)*optionA1_tree[i+1,j])*D1)
      optionE1_tree[i,j]=(Q*optionE1_tree[i+1,j+1]+
                            (1-Q)*optionE1_tree[i+1,j])*D1}}
  return(list(optionE1_tree,optionAE_tree,optionA1_tree))
  }
Bin_Op_Prival<-function(S,T,r,m,K,u,d,N){
  Q<-QM1(r,m,delta_t=T/N,u,d)
  tree<-build_stock_tree(S=S,u=u,d=d,N=N)
  option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,m=m,u=u,d=d,K=K)
  return(list(Q=Q,stock=tree,option=option,priceE=option[[1]][1,1],priceA=option[[3]][1,1]))}
results2<-Bin_Op_Prival(S=55,T=6/12,r=0.08,m=4,u=1.1,d=0.8,K=50,N=2)
results2