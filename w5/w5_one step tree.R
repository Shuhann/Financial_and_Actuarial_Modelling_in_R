build_stock_tree<-function(S,u,d,N){
  tree=matrix(0, nrow = N+1 , ncol = N+1)
  for (i in 1:(N+1)){
    for (j in 1:i) {
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
binprice1<-build_stock_tree(S=20,1.2,0.8,N=1)
binprice1
  
Bin_Op_Pri2<-function(tree, delta_t, r, u, d, K){
  option1_tree=matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  Delta_tree=matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  option1_tree[nrow(option1_tree),]=pmax(tree[nrow(tree),]-K,0)
  Delta_tree[nrow(Delta_tree),]=0
  for (i in (nrow(tree)-1):1) {
    for (j in 1:i) {
  Delta_tree[i,j]=(option1_tree[i+1,j+1]-option1_tree[i+1,j])/(tree[i,j]*(u-d))
  option1_tree[i,j]=Delta_tree[i,j]*tree[i,j]-
    (Delta_tree[i,j]*tree[i+1,j+1]-option1_tree[i+1,j+1])*exp(-r*delta_t)
    }
}
  return(list(option1_tree,Delta_tree))
  } 
  Bin_Op_Pri_val<-function(S,T,r,K,u,d,type,N){
    tree<-build_stock_tree(S=S,u=u,d=d,N=N)
    option<-Bin_Op_Pri2(tree,delta_t=T/N,r=r,u=u,d=d,K=K)
  return(list(stock=tree,option=option,price=option[[1]][1,1]))
  }
  results1<-Bin_Op_Pri_val(S=20,T=1/3,r=0.09,u=1.2,d=0.8,K=22,N=1)
  results1
  
  
  
  #risk neutral valuation
  build_stock_tree<-function(S,u,d,N){
    tree=matrix(0,nrow=N+1,ncol=N+1)
    for(i in 1:(N+1)){
      for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
    return(tree)}
  Q_M<-function(r,delta_t,u,d){
      return((exp(r*delta_t)-d)/(u-d))
      }
  Bin_Op_Pri3<-function(tree,delta_t,r,u,d,K){
  q<-Q_M(r,delta_t,u,d)
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option1_tree[nrow(option1_tree),]=pmax(tree[nrow(tree),]-K,0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option1_tree[i,j]=(exp(-r*delta_t))*((1-q)*option1_tree[i+1,j]+
                                             q*option1_tree[i+1,j+1])}
    }
  return(option1_tree)
  }
  binomial_option<-function(T,r,K,S,u,d,N){
    q<-Q_M(r=r,delta_t=T/N,u=u,d=d)
    tree<-build_stock_tree(S=S,u=u,d=d,N=N)
    option<-Bin_Op_Pri3(tree,delta_t=T/N,r=r,u=u,d=d,K=K)
    return(list(q=q,stock=tree,option=option,price=option[1,1]))
    }
  results2<-binomial_option(T=1/3,r=0.09,S=20,u=1.2,d=0.8,K=22,N=1)
  results2