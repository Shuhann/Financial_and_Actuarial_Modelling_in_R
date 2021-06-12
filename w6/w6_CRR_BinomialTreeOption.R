CRRBinomialTreeOption(TypeFlag="ce",S=110,X=100,Time=1,r=0.1,b=0.1,sigma=0.15,n=5)
#calculatethepriceofaEuropeancall
CRRTree=BinomialTreeOption(TypeFlag="ce",S=110,X=100,Time=1,r=0.1,b=0.1, sigma=0.15, n=5)
BinomialTreePlot(CRRTree,dy=1,cex=0.8,ylim=c(-10,10),xlab='n',ylab='OptionValue')
title(main="OptionTree")


build_stock_tree<-function(S,u,d,delta_t,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)}
Q_M<-function(r,delta_t,u,d){
  return((exp(r*delta_t)-d)/(u-d))
  }
Value_bin_Opt2<-function(tree,u,d,delta_t,r,K,k){
  q=Q_M(r,delta_t,u,d)
  option_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option_tree[nrow(option_tree),]=pmax((tree[nrow(tree),])^k-K,0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option_tree[i,j]=(exp(-r*delta_t))*((1-q)*option_tree[i+1,j]+
                                            q*option_tree[i+1,j+1])}
    }
  return(option_tree)
  }
binomial_option1<-function(u,d,k,T,r,K,S,N){
  q=Q_M(r=r,delta_t=T/N,u=u,d=d)
  tree<-build_stock_tree(S=S,u=u,d=d,delta_t=T/N,N=N)
  option<-Value_bin_Opt2(tree,u=u,d=d,delta_t=T/N,r=r,K=K,k=k)
  return(list(q=q,stock=tree,option=option,price=option[1,1]))
  }
results<-binomial_option1(u=1.08,d=0.9,k=2,T=1/3,r=0.1,K=30,S=30,N=2)
results

