#Example3.2.6-American PUT&continuous compounding&risk neutral
#first method
#CRR:CRRBinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, Time, r, b, sigma, n, title = NULL, description = NULL)
#cex:a numerical value giving the amount by which the plotting text and symbols should be scaled relative to the default.
#dx, dy:numerical values, an offset fine tuning for the placement of the option values in the option tree.

CRRBinomialTreeOption(TypeFlag="pa",S=110,X=100,Time=1,r=0.1,b=0.1,sigma=0.15,n=5)

CRRTreeA=BinomialTreeOption(TypeFlag="pa",S=110,X=100,Time=1,r=0.1,b=0.1,sigma=0.15,n=5)
BinomialTreePlot(CRRTreeA,dy=1,cex=0.8,ylim=c(-10,10),xlab='n',ylab='OptionValue')
title(main="AmericanOptionTree")


build_stock_tree<-function(S,u,d,delta_t,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}
    }
  return(tree)
  }
Q_M<-function(r,delta_t,u,d){
  return((exp(r*delta_t)-d)/(u-d))
  }
Value_bin_OptA2<-function(tree,u,d,delta_t,r,K){
  q=Q_M(r,delta_t,u,d)
  option_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option_tree[nrow(option_tree),]=pmax(K-tree[nrow(tree),],0)
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option_tree[i,j]=max(K-tree[i,j],(exp(-r*delta_t))*((1-q)*option_tree[i+1,j]+
                                                            q*option_tree[i+1,j+1]))}
    }
  return(option_tree)
  }
binomial_optionA1<-function(u,d,T,r,K,S,N){
  q=Q_M(r=r,delta_t=T/N,u=u,d=d)
  tree<-build_stock_tree(S=S,u=u,d=d,delta_t=T/N,N=N)
  option<-Value_bin_OptA2(tree,u=u,d=d,delta_t=T/N,r=r,K=K)
  return(list(q=q,stock=tree,option=option,price=option[1,1]))
  }
resultsA1<-binomial_optionA1(u=1.0693832,d=0.935118487,T=1,r=0.1,K=100,S=110,N=5)
resultsA1


#Example3.2.7
#S(0)=40,N=3, T=15/12,u=1.1, d=0.95
#Q1:European,payoff=[max(K-S(T),0)]^3,K=44
#Q2:American,others same
#Q3:European, payoff=min(S(T)^2,K)

#Q1
build_stock_tree<-function(S,u,d,delta_t,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  for(i in 1:(N+1)){
    for(j in 1:i){tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}
    }
  return(tree)
  }
Q_M<-function(r,delta_t,u,d){
  return((exp(r*delta_t)-d)/(u-d))
  }
Value_bin_OptE2<-function(tree,u,d,delta_t,r,K){
  q=Q_M(r,delta_t,u,d)
  option_treeE=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option_treeE[nrow(option_treeE),]=(pmax(K-tree[nrow(tree),],0))^3
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option_treeE[i,j]=(exp(-r*delta_t))*((1-q)*option_treeE[i+1,j]++q*option_treeE[i+1,j+1])}
    }
  return(option_treeE)
  }
binomial_optionE<-function(u,d,T,r,K,S,N){
  q=Q_M(r=r,delta_t=T/N,u=u,d=d)
  tree<-build_stock_tree(S=S,u=u,d=d,delta_t=T/N,N=N)
  optionE<-Value_bin_OptE2(tree,u=u,d=d,delta_t=T/N,r=r,K=K)
  return(list(q=q,stock=tree,option=optionE,price=optionE[1,1]))
  }
resultsE1<-binomial_optionE(u=1.1,d=0.95,T=15/12,r=0.06,K=44,S=40,N=3)
resultsE1



#Q2
Value_bin_OptA2<-function(tree,u,d,delta_t,r,K){
  q=Q_M(r,delta_t,u,d)
  option_treeA=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option_treeA[nrow(option_treeA),]=(pmax(K-tree[nrow(tree),],0))^3
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      option_treeA[i,j]=max((max(K-tree[i,j],0))^3,
                            (exp(-r*delta_t))*((1-q)*option_treeA[i+1,j]+q*option_treeA[i+1,j+1]))}
    }
  return(option_treeA)
  }
binomial_optionA1<-function(u,d,T,r,K,S,N){
  q=Q_M(r=r,delta_t=T/N,u=u,d=d)
  tree<-build_stock_tree(S=S,u=u,d=d,delta_t=T/N,N=N)
  optionA<-Value_bin_OptA2(tree,u=u,d=d,delta_t=T/N,r=r,K=K)
  return(list(q=q,stock=tree,option=optionA,price=optionA[1,1]))
  }
resultsA2<-binomial_optionA1(u=1.1,d=0.95,T=15/12,r=0.06,K=44,S=40,N=3)
resultsA2

#Q3
Value_bin_OptE3<-function(tree,u,d,delta_t,r,K){
  q=Q_M(r,delta_t,u,d)
  option_treeE1=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  option_treeE1[nrow(option_treeE1),]=pmin((tree[nrow(tree),])^2,K)
  for (i in (nrow(tree)-1):1){
    for(j in 1:i) {
      option_treeE1[i,j]=(exp(-r*delta_t))*((1-q)*option_treeE1[i+1,j]+
                                              q*option_treeE1[i+1,j+1])}
    }
  return(option_treeE1)
  }
binomial_optionE<-function(u,d,T,r,K,S,N){
  q=Q_M(r=r, delta_t=T/N, u=u, d=d)
  tree<-build_stock_tree(S=S,u=u,d=d,delta_t=T/N,N=N)
  optionE1<-Value_bin_OptE3(tree, u=u, d=d, delta_t=T/N, r=r, K=K)
  return(list(q=q, stock=tree, option=optionE1, price=optionE1[1,1]))
  }
resultsE3<-binomial_optionE(u=1.1,d=0.95,T=15/12,r=0.06,K=2000,S=40,N=3)
resultsE3