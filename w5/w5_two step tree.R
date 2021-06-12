build_stock_tree<-function(S,sigma,delta_t,N){
  tree=matrix(0,nrow=N+1,ncol=N+1)
  u=exp(sigma*sqrt(delta_t))
  d=exp(-sigma*sqrt(delta_t))
  for(i in 1:(N+1)){
    for(j in 1:i){
      tree[i,j]=S*u^(j-1)*d^((i-1)-(j-1))}}
  return(tree)
  }
binprice1<-build_stock_tree(S=80,sigma=0.1,delta_t=1/2,N=2)
binprice1

binprice2<-build_stock_tree(S=50,sigma=0.05,delta_t=1/4,N=5)
binprice2

K<-83
Termopvalue1<-pmax(binprice1[3,]-K,0)#return the maximum of two or more input vector
Termopvalue1 
Termopvalue2<-pmax(binprice2[6,]-50,0)
Termopvalue2 


Q_M<-function(r, delta_t, sigma){
  u=exp(sigma*sqrt(delta_t))
  d=exp(-sigma*sqrt(delta_t))
  return((exp(r*delta_t)-d)/(u-d))
} 
 

#Example3.2.3
Value_bin_Opt<-function(tree,sigma,delta_t,r,K,type){
q=Q_M(r,delta_t,sigma) 
option_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
if(type=='put'){
option_tree[nrow(option_tree),]=pmax(K-tree[nrow(tree),],0)
}
else{
option_tree[nrow(option_tree),]=pmax(tree[nrow(tree),]-K,0)
}
for(i in (nrow(tree)-1):1){
  for(j in 1:i){
    option_tree[i,j]=(exp(-r*delta_t))*((1-q)*option_tree[i+1,j]
                                        +q*option_tree[i+1,j+1])}
}
return(option_tree)#fill the option_tree structure in backward direction 
#starting with the values from the last row of the matrix
}
binomial_option<-function(type,sigma,T,r,K,S,N){
#join all the functions that we have described and 
#will return a tree with the values of the option at the different periods
q<-Q_M(r=r,delta_t=T/N,sigma=sigma)
tree<-build_stock_tree(S=S,sigma=sigma,delta_t=T/N,N=N)
option<-Value_bin_Opt(tree,sigma=sigma,delta_t=T/N,r=r,K=K,type=type)
return(list(q=q,stock=tree,option=option,price=option[1,1]))
}
results<-binomial_option(type='call',sigma=0.15,T=1,r=0.1,K=100,S=110,N=5)
results


#Write the option as afunction of the strike
strikes<-seq(80,160)
option_price_vary_K<-function(strike){option=
  binomial_option(type='call',sigma=0.15,T=1,r=0,K=strike,S=110,N=5)
return(option$price)
}
values<-sapply(strikes,option_price_vary_K)
#use to pass each value of the strikes array
#into the option_price_vary_strike function
data<-as.data.frame(list(strikes=strikes,values=values))
#Create a data frame with the Strikes prices and the option prices(values)from above
head(data)


#PLOT
ggplot(data=data) + geom_line(aes(x=strikes,y=values))+
  labs(title="CallValue",x="Strikes",y="Value")+
  geom_vline(xintercept=100,linetype="dotted", color="red",size=1.5)
#plot the relationship between option prices and strikes values.



#replicating portfolio
Bin_Op_Pri2<-function(tree,sigma,delta_t,r,K,type){
  option1_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  Delta_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  Theta_tree=matrix(0,nrow=nrow(tree),ncol=ncol(tree))
  if(type=='put'){
    option1_tree[nrow(option1_tree),]=pmax(K-tree[nrow(tree),],0)}
  else{option1_tree[nrow(option1_tree),]=pmax(tree[nrow(tree),]-K,0)}
  Delta_tree[nrow(Delta_tree),]=0
  Theta_tree[nrow(Delta_tree),]=0
  u=exp(sigma*sqrt(delta_t))
  d=exp(-sigma*sqrt(delta_t))
  for(i in (nrow(tree)-1):1){
    for(j in 1:i){
      Delta_tree[i,j]=(option1_tree[i+1,j+1]-option1_tree[i+1,j])/(tree[i,j]*(u-d))
      Theta_tree[i,j]=(-d*option1_tree[i+1,j+1]+u*option1_tree[i+1,j])*(exp(-r*delta_t))/((u-d))
      option1_tree[i,j]=Theta_tree[i,j]+Delta_tree[i,j]*tree[i,j]}
    }
  return(list(option1_tree,Delta_tree,Theta_tree))
  }
Bin_Op_Pri_val<-function(S,sigma,T,r,K,type,N){
  tree<-build_stock_tree(S=S,sigma=sigma,delta_t=T/N,N=N)
  option<-Bin_Op_Pri2(tree,sigma=sigma,delta_t=T/N,r=r,K=K,type=type)
  return(list(stock=tree,option=option,price=option[[1]][1,1]))
  }
results1<-Bin_Op_Pri_val(S=110,sigma=0.15,T=1,r=0.1,K=100,type='call',N=5)
results1