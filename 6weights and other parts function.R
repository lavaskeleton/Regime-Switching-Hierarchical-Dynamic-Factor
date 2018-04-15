options(digits=16)



weight_estimation=function(y,x,sigma,prior_miu,prior_sigma){
#weight_estimation=function(y,x,sigma){
  
  miu=matrix(0,nrow=nrow(y),ncol=1)
  sigma2=matrix(1,nrow=nrow(y),ncol=1)
  
  weights=matrix(0,nrow=nrow(y),ncol=1)
  i=1
  for(i in 1:nrow(y))
  {
    miu[i,]=solve(1/(prior_sigma)^2+(t(x[i,])%*%as.matrix(x[i,])/sigma[i,]^2))%*%(prior_miu/(prior_sigma)^2+(t(x[i,])%*%as.matrix(y[i,])/sigma[i,]^2))
    sigma2[i,]=solve(1/(prior_sigma)^2+(t(x[i,])%*%as.matrix(x[i,])/sigma[i,]^2))
    
    # miu[i,]=solve(1+(t(x[i,])%*%as.matrix(x[i,])/sigma[i,]^2))%*%((t(x[i,])%*%as.matrix(y[i,])/sigma[i,]^2))
    # sigma2[i,]=solve(1+(t(x[i,])%*%as.matrix(x[i,])/sigma[i,]^2))
    
    weights[i,]=rnorm(1,mean=miu[i,],sd=sqrt(sigma2[i,]))
    
    
  }
  #return(list(weights,miu,sigma2))
  return(list(weights))
  
}




sigma2_estimation=function(y,autoreg_residual,prior_v,prior_f){
  
  sigma2=matrix(0,nrow=nrow(y),ncol=1)
  for(i in 1:nrow(y))
  {
    sigma2[i,]=rinvgamma(1,shape =(prior_v+ncol(y))/2, scale = (prior_f+t(autoreg_residual[i,])%*%(autoreg_residual[i,]))/2)
  }
  return(sigma2)
}





miu_estimation=function(y,x,sigma,prior_miu,prior_sigma){
#miu_estimation=function(y,x,sigma){
  
  miu=matrix(0,nrow=2,ncol=1)
  sigma2=matrix(1,nrow=2,ncol=2)
  
  miu=solve(solve(prior_sigma^2)+(t(x)%*%x)/sigma^2)%*%(solve(prior_sigma^2)%*%prior_miu+(t(x)%*%y)/sigma^2)
  sigma2=solve(solve(prior_sigma^2)+(t(x)%*%x)/sigma^2)
  # miu=solve(diag(2)+(t(x)%*%x)/sigma^2)%*%((t(x)%*%y)/sigma^2)
  # sigma2=solve(diag(2)+(t(x)%*%x)/sigma^2)
  
  factor_miu=matrix(0,nrow=2,ncol=1)
  factor_miu[1,]=rnorm(1,mean = miu[1,],sd = sigma2[1,1])
  factor_miu[2,]=factor_miu[1,]+abs(rnorm(1,mean = miu[2,],sd = sigma2[2,2]))
  
  #as.matrix(mvrnorm(1,mu=miu,Sigma =sigma2))
  
    
  #return(list(factor_miu,miu,sigma2))
  return(list(factor_miu))
  
}





transition_p=function(state_value){
  
  p_miu=matrix(0,nrow=2,ncol=2)
  #转移概率矩阵更新
  
  counter_0_0=0
  counter_0_1=0
  counter_1_0=0
  counter_1_1=0
  
  
  for(t in 1:(ncol(state_value)-1))
  {
    if(state_value[,t]==0&&state_value[,t+1]==0)
    {  
      counter_0_0=counter_0_0+1
    }else if(state_value[,t]==0&&state_value[,t+1]==1)
    {
      counter_0_1=counter_0_1+1
    }else if(state_value[,t]==1&&state_value[,t+1]==0)
    {
      counter_1_0=counter_1_0+1
    }else if (state_value[,t]==1&&state_value[,t+1]==1)
    {
      counter_1_1=counter_1_1+1
    }
  }
  
  
  p_miu[1,1]=rbeta(1, shape1 = 1+counter_0_0, shape2 = 1+counter_0_1)
  
  p_miu[2,2]=rbeta(1, shape1 = 1+counter_1_1, shape2 = 1+counter_1_0)
  
  p_miu[2,1]=1-p_miu[1,1]
  
  p_miu[1,2]=1-p_miu[2,2]
  
  return(p_miu)
}


