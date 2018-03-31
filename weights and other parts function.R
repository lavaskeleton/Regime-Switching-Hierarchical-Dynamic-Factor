options(digits=16)

weight_estimation=function(y,x,sigma){
  
  miu=matrix(0,nrow=nrow(y),ncol=1)
  sigma2=matrix(1,nrow=nrow(y),ncol=1)
  
  
  i=1
  for(i in 1:nrow(y))
  {
    miu[i,]=solve(1+(t(x[i,])%*%x[i,])/sigma[i,]^2)%*%((t(x[i,])%*%y[i,])/sigma[i,]^2)
    sigma2[i,]=solve(1+(t(x[i,])%*%x[i,])/sigma[i,]^2)
    
    weights[i,]=rnorm(1,mean=miu[i,],sd=sqrt(sigma2[i,]))
    
    return(weights)
  }
}

sigma_estimation=function(y,autoreg_w,autoreg_residual){
  
  for(i in 1:nrow(y))
  {
    sigma2[i,]=rinvgamma(1,shape = (2+ncol(y))/2, scale = 1+0.5*t(autoreg_residual)%*%(autoreg_residual))
  }
}

miu_estimation=function(y,x,sigma){
  
  miu=matrix(0,nrow=2,ncol=1)
  sigma2=matrix(1,nrow=2,ncol=2)
  
  

    miu=solve(diag(2)+(t(x)%*%x)/sigma[i,]^2)%*%((t(x)%*%y)/sigma[i,]^2)
    sigma2[i,]=solve(diag(2)+(t(x)%*%x)/sigma[i,]^2)
    
    factor_miu=as.matrix(mvrnorm(1,mu=miu,Sigma =sigma2))
    
    
    return(factor_miu)
  
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
  
  
  p_miu[1,1]=rbeta(1, shape1 = 8+counter_0_0, shape2 = 2+counter_0_1)
  
  p_miu[2,2]=rbeta(1, shape1 = 8+counter_1_1, shape2 = 2+counter_1_0)
  
  p_miu[2,1]=1-p_miu[1,1]
  
  p_miu[1,2]=1-p_miu[2,2]
  
  return(p_miu)
}



