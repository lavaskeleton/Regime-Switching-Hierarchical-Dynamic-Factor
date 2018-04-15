options(digits=16)


#函数形式
kalman_filter=function(y,factor_value_initial,factor_var_initial,ob_w,ob_var,autoreg_intercept,autoreg_w,autoreg_var)
{
  
  ##上一期共同因子
  factor_for_former=factor_value_initial
  
  ##上一期共同因子估计误差协方差矩阵
  factor_var_for_former=factor_var_initial[(1:2),(1:2)]
  
  ##共同因子储存变量
  factor_estimation=matrix(0,nrow=2,ncol=(ncol(y)))
  
  factor_var_estimation=matrix(0,nrow=2,ncol=2*(ncol(y)))
  
  ##更新方程参数矩阵
  update_parameter=matrix(0,nrow=nrow(y),ncol=nrow(y)-1)
  
  t=1
  
  for(t in 1:(ncol(y)))
  {
    ##记录状态变量与状态估计误差
    factor_estimation[,t]=factor_for_former
    factor_var_estimation[,((t-1)*2+1):(t*2)]=factor_var_for_former
    
    if(t==(ncol(y)))
    {
      break
    }
    
    ##预测阶段
    factor_for_former=autoreg_intercept[,t]+autoreg_w%*%factor_for_former
    
    factor_var_for_former=autoreg_w%*%factor_var_for_former%*%t(autoreg_w)+autoreg_var
    
    ##更新阶段
    update_parameter=ob_w%*%factor_var_for_former%*%t(ob_w)+ob_var
    
    factor_for_former=factor_for_former+factor_var_for_former%*%t(ob_w)%*%
                      solve(update_parameter)%*%(y[,t]-ob_w%*%factor_for_former)
    
    
    factor_var_for_former=factor_var_for_former-factor_var_for_former%*%t(ob_w)%*%
                          solve(update_parameter)%*%ob_w%*%factor_var_for_former
  }
  
  
  ##反向更新得到最终的共同因子估计
  
  update_factor_estimation=matrix(0,nrow=2,ncol=(ncol(y)))
  update_factor_var_estimation=matrix(0,nrow=2,ncol=2*(ncol(y)))
  
  update_update_parameter=matrix(0,nrow=1,ncol=1)
  update_update_parameter1=matrix(0,nrow=1,ncol=1)
  
  final_factor=matrix(0,nrow=1,ncol=ncol(y)+1)
  
  t=ncol(y)
  
  update_factor_estimation[,t]=factor_estimation[,t]
  final_factor[,t+1]=as.numeric(factor_estimation[1,t])
  
  
  for(t in (ncol(y)):1)
  {
    ###更新阶段
    ####R
    update_update_parameter=autoreg_w[1,]%*%factor_var_estimation[,((t-1)*2+1):(t*2)]%*%as.matrix(autoreg_w[1,])+autoreg_var[1,1]
    ####η
    update_update_parameter1=(final_factor[,t+1]-autoreg_intercept[1,t]-as.numeric(autoreg_w[1,]%*%factor_estimation[,t]))
    
    update_factor_estimation[,t]=factor_estimation[,t]+factor_var_estimation[,((t-1)*2+1):(t*2)]%*%
                                 autoreg_w[1,]*as.numeric(update_update_parameter1)/as.numeric(update_update_parameter)
    
    update_factor_var_estimation[,((t-1)*2+1):(t*2)]=factor_var_estimation[,((t-1)*2+1):(t*2)]-factor_var_estimation[,((t-1)*2+1):(t*2)]%*%
                                                     as.matrix(autoreg_w[1,])%*%autoreg_w[1,]%*%factor_var_estimation[,((t-1)*2+1):(t*2)]/
                                                     as.numeric(update_update_parameter)
    
    ###生成共同因子
    #Sset.seed(t)
    final_factor[,t]=rnorm(1,mean=update_factor_estimation[1,t],sd=sqrt(update_factor_var_estimation[1,(t-1)*2+1]))
    
  }
  return(final_factor)
}













# factor_value=matrix(0,nrow=1,ncol=time)
# 
# factor_value[,1]=rnorm(1,mean=0,sd=1)
# 
# for(t in 2:time)
# {
#   factor_value[,t]=0.8*factor_value[,t-1]+rnorm(1,mean=0,sd=1)
# }
# 
# e_0=matrix(0,nrow=3,ncol=time)
# 
# for(i in 1:3)
# {
#   e_0[i,1]=rnorm(1,mean=0,sd=1)
# }
# 
# for(i in 1:3)
# {
#   for(t in 2:time)
#   {
#     e_0[i,t]=0.25*i*e_0[i,t-1]+rnorm(1,mean=0,sd=i/1.1)
#   }
# }
# 
# y=e_0
# 
# for(i in 1:3)
# {
#   y[i,]=2*i*factor_value+e_0[i,]
# }
# y_for_estimation=y[,-1]
# for(i in 1:3)
# {
#   for(t in 2:time)
#   {
#     y_for_estimation[i,t-1]=y[i,t]-0.25*i*y[i,t-1]
#   }
# }
# 
# ob_weight=matrix(0,nrow=3,ncol=2)
# for(i in 1:3)
# {
#   ob_weight[i,1]=2*i
#   ob_weight[i,2]=-0.25*i*2*i
# }
# 
# Factor=kalman_filter(y = y_for_estimation,factor_value_initial =as.matrix(c(factor_value[,2],factor_value[,1])) ,factor_var_initial = diag(c(0,0)),ob_w = ob_weight,ob_var =diag(c(1/1.1,2/1.1,3/1.1)),autoreg_intercept = matrix(0,nrow=2,ncol=time),autoreg_w =cbind(c(0.8,1),c(0,0)),autoreg_var =cbind(c(1,0),c(0,0)) )
# 
# plot(t(Factor),type='l')
# lines(t(factor_value),col='red')
