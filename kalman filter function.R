library(openxlsx)
library(actuar)
library(psych)
library(MASS)
library(mnormt)
library(mFilter)
library(tseries)
library(urca)
library(forecast)

options(digits=16)



#函数形式
kalman_filter=function(y,factor_value_initial,factor_var_initial,ob_w,ob_var,autoreg_intercept,autoreg_w,autoreg_var){
  
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
    
    factor_for_former=factor_for_former+factor_var_for_former%*%t(ob_w)%*%solve(update_parameter)%*%
      (y[,t]-ob_w%*%factor_for_former)
    
    
    factor_var_for_former=factor_var_for_former-factor_var_for_former%*%t(ob_w)%*%
      solve(update_parameter)%*%ob_w%*%factor_var_for_former
  }
  
  
  ##反向更新得到最终的共同因子估计
  
  update_factor_estimation=matrix(0,nrow=2,ncol=(ncol(y)))
  update_factor_var_estimation=matrix(0,nrow=2,ncol=2*(ncol(y)))
  
  update_update_parameter=matrix(0,nrow=1,ncol=1)
  update_update_parameter1=matrix(0,nrow=1,ncol=1)
  
  final_factor=matrix(0,nrow=1,ncol=ncol(y)+1)
  
  t=ncol(y)-1
  
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
      as.matrix(autoreg_w[1,])%*%autoreg_w[1,]%*%factor_var_estimation[,((t-1)*2+1):(t*2)]/as.numeric(update_update_parameter)
    
    ###生成共同因子
    final_factor[,t]=rnorm(1,mean=update_factor_estimation[1,t],sd=sqrt(update_factor_var_estimation[1,(t-1)*2+1]))
    
    return(final_factor)
  }
  
  
}



