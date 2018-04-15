
options(digits=16)

#通过Hamilton滤波更新共同因子所处状态


hamilton_filter=function(factor_value,p_s_initial,p_miu, autoreg_w,factor_miu,autoreg_var){
  
  p_state=p_s_initial
  
  ###上一期与当期状态的联合概率（先列后行）,包括预测与更新的
  p_joint_forecast=matrix(0,nrow=2,ncol=2)
  p_joint_update=matrix(0,nrow=2,ncol=2)
  
  ###共同因子状态概率储存变量
  p_s_forecast=matrix(0,nrow=2,ncol=(ncol(factor_value)+1))
  p_s_update=matrix(0,nrow=2,ncol=(ncol(factor_value)+1))
  
  
  
  ###上一期到当期指标的概率
  f_y_y=0
  
  ###在当期预测状态概率下取到当期指标观测值的概率
  f_y_s=matrix(0,nrow=2,ncol=2)
  
  
  ##hamilton filter，所有时间状态的估计
  t=1
  for(t in 1:(ncol(factor_value)+1))
  {
    ###储存状态概率预测值与更新值
    p_s_forecast[,t]=rowSums(p_joint_forecast)
    p_s_update[,t]=p_state
    
    if(t==(ncol(factor_value)+1))
    {
      break
    }
    
    ###预测阶段
    p_joint_forecast[,1]=p_miu[,1]*p_state[1,]
    p_joint_forecast[,2]=p_miu[,2]*p_state[2,]
    
    ###更新阶段
    f_y_s[1,1]=dnorm(factor_value[,t],mean=(1-autoreg_w)*factor_miu[1,],sd=autoreg_var)
    f_y_s[2,1]=dnorm(factor_value[,t],mean=factor_miu[2,]-autoreg_w*factor_miu[1,],sd=autoreg_var)
    f_y_s[1,2]=dnorm(factor_value[,t],mean=factor_miu[1,]-autoreg_w*factor_miu[2,],sd=autoreg_var)
    f_y_s[2,2]=dnorm(factor_value[,t],mean=(1-autoreg_w)*factor_miu[2,],sd=autoreg_var)
    
    f_y_y=p_joint_forecast[1,1]*f_y_s[1,1]+p_joint_forecast[1,2]*f_y_s[1,2]+p_joint_forecast[2,1]*f_y_s[2,1]+p_joint_forecast[2,2]*f_y_s[2,2]
    
    p_joint_update[1,1]=f_y_s[1,1]*p_joint_forecast[1,1]/f_y_y
    p_joint_update[2,1]=f_y_s[2,1]*p_joint_forecast[2,1]/f_y_y
    p_joint_update[1,2]=f_y_s[1,2]*p_joint_forecast[1,2]/f_y_y
    p_joint_update[2,2]=f_y_s[2,2]*p_joint_forecast[2,2]/f_y_y
    
    p_state[1,]=p_joint_update[1,1]+p_joint_update[1,2]
    p_state[2,]=p_joint_update[2,1]+p_joint_update[2,2]
    
    
    
  }
  
  
  
  ###生成最后一期状态变量
  state_variable[,(ncol(factor_value))]=sample(x=c(0,1),size = 1,prob = p_s_update[,(ncol(factor_value)+1)])
  
  ##反向更新生成状态变量
  
  ###最终状态变量概率
  p_s_final=matrix(0,nrow=2,ncol=(ncol(factor_value)))
  t=(ncol(factor_value))
  
  for(t in (ncol(factor_value)):1)
  {
    p_s_final[1,t]=p_miu[state_variable[,t+1]+1,1]*p_s_update[1,t]/p_s_forecast[state_variable[,t+1]+1,t+1]
    p_s_final[2,t]=p_miu[state_variable[,t+1]+1,2]*p_s_update[2,t]/p_s_forecast[state_variable[,t+1]+1,t+1]
    
    
    
    state_variable[,t]=sample(x=c(0,1),size = 1,prob = p_s_final[,t])
  }
  return(state_variable)
  
}


