
hamilton_filter=function(factor_value,p_s_initial,p_miu, autoreg_w,factor_miu,autoreg_sd)
{
  
  p_state=p_s_initial
  
  ###上一期与当期状态的联合概率（先列后行）,包括预测与更新的
  p_joint_forecast=matrix(0,nrow=2,ncol=2)
  p_joint_update=matrix(0,nrow=2,ncol=2)
  
  ###共同因子状态概率储存变量
  p_s_forecast=matrix(0,nrow=2,ncol=(ncol(factor_value)))
  p_s_update=matrix(0,nrow=2,ncol=(ncol(factor_value)))
  

  
  ###上一期到当期指标的概率
  f_y_y=0
  
  ###在当期预测状态概率下取到当期指标观测值的概率
  f_y_s=matrix(0,nrow=2,ncol=2)
  
  
  ##hamilton filter，所有时间状态的估计
  t=1
  for(t in 1:(ncol(factor_value)))
  {
    ###储存状态概率预测值与更新值
    p_s_forecast[,t]=rowSums(p_joint_forecast)
    p_s_update[,t]=p_state
    
    if(t==(ncol(factor_value)))
    {
      break
    }
    
    ###预测阶段
    p_joint_forecast[1,1]=p_miu[1,1]*p_state[1,]
    p_joint_forecast[2,1]=p_miu[2,1]*p_state[1,] 
    p_joint_forecast[1,2]=p_miu[1,2]*p_state[2,]
    p_joint_forecast[2,2]=p_miu[2,2]*p_state[2,]
    
    
    ###更新阶段
    f_y_s[1,1]=dnorm(factor_value[,t+1],mean=(1-autoreg_w)*factor_miu[1,]+autoreg_w*factor_value[,t],sd=autoreg_sd)
    f_y_s[2,1]=dnorm(factor_value[,t+1],mean=factor_miu[2,]-autoreg_w*factor_miu[1,]+autoreg_w*factor_value[,t],sd=autoreg_sd)
    f_y_s[1,2]=dnorm(factor_value[,t+1],mean=factor_miu[1,]-autoreg_w*factor_miu[2,]+autoreg_w*factor_value[,t],sd=autoreg_sd)
    f_y_s[2,2]=dnorm(factor_value[,t+1],mean=(1-autoreg_w)*factor_miu[2,]+autoreg_w*factor_value[,t],sd=autoreg_sd)
    
    f_y_y=p_joint_forecast[1,1]*f_y_s[1,1]+p_joint_forecast[1,2]*f_y_s[1,2]+p_joint_forecast[2,1]*f_y_s[2,1]+p_joint_forecast[2,2]*f_y_s[2,2]
    
    
    p_joint_update[1,1]=f_y_s[1,1]*p_joint_forecast[1,1]/f_y_y
    p_joint_update[2,1]=f_y_s[2,1]*p_joint_forecast[2,1]/f_y_y
    p_joint_update[1,2]=f_y_s[1,2]*p_joint_forecast[1,2]/f_y_y
    p_joint_update[2,2]=f_y_s[2,2]*p_joint_forecast[2,2]/f_y_y
    
    p_state[1,]=abs(p_joint_update[1,1]+p_joint_update[1,2])
    p_state[2,]=abs(p_joint_update[2,1]+p_joint_update[2,2])
  }
  
  state_variable_0=matrix(0,nrow=1,ncol=ncol(factor_value))
  
  ###生成最后一期状态变量
  state_variable_0[,(ncol(factor_value))]=sample(x=c(0,1),size = 1,prob = p_s_update[,(ncol(factor_value))])
  
  ##反向更新生成状态变量
  
  ###最终状态变量概率
  p_s_final=matrix(0,nrow=2,ncol=(ncol(factor_value)))
  
  t=(ncol(factor_value)-1)

  for(t in (ncol(factor_value)-1):1)
  {
    p_s_final[1,t]=abs(p_miu[1,state_variable_0[,t+1]+1]*p_s_update[1,t]/p_s_forecast[state_variable_0[,t+1]+1,t+1])
    p_s_final[2,t]=abs(p_miu[2,state_variable_0[,t+1]+1]*p_s_update[2,t]/p_s_forecast[state_variable_0[,t+1]+1,t+1])
    
    
    
    state_variable_0[,t]=sample(x=c(0,1),size = 1,prob = p_s_final[,t])
  }
  return(state_variable_0)
  
}













 
# factor_with_state=matrix(0,nrow=1,ncol=time)
# 
# miu=as.matrix(c(-1,1))
# 
# p_miu=cbind(c(0.8,0.2),c(0.2,0.8))
# state_value=matrix(0,nrow=1,ncol=time)
# 
# 
# for(t in 2:time)
# {
#   if(state_value[,t-1]==0)
#   {
#     state_value[,t]=sample(c(0,1),1,prob = p_miu[,1])
#   }else if (state_value[,t-1]==1)
#   {
#     state_value[,t]=sample(c(0,1),1,prob = p_miu[,2])
#   }
# }
# 
# factor_with_state[,1]=0.8*(0.1+1)+miu[state_value[,1]+1,]+rnorm(1,mean=0,sd=1)
# 
# for(t in 2:time)
# {
#   factor_with_state[,t]=miu[state_value[,t]+1,]+0.8*(factor_with_state[,t-1]-miu[state_value[,t-1]+1,])+rnorm(1,mean=0,sd=0.3)
# }
# 
# 
# Factor_with_state=hamilton_filter(factor_value = factor_with_state,p_s_initial =as.matrix(c(0.8,0.2)),p_miu = p_miu,autoreg_w = 0.8,factor_miu = miu,autoreg_sd = 0.3)
# 
# 
# plot(t(factor_with_state),type='l')
# lines(t(state_value),col='red')
# 
# lines(t(Factor_with_state),col='blue')
# plot(t(Factor_with_state),type='l')
# plot(t(state_value),type='l')


