#三层因子估计误差初始值

p_0=matrix(c(1,0,0,1),nrow=2,ncol=2)

b_factor_var_initial=rep(list(0),block)

for(i in 1:block)
{
  b_factor_var_initial[[i]]=autoreg_w_1[[i]]%*%p_0%*%t(autoreg_w_1[[i]])+autoreg_sigma2_1[[i]]
}


m_factor_var_initial=rep(list(0),area)
for(i in 1:area)
{
  m_factor_var_initial[[i]]=autoreg_w_2[[i]]%*%p_0%*%t(autoreg_w_2[[i]])+autoreg_sigma2_2[[i]]
}

u_factor_var_initial=list(0)
u_factor_var_initial=autoreg_w_3%*%p_0%*%t(autoreg_w_3)+autoreg_sigma2_3


#三层状态空间模型共同因子的kalman滤波估计
for(i in 1:block)
{
  
  e_2[i,]=kalman_filter(y=y_b[[i]],factor_value_initial=as.matrix(b_factor_initial[,i]),factor_var_initial=b_factor_var_initial[[i]],
                        ob_w=ob_w_1[[i]],ob_var= ob_sigma2_1[[i]],autoreg_intercept = autoreg_intercept_1[[i]],
                        autoreg_w = autoreg_w_1[[i]],autoreg_var = autoreg_sigma2_1[[i]])
}



for(i in 1:area)
{
  
  e_3[i,]=kalman_filter(y=y_m[[i]],factor_value_initial=as.matrix(m_factor_initial[,i]),factor_var_initial=m_factor_var_initial[[i]],
                        ob_w=ob_w_2[[i]],ob_var= ob_sigma2_2[[i]],autoreg_intercept = autoreg_intercept_2[[i]],
                        autoreg_w = autoreg_w_1[[i]],autoreg_var = autoreg_sigma2_2[[i]])
}


Factor=kalman_filter(y=y_u,factor_value_initial=u_factor_initial,factor_var_initial=u_factor_var_initial,
                     ob_w=ob_w_3,ob_var= ob_sigma2_3,autoreg_intercept = matrix(0,nrow=2,ncol=time),
                     autoreg_w = autoreg_w_3,autoreg_var = autoreg_sigma2_3)

#autoreg_intercept_3