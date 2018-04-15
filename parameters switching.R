#底层模型变量与参数定义与转换

##量测方程
###y
y_b[,t]=(y[,t]-Alpha*Beta*G[,t])-lambda_1*(y[,t-1]-Alpha*Beta*G[,t-1])

###量测系数
ob_w_1=cbind(Alpha, -lambda_1*Alpha)

###factor
b_factor=rbind(e_2[,2],e_2[,1])

###量测扰动项
ob_sigma2_1=b_noise_sigma^2

##转移方程
###截距项
autoreg_intercept_1=rbind(miu_2[state_2[,t]+1,]-lambda_2*miu_2[state_2[,t-1]+1,],0)

###转移系数
autoreg_w_1=cbind(rbind(lambda_2,1),matrix(0,nrow = 2,ncol=1))

###转移扰动项
autoreg_sigma2_1=as.matrix(c(m_noise_sigma^2,0),nrow=2,ncol=1)

##系数估计用变量形式转换
###alpha
y_for_alpha=y[,t]-lambda_1*y[,t-1]
x_for_alpha=Beta*G[,t]+e_2[,t]-lambda_1*(Beta*G[,t-1]+e_2[,t-1])

###lambda_1
y_for_lambda_1=y[,t]-alpha*Beta*G[,t]-alpha*e_2[,t]
x_for_lambda_1=y[,t]-alpha*Beta*G[,t]-alpha*e_2[,t]

###sigma2_1
parameter_for_sigma2_1=y_for_lambda_1-lambda_1*x_for_lambda_1

###lambda_2
y_for_lambda_2=e_2[,t]-miu_2[state_2[,t]+1,]
x_for_lambda_2=e_2[,t-1]-miu_2[state_2[,t-1]+1,]

###miu_2
y_for_miu_2=e_2[,t]-lambda_2*e_2[,t-1]
x_for_miu_2=matrix(c((1-state_2[,t])-lambda_2*(1-state_2[,t-1]),state_2[,t]-lambda_2*state_2[,t-1]),ncol=2)





#中间层模型变量与参数转换

##量测方程
###y
y_m[,t]=(H[,t]-miu_2[state_2[,t]+1,]-Beta*gamma*Factor[,t])-lambda_2*(H[,t-1]-miu_2[state_2[,t-1]+1,]-Beta*gamma*Factor[,t-1])

###量测系数
ob_w_1=cbind(Beta, -lambda_2*Beta)

###factor
m_factor=rbind(e_3[,t],e_3[,t-1])

###量测扰动项
ob_sigma2_2=m_noise_sigma^2

##转移方程
###截距项
autoreg_intercept_2=rbind(miu_3[state_3[,t]+1,]-lambda_3*miu_3[state_3[,t-1]+1,],0)

###转移系数
autoreg_w_2=cbind(rbind(lambda_3,1),matrix(0,nrow = 2,ncol=1))

###转移扰动项
autoreg_sigma2_2=as.matrix(c(u_noise_sigma^2,0),nrow=2,ncol=1)

###beta
y_for_beta=H[,t]-miu_2[state_2[,t]+1,]-lambda_2*(H[,t-1]-miu_2[state_2[,t-1]+1,])
x_for_beta=theta*Factor[,t]+e_3[,t]-lambda_2*(theta*Factor[,t-1]+e_3[,t-1])

###sigma2_2
parameter_for_sigma2_2=H[,t]-miu_2[state_2[,t]+1,]-Beta*theta*Factor[,t]-Beta*e_3[,t]-
                      lambda_2*(H[,t-1]-miu_2[state_2[,t-1]+1,]-Beta*theta*Factor[,t-1]-Beta*e_3[,t-1])

###lambda_3
y_for_lambda_3=e_3[,t]-miu_3[state_3[,t]+1,]
x_for_lambda_3=e_3[,t-1]-miu_3[state_3[,t-1]+1,]

###miu_3
y_for_miu_3=e_3[,t]-lambda_3*e_3[,t-1]
x_for_miu_3=matrix(c((1-state_3[,t])-lambda_3*(1-state_3[,t-1]),state_3[,t]-lambda_3*state_3[,t-1]),ncol=2)






#上层模型变量与参数转换

##量测方程
###y
y_u[,t]=(G[,t]-miu_3[state_3[,t]+1,])-lambda_3*(G[,t-1]-miu_3[state_3[,t-1]+1,])

###量测系数
ob_w_3=cbind(gamma, -lambda_3*gamma)

###factor
u_factor=rbind(Factor[,t],Factor[,t-1])

###量测扰动项
ob_sigma2_3=u_noise_sigma^2

##转移方程
###截距项
autoreg_intercept_3=rbind(miu[state[,t]+1,]-lambda*miu[state[,t-1]+1,],0)

###转移系数
autoreg_w_3=cbind(rbind(lambda,1),matrix(0,nrow = 2,ncol=1))

###转移扰动项
autoreg_sigma2_3=as.matrix(c(noise_sigma^2,0),nrow=2,ncol=1)

###theta
y_for_theta=G[,t]-miu_3[state_3[,t]+1,]-lambda_3*(G[,t-1]-miu_3[state_3[,t-1]+1,])
x_for_theta=Factor[,t]-lambda_3*Factor[,t-1]

###sigma2_3
parameter_for_sigma2_3=G[,t]-miu_3[state_3[,t]+1,]-theta*Factor[,t]-lambda_3*(G[,t-1]-miu_3[state_3[,t-1]+1,]-theta*Factor[,t-1])

###lambda
y_for_lambda=Factor[,t]-miu[state_F[,t]+1,]
x_for_lambda=Factor[,t-1]-miu[state_F[,t-1]+1,]

###miu
y_for_miu=Factor[,t]-lambda*Factor[,t-1]
x_for_miu=matrix(c((1-state_F[,t])-lambda*(1-state_F[,t-1]),state_F[,t]-lambda*state_F[,t-1]),ncol=2)

