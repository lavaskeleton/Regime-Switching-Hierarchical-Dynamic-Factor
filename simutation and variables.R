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
#生成三层随机变量
set.seed(666)
true_miu=matrix(c(-1,1),nrow=2,ncol=1)
true_state=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.8,0.2),replace = TRUE)))
true_lambda=0.8
true_Factor=matrix(0,nrow=1,ncol=60)
true_Factor[,1]=0.1
for(t in 2:60)
{
  true_Factor[,t]=true_miu[true_state[,t]+1,]+true_lambda*(true_Factor[,t-1]-true_miu[true_state[,t-1]+1,])+rnorm(n = 1,mean=0,sd=1)
}


true_miu_3=matrix(0,nrow=2,ncol=2)
true_miu_3[,1]=c(-2,2)
true_miu_3[,2]=c(-3,3)

true_state_3=matrix(0,nrow=2,ncol=60)
true_state_3[1,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.7,0.3),replace = TRUE)))
true_state_3[2,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.7,0.3),replace = TRUE)))

true_lambda_3=as.matrix(c(0.7,0.6))

true_e_3=matrix(0,nrow=2,ncol=60)
true_e_3[1,1]=0.1
true_e_3[2,1]=0.2
for(t in 2:60)
{
  true_e_3[1,t]=true_miu_3[true_state_3[1,t]+1,1]+true_lambda_3[1,]*(true_e_3[1,t-1]-true_miu_3[true_state_3[1,t-1]+1,1])+rnorm(n = 1,mean=0,sd=2)
  true_e_3[2,t]=true_miu_3[true_state_3[2,t]+1,2]+true_lambda_3[2,]*(true_e_3[2,t-1]-true_miu_3[true_state_3[2,t-1]+1,2])+rnorm(n = 1,mean=0,sd=3)
}


true_theta=matrix(0,nrow=2,ncol=1)
true_theta[1,1]=2
true_theta[2,1]=3

true_G=matrix(0,nrow=2,ncol=60)

for(t in 1:60)
{
  true_G[1,t]=true_theta[1,1]*true_Factor[,t]+true_e_3[1,t]
  true_G[2,t]=true_theta[2,1]*true_Factor[,t]+true_e_3[2,t]
}


true_miu_2=matrix(0,nrow=2,ncol=4)
true_miu_2[,1]=c(-2.5,2.5)
true_miu_2[,2]=c(-3.5,3.5)
true_miu_2[,3]=c(-1.5,1.5)
true_miu_2[,4]=c(-0.5,0.5)

true_state_2=matrix(0,nrow=4,ncol=60)
for(i in 1:4)
{
  true_state_2[i,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.75,0.25),replace = TRUE)))
}

true_lambda_2=as.matrix(c(0.75,0.65,0.55,0.45))

true_e_2=matrix(0,nrow=4,ncol=60)
true_e_2[1,1]=0.15
true_e_2[2,1]=0.25
true_e_2[3,1]=0.3
true_e_2[4,1]=0.4

for(i in 1:4)
{
  for(t in 2:60)
  {
    true_e_2[i,t]=true_miu_2[true_state_2[i,t]+1,1]+true_lambda_2[i,]*(true_e_2[i,t-1]-true_miu_2[true_state_2[i,t-1]+1,1])+rnorm(n = 1,mean=0,sd=true_miu_2[2,i])
  }
}


true_Beta=matrix(0,nrow=4,ncol=1)
true_Beta[1,1]=1.25
true_Beta[2,1]=2.25
true_Beta[3,1]=1.75
true_Beta[4,1]=0.75


true_H=matrix(0,nrow=4,ncol=60)


for(t in 1:60)
{
  true_H[1,t]=true_Beta[1,1]*true_G[1,t]+true_e_2[1,t]
  true_H[2,t]=true_Beta[2,1]*true_G[1,t]+true_e_2[2,t]
  true_H[3,t]=true_Beta[3,1]*true_G[2,t]+true_e_2[3,t]
  true_H[4,t]=true_Beta[4,1]*true_G[2,t]+true_e_2[4,t]
}






true_lambda_1=as.matrix(c(0.72,0.53,0.44,0.12,0.22,0.32,0.5,0.8))

true_e_1=matrix(0,nrow=8,ncol=60)
true_e_1[1,1]=0.15
true_e_1[2,1]=0.25
true_e_1[3,1]=0.3
true_e_1[4,1]=0.4
true_e_1[5,1]=0.5
true_e_1[6,1]=0.55
true_e_1[7,1]=0.45
true_e_1[8,1]=0.31

for(i in 1:8)
{
  for(t in 2:60)
  {
    true_e_1[i,t]=true_lambda_1[i,]*true_e_1[i,t-1]+rnorm(n = 1,mean=0,sd=i/2+0.6)
  }
}


true_alpha=matrix(0,nrow=8,ncol=1)
true_alpha[1,1]=1.25
true_alpha[2,1]=2.25
true_alpha[3,1]=1.75
true_alpha[4,1]=3.2
true_alpha[5,1]=4.8
true_alpha[6,1]=2.1
true_alpha[7,1]=2.65
true_alpha[8,1]=0.75

true_y=matrix(0,nrow=8,ncol=60)


for(t in 1:60)
{
  true_y[1,t]=true_alpha[1,1]*true_H[1,t]+true_e_1[1,t]
  true_y[2,t]=true_alpha[2,1]*true_H[2,t]+true_e_1[2,t]
  true_y[3,t]=true_alpha[3,1]*true_H[3,t]+true_e_1[3,t]
  true_y[4,t]=true_alpha[4,1]*true_H[4,t]+true_e_1[4,t]
  true_y[5,t]=true_alpha[5,1]*true_H[1,t]+true_e_1[5,t]
  true_y[6,t]=true_alpha[6,1]*true_H[2,t]+true_e_1[6,t]
  true_y[7,t]=true_alpha[7,1]*true_H[3,t]+true_e_1[7,t]
  true_y[8,t]=true_alpha[8,1]*true_H[4,t]+true_e_1[8,t]
}






#主成分变量作为变量初始值

##主成分分析（每一列为变量，从变量中提取）

rotated_principal_object=principal(t(true_y),nfactors=1,rotate="oblimin")

#fa.parallel(true_y,fa="both",n.iter=100,main="Scree plots with parallel analysis")  


##获得主成分变量


weights_for_principal=t(as.matrix(rotated_principal_object$weights))
principal_variable=matrix(0,nrow=ncol(true_y),ncol=1)

i=1
j=1

for(i in 1:ncol(true_y))
{
  for(j in 1:nrow(true_y))
  {
    principal_variable[i,]=principal_variable[i,]+weights_for_principal[,j]*true_y[j,i]
  }
}



#通过OLS和自回归得到模型系数初始值

##观测方程系数初始值，为各个指标对主成份的OLS回归系数
observe_weights=matrix(0,nrow=nrow(true_y),ncol=1)

# j=1
# for(j in 1:ncol(true_y))
# {
#   observe_weights[j,]=rnorm(1,mean=0,sd=1)
# }

reg_model=rep(list(0),nrow(true_y))


j=1
for(j in 1:nrow(true_y))
{
  reg_model[[j]]=lm(true_y[,j]~principal_variable)
  observe_weights[j,]=reg_model[[j]]$coefficients[2]
}



#状态方程系数初始值与特质波动系数初始值，为主成份和特质波动的自回归系数

##特质波动自回归系数初始值
idiosyncracy_autoreg_weights=matrix(0,nrow=ncol(true_y),ncol=1)
#idiosyncracy_autoreg_weights=mvrnorm(ncol(true_y),mu = c(rep(0,10)),Sigma = diag(c(rep(1,10))))

idiosyncracy_autoreg_model=rep(list(0),ncol(true_y))
idiosyncracy_matrix=matrix(0,nrow=nrow(true_y),ncol = ncol(true_y))

j=1
for(j in 1:ncol(true_y))
{
  idiosyncracy_matrix[,j]=reg_model[[j]]$residuals
}

j=1
for(j in 1:ncol(true_y))
{
  idiosyncracy_autoreg_model[[j]]=arma(idiosyncracy_matrix[,j],order = c(1,0))
  idiosyncracy_autoreg_weights[j,]=idiosyncracy_autoreg_model[[j]]$coef[1]
}

##主成份自回归系数，也就是状态方程系数初始值

factor_autoreg_model=arma(principal_variable,order = c(1,0))

factor_autoreg_weight=factor_autoreg_model$coef[1]



#factor_autoreg_weight=rnorm(1,mean = 0,sd=1)


#经济周期两种不同状态下均值的先验分布

factor_miu=as.matrix(mvrnorm(n=1, mu = c(-4,4), Sigma = matrix(c(4^2,0,0,4^2),2,2)))


#特质波动自回归噪声方差的先验分布

noise_sigma=as.matrix((1/rgamma(10,shape=1,rate=1))^(1/2))

#行列调整（行为指标，列为时间）
true_y=t(true_y)
idiosyncracy_matrix=t(idiosyncracy_matrix)
principal_variable=t(principal_variable)

#markov链初始转移概率，分别为0到0,0到1,1到0和1到1的转移概率(先列后行)

p_miu=matrix(0,2,2)

p_miu[1,1]=rbeta(1,1,1)
p_miu[2,2]=rbeta(1,1,1)

p_miu[2,1]=1-p_miu[1,1]
p_miu[1,2]=1-p_miu[2,2]


#经济所属状态识别变量

state_variable=matrix(0,nrow=1,ncol=ncol(true_y))



