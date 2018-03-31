

#通过OLS和自回归得到模型系数初始值

# ##观测方程系数初始值，为各个指标对主成份的OLS回归系数
# observe_weights_1=matrix(0,nrow=ncol(GDP),ncol=3)
# 
# 
# # j=1
# # for(j in 1:ncol(true_y))
# # {
# #   observe_weights[j,]=rnorm(1,mean=0,sd=1)
# # }
# 
# reg_model_1=rep(list(0),(ncol(GDP)*3))
# 
# 
# j=1
# for(j in 1:ncol(GDP))
# {
# 
#     reg_model_1[[(j-1)*3+1]]=lm(GDP[,j]~province_principal_variable[i,])
#     reg_model_1[[(j-1)*3+2]]=lm(invest[,j]~province_principal_variable[i,])
#     reg_model_1[[(j-1)*3+3]]=lm(consume[,j]~province_principal_variable[i,])
#     
#     observe_weights_1[j,1]=reg_model_1[[(j-1)*3+1]]$coefficients[2]
#     observe_weights_1[j,2]=reg_model_1[[(j-1)*3+2]]$coefficients[2]
#     observe_weights_1[j,3]=reg_model_1[[(j-1)*3+3]]$coefficients[2]
#     
# }
# 
# 
# 
# 
# 
# observe_weights_2=rep(list(0),4)
# 
# 
# 
# # j=1
# # for(j in 1:ncol(true_y))
# # {
# #   observe_weights[j,]=rnorm(1,mean=0,sd=1)
# # }
# 
# reg_model_2=rep(list(0),4)
# reg_model_2[[1]]=rep(list(0),9)
# reg_model_2[[2]]=rep(list(0),9)
# reg_model_2[[3]]=rep(list(0),10)
# reg_model_2[[4]]=rep(list(0),3)
# 
# east=1
# mid=1
# west=1
# north=1
# 
# j=1
# for(j in 1:ncol(GDP))
# {
#   if(j%in%east_number)
#   {
#     reg_model_2[[1]][[east]]=lm(east_province[,east]~area_principal_variable[1,])
#     east=east+1
#   }
#   else if(j%in%mid_number)
#   {
#     
#     reg_model_2[[2]][[mid]]=lm(mid_province[,mid]~area_principal_variable[2,])
#     mid=mid+1
#   }
#   else if(j%in%west_number)
#   {
#     reg_model_2[[3]][[west]]=lm(west_province[,west]~area_principal_variable[3,])
#     west=west+1
#   }
#   else if(j%in%north_number)
#   {
#     reg_model_2[[4]][[north]]=lm(north_province[,north]~area_principal_variable[4,])
#     north=north+1
#   }
# }
# reg_model_2[[1]][[1]]$coefficients[2]
#   observe_weights_2[[1]]=reg_model_1[[(j-1)*3+1]]$coefficients[2]
#   observe_weights_1[j,2]=reg_model_1[[(j-1)*3+2]]$coefficients[2]
#   observe_weights_1[j,3]=reg_model_1[[(j-1)*3+3]]$coefficients[2]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #状态方程系数初始值与特质波动系数初始值，为主成份和特质波动的自回归系数
# 
# ##特质波动自回归系数初始值
# idiosyncracy_autoreg_weights=matrix(0,nrow=ncol(true_y),ncol=1)
# #idiosyncracy_autoreg_weights=mvrnorm(ncol(true_y),mu = c(rep(0,10)),Sigma = diag(c(rep(1,10))))
# 
# idiosyncracy_autoreg_model=rep(list(0),ncol(true_y))
# idiosyncracy_matrix=matrix(0,nrow=nrow(true_y),ncol = ncol(true_y))
# 
# j=1
# for(j in 1:ncol(true_y))
# {
#   idiosyncracy_matrix[,j]=reg_model[[j]]$residuals
# }
# 
# j=1
# for(j in 1:ncol(true_y))
# {
#   idiosyncracy_autoreg_model[[j]]=arma(idiosyncracy_matrix[,j],order = c(1,0))
#   idiosyncracy_autoreg_weights[j,]=idiosyncracy_autoreg_model[[j]]$coef[1]
# }
# 
# ##主成份自回归系数，也就是状态方程系数初始值
# 
# factor_autoreg_model=arma(principal_variable,order = c(1,0))
# 
# factor_autoreg_weight=factor_autoreg_model$coef[1]

#无信息下超参数的初始值

#factor_autoreg_weight=rnorm(1,mean = 0,sd=1)
#量测方程系数初始值
Alpha=matrix(0,nrow=ncol(GDP),ncol=3)

for(i in 1:3)
{
  Alpha[,i]=rnorm(ncol(GDP),mean = 0,sd=1)
}

Beta=rep(list(0),4)

Beta[[1]]=rnorm(length(east_number),mean = 0,sd=1)
Beta[[2]]=rnorm(length(mid_number),mean=0,sd=1)
Beta[[3]]=rnorm(length(west_number),mean=0,sd=1)
Beta[[4]]=rnorm(length(north_number),mean=0,sd=1)

Theta=rnorm(1,mean=0,sd=1)



#异质性因子自回归与状态方程自回归系数

lambda_1=matrix(0,nrow=ncol(GDP),ncol=nrow(province[[1]]))
for(i in 1:nrow(province[[1]]))
{
  lambda_1[,i]=as.matrix(rnorm(ncol(GDP),mean=0,sd=1))
}


lambda_2=list(matrix(0,nrow=length(east_number)),matrix(0,nrow=length(mid_number)),
              matrix(0,nrow=length(west_number)),matrix(0,nrow=length(north_number)))

  lambda_2[[1]]=as.matrix(rnorm(length(east_number),mean=0,sd=1))
  lambda_2[[2]]=as.matrix(rnorm(length(mid_number),mean=0,sd=1))
  lambda_2[[3]]=as.matrix(rnorm(length(west_number),mean=0,sd=1))
  lambda_2[[4]]=as.matrix(rnorm(length(north_number),mean=0,sd=1))
  
lambda_3=as.matrix(rnorm(4,mean=0,sd=1))
lambda=rnorm(1,mean=0,sd=1)

#经济周期两种不同状态下均值的先验分布

miu_2=as.matrix(mvrnorm(n=ncol(GDP), mu = c(-1,1), Sigma = matrix(c(1^2,0,0,1^2),2,2)))
miu_3=as.matrix(mvrnorm(n=4, mu = c(-1,1), Sigma = matrix(c(1^2,0,0,1^2),2,2)))
miu=as.matrix(mvrnorm(n=1, mu = c(-1,1), Sigma = matrix(c(1^2,0,0,1^2),2,2)))


#特质波动自回归噪声方差的先验分布
noise_sigma_1=matrix(0,nrow=ncol(GDP),ncol=nrow(province[[1]]))
for(i in 1:nrow(province[[1]]))
{
  noise_sigma_1[,i]=as.matrix((1/rgamma(ncol(GDP),shape=1,rate=1))^(1/2))
}
noise_sigma_2=as.matrix((1/rgamma(ncol(GDP),shape=1,rate=1))^(1/2))
noise_sigma_3=as.matrix((1/rgamma(4,shape=1,rate=1))^(1/2))
noise_sigma=as.matrix((1/rgamma(1,shape=1,rate=1))^(1/2))




#行列调整（行为指标，列为时间）
# true_y=t(true_y)
# idiosyncracy_matrix=t(idiosyncracy_matrix)
# principal_variable=t(principal_variable)

#markov链初始转移概率，分别为0到0,0到1,1到0和1到1的转移概率(先列后行)

p_miu_2=rep(list(0),ncol(GDP))
for(i in 1:ncol(GDP))
{
  p_miu_2[[i]]=matrix(0,2,2)
  
  p_miu_2[[i]][1,1]=rbeta(1,1,1)
  p_miu_2[[i]][2,2]=rbeta(1,1,1)
  p_miu_2[[i]][2,1]=1-p_miu_2[[i]][1,1]
  p_miu_2[[i]][1,2]=1-p_miu_2[[i]][2,2]

}




p_miu_3=rep(list(0),4)
for(i in 1:4)
{
  p_miu_3[[i]]=matrix(0,2,2)
  
  p_miu_3[[i]][1,1]=rbeta(1,1,1)
  p_miu_3[[i]][2,2]=rbeta(1,1,1)
  p_miu_3[[i]][2,1]=1-p_miu_3[[i]][1,1]
  p_miu_3[[i]][1,2]=1-p_miu_3[[i]][2,2]
  
}

p_miu=matrix(0,2,2)

  p_miu[1,1]=rbeta(1,1,1)
  p_miu[2,2]=rbeta(1,1,1)
  p_miu[2,1]=1-p_miu[1,1]
  p_miu[1,2]=1-p_miu[2,2]
  

#经济所属状态识别变量

state_variable_2=matrix(0,nrow=ncol(GDP),ncol=nrow(GDP))


state_variable_3=matrix(0,nrow=4,ncol=nrow(GDP))

state_variable=matrix(0,nrow=1,ncol=nrow(GDP))
