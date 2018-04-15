

#通过OLS和自回归得到模型系数初始值


#factor_autoreg_weight=rnorm(1,mean = 0,sd=1)
#量测方程系数初始值
Alpha=matrix(0,nrow=block,ncol=indices)
#Alpha_miu=matrix(0,nrow=block,ncol=indices)
#Alpha_sigma=matrix(1,nrow=block,ncol=indices)

for(k in 1:block)
{
  for(i in 1:indices)
  {
    # set.seed(660-i)
    # Alpha[,i]=rnorm(ncol(GDP),mean = 0,sd=1)
    Alpha[k,i]=lm(province[[k]][i,]~H[k,])$coefficients[2]
  }
}


Beta=list(matrix(0,nrow=length(east_number),ncol=1),matrix(0,nrow=length(mid_number),ncol=1),
          matrix(0,nrow=length(west_number),ncol=1),matrix(0,nrow=length(north_number),ncol=1))
# Beta_miu=list(matrix(0,nrow=length(east_number),ncol=1),matrix(0,nrow=length(mid_number),ncol=1),
#               matrix(0,nrow=length(west_number),ncol=1),matrix(0,nrow=length(north_number),ncol=1))
# Beta_sigma=list(matrix(1,nrow=length(east_number),ncol=1),matrix(1,nrow=length(mid_number),ncol=1),
#                 matrix(1,nrow=length(west_number),ncol=1),matrix(1,nrow=length(north_number),ncol=1))

east=1
mid=1
west=1
north=1
for(i in 1:block)
{
  if(i%in%east_number)
  {
    Beta[[1]][east,]=lm(H[i,]~G[1,])$coefficients[2]
    
    east=east+1
  }
  else if(i%in%mid_number)
  {
    Beta[[2]][mid,]=lm(H[i,]~G[2,])$coefficients[2]
    mid=mid+1
  } 
  else if(i%in%west_number)
  {
    Beta[[3]][west,]=lm(H[i,]~G[3,])$coefficients[2]
    west=west+1
  }
  else if(i%in%north_number)
  {
    Beta[[4]][north,]=lm(H[i,]~G[4,])$coefficients[2]
    north=north+1
  }
}



# set.seed(661)
# Beta[[1]]=as.matrix(rnorm(length(east_number),mean = 0,sd=1))
# set.seed(662)
# Beta[[2]]=as.matrix(rnorm(length(mid_number),mean=0,sd=1))
# set.seed(663)
# Beta[[3]]=as.matrix(rnorm(length(west_number),mean=0,sd=1))
# set.seed(664)
# Beta[[4]]=as.matrix(rnorm(length(north_number),mean=0,sd=1))

# set.seed(665)
# Theta=as.matrix(rnorm(4,mean=0,sd=1))

Theta=matrix(0,nrow=area,ncol=1)
# Theta_miu=matrix(0,nrow=area,ncol=1)
# Theta_sigma=matrix(1,nrow=area,ncol=1)
for(i in 1:area)
{
  Theta[i,]=lm(G[i,]~t(Factor))$coefficients[2]
}



#异质性因子自回归与状态方程自回归系数
###factor
e_1=province

for(k in 1:block)
{
  for(i in 1:indices)
  {
    e_1[[k]][i,]=province[[k]][i,]-Alpha[k,i]*H[k,]
  }
}


east=1
mid=1
west=1
north=1
t=1
e_2=H

for(i in 1:block)
{
  for(t in 1:time)
  {
    if(i%in%east_number)
    {
      e_2[i,t]=H[i,t]-Beta[[1]][east,]*G[1,t]
    }else if(i%in%mid_number)
    {
      e_2[i,t]=H[i,t]-Beta[[2]][mid,]*G[2,t]
    }else if(i%in%west_number)
    {
      e_2[i,t]=H[i,t]-Beta[[3]][west,]*G[3,t]
    }else if(i%in%north_number)
    {
      e_2[i,t]=H[i,t]-Beta[[4]][north,]*G[4,t]
    }
  }
  if(i%in%east_number)
  {
    east=east+1
  }else if(i%in%mid_number)
  {
    mid=mid+1
  }else if(i%in%west_number)
  {
    west=west+1
  }else if(i%in%north_number)
  {
    north=north+1
  }
}

e_3=matrix(0,nrow=area,ncol=time)

for(i in 1:area)
{
  for(t in 1:time)
  {
    e_3[i,t]=G[i,t]-Theta[i,]*Factor[,t]
  }
}



lambda_1=matrix(0,nrow=block,ncol=indices)
# lambda_1_miu=matrix(0,nrow=block,ncol=indices)
# lambda_1_sigma=matrix(1,nrow=block,ncol=indices)



for(k in 1:block)
{
   for(i in 1:indices)
  {
    # set.seed(566-2*i)
    # lambda_1[,i]=as.matrix(rnorm(ncol(GDP),mean=0,sd=1))
    lambda_1[k,i]=arma(e_1[[k]][i,],order = c(1,0))$coef[1]
  }
}

# 
# 
#经济周期两种不同状态下均值的先验分布
miu_2=matrix(0,nrow=block,ncol=2)
# miu_2_miu=matrix(0,nrow=block,ncol=2)
# miu_2_sigma=matrix(1,nrow=block,ncol=2)
for(i in 1:block)
{
  #set.seed(673+i)
  miu_2[i,1]=rnorm(1,mean=0,sd=6)
  miu_2[i,2]=miu_2[i,1]+abs(rnorm(1,mean=0,sd=2))
}
#set.seed(674)
miu_3=matrix(0,nrow=area,ncol=2)
# miu_3_miu=matrix(0,nrow=area,ncol=2)
# miu_3_sigma=matrix(1,nrow=area,ncol=2)

for(i in 1:area)
{
  miu_3[i,1]=-rnorm(1,mean=0,sd=4)
  miu_3[i,2]=miu_3[i,1]+abs(rnorm(1,mean=0,sd=2))
}
#set.seed(675)
#miu=matrix(0,nrow=1,ncol=2)
# miu_miu=matrix(0,nrow=1,ncol=2)
# miu_sigma=matrix(1,nrow=1,ncol=2)

# miu[1,1]=rnorm(1,mean=0,sd=2)
# miu[1,2]=miu[1,1]+abs(rnorm(1,mean=0,sd=2))


#markov链初始转移概率，分别为0到0,0到1,1到0和1到1的转移概率(先列后行)

p_miu_2=rep(list(0),block)
for(i in 1:block)
{
  p_miu_2[[i]]=matrix(0,2,2)
  #set.seed(679+i)
  p_miu_2[[i]][1,1]=rbeta(1,8,2)
  p_miu_2[[i]][2,2]=rbeta(1,8,2)
  p_miu_2[[i]][2,1]=1-p_miu_2[[i]][1,1]
  p_miu_2[[i]][1,2]=1-p_miu_2[[i]][2,2]
}


p_miu_3=rep(list(0),area)
for(i in 1:area)
{
  p_miu_3[[i]]=matrix(0,2,2)
  #set.seed(780+i)
  p_miu_3[[i]][1,1]=rbeta(1,8,2)
  p_miu_3[[i]][2,2]=rbeta(1,8,2)
  p_miu_3[[i]][2,1]=1-p_miu_3[[i]][1,1]
  p_miu_3[[i]][1,2]=1-p_miu_3[[i]][2,2]
}

#set.seed(881)
# p_miu=matrix(0,2,2)
# p_miu[1,1]=rbeta(1,8,2)
#set.seed(882)
# p_miu[2,2]=rbeta(1,8,2)
# p_miu[2,1]=1-p_miu[1,1]
# p_miu[1,2]=1-p_miu[2,2]






#经济所属状态识别变量
state_variable_2=matrix(0,nrow=block,ncol=time)
for(i in 1:block)
{
  state_variable_2[i,1]=0
  for(t in 2:time)
  {
    #set.seed(483+t)
    if(state_variable_2[i,t-1]==0)
    {
      state_variable_2[i,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu_2[[i]][,1])
    }else if(state_variable_2[i,t-1]==1)
    {
      state_variable_2[i,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu_2[[i]][,2])
    }
  }
}


state_variable_3=matrix(0,nrow=area,ncol=time)

for(i in 1:area)
{
  state_variable_3[i,1]=0
  for(t in 2:time)
  {
    #set.seed(483-t)
    if(state_variable_3[i,t-1]==0)
    {
      state_variable_3[i,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu_3[[i]][,1])
    }else if(state_variable_3[i,t-1]==1)
    {
      state_variable_3[i,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu_3[[i]][,2])
    }  
  }
}

# state_variable=matrix(0,nrow=1,ncol=time)
# state_variable[,1]=0
# 
# for(t in 2:time)
# {
#   #set.seed(984+t)
#   if(state_variable[,t-1]==0)
#   {
#     state_variable[,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu[,1])
#   }else if(state_variable[,t-1]==1)
#   {
#     state_variable[,t]=sample(c(0,1),size =1,replace = TRUE,prob = p_miu[,2])
#   }  
# }

e_2_minus_miu_2=e_2
for(i in 1:block)
{
  for(t in 1:time)
  {
    e_2_minus_miu_2[i,t]=e_2[i,t]-miu_2[i,state_variable_2[i,t]+1]
  }
}

e_3_minus_miu_3=e_3
for(i in 1:area)
{
  for(t in 1:time)
  {
    e_3_minus_miu_3[i,t]=e_3[i,t]-miu_3[i,state_variable_3[i,t]+1]
  }
}

east=1
mid=1
west=1
north=1
lambda_2=list(matrix(0,nrow=length(east_number)),matrix(0,nrow=length(mid_number)),
              matrix(0,nrow=length(west_number)),matrix(0,nrow=length(north_number)))
# lambda_2_miu=list(matrix(0,nrow=length(east_number)),matrix(0,nrow=length(mid_number)),
#               matrix(0,nrow=length(west_number)),matrix(0,nrow=length(north_number)))
# lambda_2_sigma=list(matrix(1,nrow=length(east_number)),matrix(1,nrow=length(mid_number)),
#               matrix(1,nrow=length(west_number)),matrix(1,nrow=length(north_number)))
for(i in 1:block)
{
  if(i%in%east_number)
  {
    lambda_2[[1]][east,]=arma(e_2_minus_miu_2[i,],order = c(1,0))$coef[1]
    east=east+1
  }else if(i%in%mid_number)
  {
    lambda_2[[2]][mid,]=arma(e_2_minus_miu_2[i,],order = c(1,0))$coef[1]
    mid=mid+1
  }else if(i%in%west_number)
  {
    lambda_2[[3]][west,]=arma(e_2_minus_miu_2[i,],order = c(1,0))$coef[1]
    west=west+1
  }else if(i%in%north_number)
  {
    lambda_2[[4]][north,]=arma(e_2_minus_miu_2[i,],order = c(1,0))$coef[1]
    north=north+1
  }
}

# set.seed(667)
# lambda_2[[1]]=as.matrix(rnorm(length(east_number),mean=0,sd=1))
# set.seed(668)
# lambda_2[[2]]=as.matrix(rnorm(length(mid_number),mean=0,sd=1))
# set.seed(669)
# lambda_2[[3]]=as.matrix(rnorm(length(west_number),mean=0,sd=1))
# set.seed(670)
# lambda_2[[4]]=as.matrix(rnorm(length(north_number),mean=0,sd=1))
#   
# set.seed(671)
# lambda_3=as.matrix(rnorm(4,mean=0,sd=1))
lambda_3=matrix(0,nrow=area,ncol=1)
# lambda_3_miu=matrix(0,nrow=area,ncol=1)
# lambda_3_sigma=matrix(1,nrow=area,ncol=1)

for(i in 1:area)
{
  lambda_3[i,]=arma(e_3_minus_miu_3[i,],order = c(1,0))$coef[1]
}

# 
# set.seed(672)
# lambda=rnorm(1,mean=0,sd=1)
# Factor_minus_miu=Factor
# 
# 
# for(t in 1:time)
# {
#   Factor_minus_miu[,t]=Factor[,t]-miu[,state_variable[,t]+1]
# }



lambda=arma(t(Factor),order = c(1,0))$coef[1]

# lambda_miu=0
# lambda_sigma=1






#特质波动自回归噪声方差的先验分布  
#set.seed(676)

noise_sigma_1=matrix(0,nrow=block,ncol=indices)
for(k in 1:block)
{
  for(i in 1:indices)
  {
    noise_sigma_1[k,i]=sd(arma(e_1[[k]][i,],order = c(1,0))$residuals[-1])
  }
}

#set.seed(677)


noise_sigma_2=matrix(0,nrow=block,ncol=1)
for(i in 1:block)
{
  noise_sigma_2[i,]=sd(arma(e_2_minus_miu_2[i,],order = c(1,0))$residuals[-1])
}
#set.seed(678)



noise_sigma_3=matrix(0,nrow=area,ncol=1)
for(i in 1:area)
{
  noise_sigma_3[i,]=sd(arma(e_3_minus_miu_3[i,],order = c(1,0))$residuals[-1])
}
noise_sigma=sd(arma(t(Factor),order = c(1,0))$residuals[-1])




#行列调整（行为指标，列为时间）
# true_y=t(true_y)
# idiosyncracy_matrix=t(idiosyncracy_matrix)
# principal_variable=t(principal_variable)






#内部参数：kalman滤波中共同变量估计误差的先验
#factor_var_initial=matrix(c(1,0,0,1),nrow=2,ncol=2)


