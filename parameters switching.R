#底层模型变量与参数定义与转换

H=province_principal_variable

G=area_principal_variable

Factor=country_principal_variable

east=1
mid=1
west=1
north=1
t=2
y_b=province
for(i in 1:ncol(GDP))
{
  y_b[[i]]=y_b[[i]][,-1]
}

##量测方程
###y
for(i in 1:ncol(GDP))
{
  for(t in 2:nrow(GDP))
  {
    if(i%in%east_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[1]][east]*G[1,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[1]][east]*G[1,t-1]))
    }else if(i%in%mid_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[2]][mid]*G[2,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[2]][mid]*G[2,t-1]))
    }else if(i%in%west_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[3]][west]*G[3,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[3]][west]*G[3,t-1]))
    }else if(i%in%north_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[4]][north]*G[4,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[4]][north]*G[4,t-1]))
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

###量测系数
ob_w_1=cbind(Alpha, -lambda_1*Alpha)

###factor
east=1
mid=1
west=1
north=1
t=1
e_2=H

for(i in 1:ncol(GDP))
{
  for(t in 1:nrow(GDP))
  {
    if(i%in%east_number)
    {
      e_2[i,t]=H[i,t]-Beta[[1]][east]*G[1,t]
    }else if(i%in%mid_number)
    {
      e_2[i,t]=H[i,t]-Beta[[2]][mid]*G[2,t]
    }else if(i%in%west_number)
    {
      e_2[i,t]=H[i,t]-Beta[[3]][west]*G[3,t]
    }else if(i%in%north_number)
    {
      e_2[i,t]=H[i,t]-Beta[[4]][north]*G[4,t]
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

b_factor=rbind(e_2[,2],e_2[,1])

###量测扰动项
ob_sigma2_1=rep(list(0),ncol(GDP))
for(i in 1:ncol(GDP))
{
  ob_sigma2_1[[i]]=diag(noise_sigma_1[i,]*noise_sigma_1[i,])
}

##转移方程
###截距项
autoreg_intercept_1=rep(list(0),ncol(GDP))

for(i in 1:ncol(GDP))
{
  autoreg_intercept_1[[i]]=rbind(t(e_2[i,-1]),matrix(0,nrow=1,ncol=nrow(GDP)-1))
}

east=1
mid=1
west=1
north=1
t=2


for(i in 1:ncol(GDP))
{
  for(t in 2:nrow(GDP))
  {
    if(i%in%east_number)
    {
      autoreg_intercept_1[[i]][,t-1]=rbind(miu_2[i,state_variable_2[i,t]+1]-lambda_2[[1]][east,]*miu_2[i,state_variable_2[i,t-1]+1],0)
    }else if(i%in%mid_number)
    {
      autoreg_intercept_1[[i]][,t-1]=rbind(miu_2[i,state_variable_2[i,t]+1]-lambda_2[[2]][mid,]*miu_2[i,state_variable_2[i,t-1]+1],0)
    }else if(i%in%west_number)
    {
      autoreg_intercept_1[[i]][,t-1]=rbind(miu_2[i,state_variable_2[i,t]+1]-lambda_2[[3]][west,]*miu_2[i,state_variable_2[i,t-1]+1],0)
    }else if(i%in%north_number)
    {
      autoreg_intercept_1[[i]][,t-1]=rbind(miu_2[i,state_variable_2[i,t]+1]-lambda_2[[4]][north,]*miu_2[i,state_variable_2[i,t-1]+1],0)
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



###转移系数
autoreg_w_1=rep(list(0),ncol(GDP))

autoreg_w_1=cbind(rbind(lambda_2,1),matrix(0,nrow = 2,ncol=1))


east=1
mid=1
west=1
north=1
t=2


for(i in 1:ncol(GDP))
{
  
    if(i%in%east_number)
    {
      autoreg_w_1[[i]]=cbind(rbind(lambda_2[[1]][east,],1),matrix(0,nrow = 2,ncol=1))
    }else if(i%in%mid_number)
    {
      autoreg_w_1[[i]]=cbind(rbind(lambda_2[[2]][mid,],1),matrix(0,nrow = 2,ncol=1))
    }else if(i%in%west_number)
    {
      autoreg_w_1[[i]]=cbind(rbind(lambda_2[[3]][west,],1),matrix(0,nrow = 2,ncol=1))
    }else if(i%in%north_number)
    {
      autoreg_w_1[[i]]=cbind(rbind(lambda_2[[4]][north,],1),matrix(0,nrow = 2,ncol=1))
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







###转移扰动项
autoreg_sigma2_1=rep(list(0),ncol(GDP))

for(i in 1:ncol(GDP))
{
  autoreg_sigma2_1[[i]]=as.matrix(c(noise_sigma_2[i,]^2,0,0,0),nrow=2,ncol=2)

}


##系数估计用变量形式转换
###alpha

y_for_alpha=province
for(i in 1:ncol(GDP))
{
  y_for_alpha[[i]]=y_for_alpha[[i]][,-1]
}

for(i in 1:ncol(GDP))
{
  for(t in 2:nrow(GDP))
  {
    y_for_alpha[[i]][,t-1]=province[[i]][,t]-lambda_1[i,]*province[[i]][,t-1]
  }
}


x_for_alpha=province
for(i in 1:ncol(GDP))
{
  x_for_alpha[[i]]=x_for_alpha[[i]][,-1]
}
east=1
mid=1
west=1
north=1
t=2


for(i in 1:ncol(GDP))
{
  for(t in 2:nrow(GDP))
  {
    if(i%in%east_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[1]][east]*G[1,t]+e_2[i,t]-lambda_1[i,]*(Beta[[1]][east]*G[1,t-1]+e_2[i,t-1])
    }else if(i%in%mid_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[2]][mid]*G[2,t]+e_2[i,t]-lambda_1[i,]*(Beta[[2]][mid]*G[2,t-1]+e_2[i,t-1])
    }else if(i%in%west_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[3]][west]*G[3,t]+e_2[i,t]-lambda_1[i,]*(Beta[[3]][west]*G[3,t-1]+e_2[i,t-1])
    }else if(i%in%north_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[4]][north]*G[4,t]+e_2[i,t]-lambda_1[i,]*(Beta[[4]][north]*G[4,t-1]+e_2[i,t-1])
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



###lambda_1

east=1
mid=1
west=1
north=1
t=1

y_for_lambda_1=province


##量测方程
###y
for(i in 1:ncol(GDP))
{
  for(t in 1:nrow(GDP))
  {
    if(i%in%east_number)
    {
      y_for_lambda_1[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[1]][east]*G[1,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%mid_number)
    {
      y_for_lambda_1[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[2]][mid]*G[2,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%west_number)
    {
      y_for_lambda_1[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[3]][west]*G[3,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%north_number)
    {
      y_for_lambda_1[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[4]][north]*G[4,t]-Alpha[i,]*e_2[i,t])
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

x_for_lambda_1=province
for(i in 1:ncol(GDP))
{
  x_for_lambda_1[[i]]=x_for_lambda_1[[i]][,t-1]
}
for(i in 1:ncol(GDP))
{
  x_for_lambda_1[[i]]=y_for_lambda_1[[i]][,-1]
}





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
y_m[,t]=(H[,t]-miu_2[state_2[,t]+1,]-Beta*Theta*Factor[,t])-lambda_2*(H[,t-1]-miu_2[state_2[,t-1]+1,]-Beta*Theta*Factor[,t-1])

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
ob_w_3=cbind(Theta, -lambda_3*Theta)

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

