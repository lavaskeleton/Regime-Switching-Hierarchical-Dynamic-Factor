


#中间层模型变量与参数转换

##量测方程
###y

east=1
mid=1
west=1
north=1
t=2
y_m_part=e_2[,-1]


##量测方程
###y
for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      y_m_part[i,t-1]=(H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[1]][east,]*Theta[1,]*Factor[,t])-lambda_2[[1]][east,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[1]][east,]*Theta[1,]*Factor[,t-1])
    }else if(i%in%mid_number)
    {
      y_m_part[i,t-1]=(H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[2]][mid,]*Theta[2,]*Factor[,t])-lambda_2[[2]][mid,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[2]][mid,]*Theta[2,]*Factor[,t-1])
    }else if(i%in%west_number)
    {
      y_m_part[i,t-1]=(H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[3]][west,]*Theta[3,]*Factor[,t])-lambda_2[[3]][west,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[3]][west,]*Theta[3,]*Factor[,t-1])
    }else if(i%in%north_number)
    {
      y_m_part[i,t-1]=(H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[4]][north,]*Theta[4,]*Factor[,t])-lambda_2[[4]][north,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[4]][north,]*Theta[4,]*Factor[,t-1])
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

y_m=rep(list(0),area)

for(i in 1:block)
{
    if(i%in%east_number)
    {
      y_m[[1]]=rbind(y_m[[1]],y_m_part[i,])
    }else if(i%in%mid_number)
    {
      y_m[[2]]=rbind(y_m[[2]],y_m_part[i,])
    }else if(i%in%west_number)
    {
      y_m[[3]]=rbind(y_m[[3]],y_m_part[i,])
    }else if(i%in%north_number)
    {
      y_m[[4]]=rbind(y_m[[4]],y_m_part[i,])
    }
}
for(i in 1:area)
{
  y_m[[i]]=y_m[[i]][-1,]
}



###量测系数
ob_w_2_part=matrix(0,nrow=block,ncol=2)

east=1
mid=1
west=1
north=1


for(i in 1:block)
{
  
  if(i%in%east_number)
  {
    ob_w_2_part[i,]=as.matrix(c(Beta[[1]][east,],-lambda_2[[1]][east,]*Beta[[1]][east,]))
  }else if(i%in%mid_number)
  {
    ob_w_2_part[i,]=as.matrix(c(Beta[[2]][mid,],-lambda_2[[2]][mid,]*Beta[[2]][mid,]))
  }else if(i%in%west_number)
  {
    ob_w_2_part[i,]=as.matrix(c(Beta[[3]][west,],-lambda_2[[3]][west,]*Beta[[3]][west,]))
  }else if(i%in%north_number)
  {
    ob_w_2_part[i,]=as.matrix(c(Beta[[4]][north,],-lambda_2[[4]][north,]*Beta[[4]][north,]))
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


ob_w_2=rep(list(0),area)

for(i in 1:block)
{
  if(i%in%east_number)
  {
    ob_w_2[[1]]=rbind(ob_w_2[[1]],ob_w_2_part[i,])
  }else if(i%in%mid_number)
  {
    ob_w_2[[2]]=rbind(ob_w_2[[2]],ob_w_2_part[i,])
  }else if(i%in%west_number)
  {
    ob_w_2[[3]]=rbind(ob_w_2[[3]],ob_w_2_part[i,])
  }else if(i%in%north_number)
  {
    ob_w_2[[4]]=rbind(ob_w_2[[4]],ob_w_2_part[i,])
  }
}
for(i in 1:area)
{
  ob_w_2[[i]]=ob_w_2[[i]][-1,]
}




###mid_factor

m_factor_initial=cbind(e_3[,2],e_3[,1])
m_factor_initial=t(m_factor_initial)

###量测扰动项

ob_sigma2_2=list(matrix(0,nrow=nrow(lambda_2[[1]]),ncol=nrow(lambda_2[[1]])),
                 matrix(0,nrow=nrow(lambda_2[[2]]),ncol=nrow(lambda_2[[2]])),
                 matrix(0,nrow=nrow(lambda_2[[3]]),ncol=nrow(lambda_2[[3]])),
                 matrix(0,nrow=nrow(lambda_2[[4]]),ncol=nrow(lambda_2[[4]])))
east=1
mid=1
west=1
north=1


for(i in 1:block)
{
  
  if(i%in%east_number)
  {
    ob_sigma2_2[[1]][east,east]=noise_sigma_2[i,]^2
  }else if(i%in%mid_number)
  {
    ob_sigma2_2[[2]][mid,mid]=noise_sigma_2[i,]^2
  }else if(i%in%west_number)
  {
    ob_sigma2_2[[3]][west,west]=noise_sigma_2[i,]^2
  }else if(i%in%north_number)
  {
    ob_sigma2_2[[4]][north,north]=noise_sigma_2[i,]^2
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






##转移方程

###截距项

autoreg_intercept_2=rep(list(matrix(0,nrow=2,ncol=time-1)),area)
for(i in 1:area)
{
  for(t in 2:time)
  {
    autoreg_intercept_2[[i]][,t-1]=rbind(miu_3[i,state_variable_3[i,t]+1]-lambda_3[i,]*miu_3[i,state_variable_3[i,t-1]+1],0)
  }
}



###转移系数
autoreg_w_2=rep(list(0),area)
for(i in 1:area)
{
  autoreg_w_2[[i]]=cbind(rbind(lambda_3[i,],1),matrix(0,nrow = 2,ncol=1))
}

###转移扰动项

autoreg_sigma2_2=rep(list(0),area)

for(i in 1:area)
{
  autoreg_sigma2_2[[i]]=matrix(c(noise_sigma_3[i,]^2,0,0,0),nrow=2,ncol=2)
}



##中层参数估计用参数转换
###beta

y_for_beta=matrix(0,nrow=block,ncol=time-1)
east=1
mid=1
west=1
north=1

for(i in 1:block)
{
  
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      y_for_beta[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-lambda_2[[1]][east,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1])
    }else if(i%in%mid_number)
    {
      y_for_beta[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-lambda_2[[2]][mid,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1])
    }else if(i%in%west_number)
    {
      y_for_beta[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-lambda_2[[3]][west,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1])
    }else if(i%in%north_number)
    {
      y_for_beta[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-lambda_2[[4]][north,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1])
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






x_for_beta=matrix(0,nrow=block,ncol=time-1)

east=1
mid=1
west=1
north=1

for(i in 1:block)
{
  
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      x_for_beta[i,t-1]=Theta[1,]*Factor[,t]+e_3[1,t]-lambda_2[[1]][east,]*(Theta[1,]*Factor[,t-1]+e_3[1,t-1])
    }else if(i%in%mid_number)
    {
      x_for_beta[i,t-1]=Theta[2,]*Factor[,t]+e_3[2,t]-lambda_2[[2]][mid,]*(Theta[2,]*Factor[,t-1]+e_3[2,t-1])
    }else if(i%in%west_number)
    {
      x_for_beta[i,t-1]=Theta[3,]*Factor[,t]+e_3[3,t]-lambda_2[[3]][west,]*(Theta[3,]*Factor[,t-1]+e_3[3,t-1])
    }else if(i%in%north_number)
    {
      x_for_beta[i,t-1]=Theta[4,]*Factor[,t]+e_3[4,t]-lambda_2[[4]][north,]*(Theta[4,]*Factor[,t-1]+e_3[4,t-1])
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






###sigma2_2


parameter_for_noise_sigma2_2=matrix(0,nrow=block,ncol=time)
east=1
mid=1
west=1
north=1

for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      parameter_for_noise_sigma2_2[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[1]][east,]*Theta[1,]*Factor[,t]-Beta[[1]][east,]*e_3[1,t]-
        lambda_2[[1]][east,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[1]][east,]*Theta[1,]*Factor[,t-1]-Beta[[1]][east,]*e_3[1,t-1])
    }else if(i%in%mid_number)
    {
      parameter_for_noise_sigma2_2[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[2]][mid,]*Theta[2,]*Factor[,t]-Beta[[2]][mid,]*e_3[2,t]-
        lambda_2[[2]][mid,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[2]][mid,]*Theta[2,]*Factor[,t-1]-Beta[[2]][mid,]*e_3[2,t-1])    
    }else if(i%in%west_number)
    {
      parameter_for_noise_sigma2_2[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[3]][west,]*Theta[3,]*Factor[,t]-Beta[[3]][west,]*e_3[3,t]-
        lambda_2[[3]][west,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[3]][west,]*Theta[3,]*Factor[,t-1]-Beta[[3]][west,]*e_3[3,t-1])    
    }else if(i%in%north_number)
    {
      parameter_for_noise_sigma2_2[i,t-1]=H[i,t]-miu_2[i,state_variable_2[i,t]+1]-Beta[[4]][north,]*Theta[4,]*Factor[,t]-Beta[[4]][north,]*e_3[4,t]-
        lambda_2[[4]][north,]*(H[i,t-1]-miu_2[i,state_variable_2[i,t-1]+1]-Beta[[4]][north,]*Theta[4,]*Factor[,t-1]-Beta[[4]][north,]*e_3[4,t-1])
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





###lambda_3
y_for_lambda_3=matrix(0,nrow=area,ncol=time)
for(i in 1:area)
{
  for(t in 1:time)
  {
      y_for_lambda_3[i,t]=e_3[i,t]-miu_3[i,state_variable_3[i,t]+1]
  }
}



#x_for_lambda_3=e_3[,t-1]-miu_3[state_3[,t-1]+1,]

###miu_3
y_for_miu_3=matrix(0,nrow=area,ncol=time-1)
for(i in 1:area)
{
  for(t in 2:time)
  {
    y_for_miu_3[i,t-1]=e_3[i,t]-lambda_3[i,]*e_3[i,t-1]
  }
}



x_for_miu_3=rep(list(0),area)

for(i in 1:area)
{
  x_for_miu_3[[i]]=matrix(0,nrow=2,ncol=time-1)
}

for(i in 1:area)
{
  for(t in 2:time)
  {
    x_for_miu_3[[i]][,t-1]=matrix(c(1-lambda_3[i,],state_variable_3[i,t]-lambda_3[i,]*state_variable_3[i,t-1]),nrow=2)
  }
}

for(i in 1:area)
{
  x_for_miu_3[[i]]=t(x_for_miu_3[[i]])
}



#上层模型变量与参数转换
##量测方程
###y
y_u=matrix(0,nrow=area,ncol=time-1)
for(i in 1:area)
{
  for(t in 2:time)
  {
    y_u[i,t-1]=(G[i,t]-miu_3[i,state_variable_3[i,t]+1])-lambda_3[i,]*(G[i,t-1]-miu_3[i,state_variable_3[i,t-1]+1])
  }
}



###量测系数
ob_w_3=cbind(Theta, -lambda_3*Theta)

###factor
u_factor_initial=rbind(Factor[,2],Factor[,1])

###量测扰动项
ob_sigma2_3=diag(as.numeric(noise_sigma_3*noise_sigma_3))


##转移方程
###截距项
# autoreg_intercept_3=matrix(0,nrow=2,ncol=time-1)
# for(t in 2:time)
# {
#   autoreg_intercept_3[,t-1]=rbind(miu[,state_variable[,t]+1]-lambda*miu[,state_variable[,t-1]+1],0)
# }




###转移系数
autoreg_w_3=cbind(rbind(lambda,1),matrix(0,nrow = 2,ncol=1))

###转移扰动项
autoreg_sigma2_3=matrix(c(noise_sigma^2,0,0,0),nrow=2,ncol=2)




###theta
y_for_theta=matrix(0,nrow=area,ncol=time-1)
for(i in 1:area)
{
  for(t in 2:time)
  {
    y_for_theta[i,t-1]=G[i,t]-miu_3[i,state_variable_3[i,t]+1]-lambda_3[i,]*(G[i,t-1]-miu_3[i,state_variable_3[i,t-1]+1])
  }
}

x_for_theta=matrix(0,nrow=area,ncol=time-1)
for(i in 1:area)
{
  for(t in 2:time)
  {
    x_for_theta[i,t-1]=Factor[,t]-lambda_3[i,]*Factor[,t-1]
  }
}

###sigma2_3
parameter_for_noise_sigma2_3=matrix(0,nrow=area,ncol=time-1)
for(i in 1:area)
{
  for(t in 2:time)
  {
    parameter_for_noise_sigma2_3[i,t-1]=G[i,t]-miu_3[i,state_variable_3[i,t]+1]-Theta[i,]*Factor[,t]-lambda_3[i,]*(G[i,t-1]-miu_3[i,state_variable_3[i,t-1]+1]-Theta[i,]*Factor[,t-1])
  }
}





###lambda
y_for_lambda=matrix(0,nrow=1,ncol=time)
for(t in 1:time)
{
  #y_for_lambda[,t]=Factor[,t]-miu[,state_variable[,t]+1]
  y_for_lambda[,t]=Factor[,t]

}

#x_for_lambda=Factor[,t-1]-miu[state_F[,t-1]+1,]

###miu
# y_for_miu=matrix(0,nrow=1,ncol=time-1)
# for(t in 2:time)
# {
#   y_for_miu[,t-1]=Factor[,t]-lambda*Factor[,t-1]
# }
# 
# 
# x_for_miu=matrix(0,nrow=2,ncol=time-1)
# 
# for(t in 2:time)
# {
#   x_for_miu[,t-1]=matrix(c(1-lambda,state_variable[,t]-lambda*state_variable[,t-1]),ncol=2)
# }
# 
# x_for_miu=t(x_for_miu)







