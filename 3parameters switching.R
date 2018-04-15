#底层模型变量与参数定义与转换

east=1
mid=1
west=1
north=1
t=2
y_b=province
for(i in 1:block)
{
  y_b[[i]]=y_b[[i]][,-1]
}

##量测方程
###y
for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[1]][east,]*G[1,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[1]][east,]*G[1,t-1]))
    }else if(i%in%mid_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[2]][mid,]*G[2,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[2]][mid,]*G[2,t-1]))
    }else if(i%in%west_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[3]][west,]*G[3,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[3]][west,]*G[3,t-1]))
    }else if(i%in%north_number)
    {
      y_b[[i]][,t-1]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[4]][north,]*G[4,t]-lambda_1[i,]*(province[[i]][,t-1]-Alpha[i,]*Beta[[4]][north,]*G[4,t-1]))
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
ob_w_1=rep(list(0),block)
for(i in 1:block)
{
  ob_w_1[[i]]=cbind(Alpha[i,], -lambda_1[i,]*Alpha[i,])
}



b_factor_initial=t(cbind(e_2[,2],e_2[,1]))

###量测扰动项
ob_sigma2_1=rep(list(0),block)
for(i in 1:block)
{
  ob_sigma2_1[[i]]=diag(noise_sigma_1[i,]*noise_sigma_1[i,])
}

##转移方程
###截距项
autoreg_intercept_1=rep(list(0),block)

for(i in 1:block)
{
  autoreg_intercept_1[[i]]=rbind(t(e_2[i,-1]),matrix(0,nrow=1,ncol=time-1))
}

east=1
mid=1
west=1
north=1
t=2


for(i in 1:block)
{
  for(t in 2:time)
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
autoreg_w_1=rep(list(0),block)

autoreg_w_1=cbind(rbind(lambda_2,1),matrix(0,nrow = 2,ncol=1))


east=1
mid=1
west=1
north=1
t=2


for(i in 1:block)
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
autoreg_sigma2_1=rep(list(0),block)

for(i in 1:block)
{
  autoreg_sigma2_1[[i]]=matrix(c(noise_sigma_2[i,]^2,0,0,0),nrow=2,ncol=2)

}


##系数估计用变量形式转换
###alpha
####y
y_for_alpha=province
for(i in 1:block)
{
  y_for_alpha[[i]]=y_for_alpha[[i]][,-1]
}

for(i in 1:block)
{
  for(t in 2:time)
  {
    y_for_alpha[[i]][,t-1]=province[[i]][,t]-lambda_1[i,]*province[[i]][,t-1]
  }
}

####x
x_for_alpha=province
for(i in 1:block)
{
  x_for_alpha[[i]]=x_for_alpha[[i]][,-1]
}
east=1
mid=1
west=1
north=1
t=2


for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[1]][east,]*G[1,t]+e_2[i,t]-lambda_1[i,]*(Beta[[1]][east,]*G[1,t-1]+e_2[i,t-1])
    }else if(i%in%mid_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[2]][mid,]*G[2,t]+e_2[i,t]-lambda_1[i,]*(Beta[[2]][mid,]*G[2,t-1]+e_2[i,t-1])
    }else if(i%in%west_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[3]][west,]*G[3,t]+e_2[i,t]-lambda_1[i,]*(Beta[[3]][west,]*G[3,t-1]+e_2[i,t-1])
    }else if(i%in%north_number)
    {
      x_for_alpha[[i]][,t-1]=Beta[[4]][north,]*G[4,t]+e_2[i,t]-lambda_1[i,]*(Beta[[4]][north,]*G[4,t-1]+e_2[i,t-1])
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
####y
y_for_lambda_1=province


for(i in 1:block)
{
  for(t in 1:time)
  {
    if(i%in%east_number)
    {
      y_for_lambda_1[[i]][,t]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[1]][east,]*G[1,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%mid_number)
    {
      y_for_lambda_1[[i]][,t]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[2]][mid,]*G[2,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%west_number)
    {
      y_for_lambda_1[[i]][,t]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[3]][west,]*G[3,t]-Alpha[i,]*e_2[i,t])
    }else if(i%in%north_number)
    {
      y_for_lambda_1[[i]][,t]=as.matrix(province[[i]][,t]-Alpha[i,]*Beta[[4]][north,]*G[4,t]-Alpha[i,]*e_2[i,t])
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
####x
# x_for_lambda_1=province
# for(i in 1:block)
# {
#   x_for_lambda_1[[i]]=x_for_lambda_1[[i]][,t-1]
# }
# for(i in 1:ncol(GDP))
# {
#   x_for_lambda_1[[i]]=y_for_lambda_1[[i]][,-nrow(GDP)]
# }





###sigma2_1
parameter_for_noise_sigma2_1=y_b

for(i in 1:block)
{
  parameter_for_noise_sigma2_1[[i]]=y_for_lambda_1[[i]][,-1] -lambda_1[i,]*y_for_lambda_1[[i]][,-time]
}

###lambda_2
y_for_lambda_2=matrix(0,nrow=block,ncol=time)


for(i in 1:block)
{
  for(t in 1:time)
  {
    y_for_lambda_2[i,t]=e_2[i,t]-miu_2[i,state_variable_2[i,t]+1]
  }
}


#x_for_lambda_2=e_2[,t-1]-miu_2[state_variable_2[,t-1]+1,]

###miu_2

east=1
mid=1
west=1
north=1
t=2

y_for_miu_2=e_2[,-1]


for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      y_for_miu_2[i,t-1]=e_2[i,t]-lambda_2[[1]][east,]*e_2[i,t-1]
    }else if(i%in%mid_number)
    {
      y_for_miu_2[i,t-1]=e_2[i,t]-lambda_2[[2]][mid,]*e_2[i,t-1]
    }else if(i%in%west_number)
    {
      y_for_miu_2[i,t-1]=e_2[i,t]-lambda_2[[3]][west,]*e_2[i,t-1]
    }else if(i%in%north_number)
    {
      y_for_miu_2[i,t-1]=e_2[i,t]-lambda_2[[4]][north,]*e_2[i,t-1]
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


x_for_miu_2=rep(list(0),block)

for(i in 1:block)
{
  x_for_miu_2[[i]]=matrix(0,nrow=2,ncol=time-1)
}

east=1
mid=1
west=1
north=1
t=2

for(i in 1:block)
{
  for(t in 2:time)
  {
    if(i%in%east_number)
    {
      x_for_miu_2[[i]][,t-1]=as.matrix(c(1-lambda_2[[1]][east,],state_variable_2[i,t]-lambda_2[[1]][east,]*state_variable_2[i,t-1]))
    }else if(i%in%mid_number)
    {
      x_for_miu_2[[i]][,t-1]=as.matrix(c(1-lambda_2[[2]][mid,],state_variable_2[i,t]-lambda_2[[2]][mid,]*state_variable_2[i,t-1]))
    }else if(i%in%west_number)
    {
      x_for_miu_2[[i]][,t-1]=as.matrix(c(1-lambda_2[[3]][west,],state_variable_2[i,t]-lambda_2[[3]][west,]*state_variable_2[i,t-1]))
    }else if(i%in%north_number)
    {
      x_for_miu_2[[i]][,t-1]=as.matrix(c(1-lambda_2[[4]][north,],state_variable_2[i,t]-lambda_2[[4]][north,]*state_variable_2[i,t-1]))
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

for(i in 1:block)
{
  x_for_miu_2[[i]]=t(x_for_miu_2[[i]])
}
