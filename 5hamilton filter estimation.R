
options(digits=16)

#通过Hamilton滤波更新共同因子所处状态
p_s_initial_2=rep(list(0),block)
for(i in 1:block)
{
  p_s_initial_2[[i]]=matrix(0,nrow = 1,ncol=2)
}

east=1
mid=1
west=1
north=1

for(i in 1:block)
{
  if(i %in% east_number)
  {
    p_s_initial_2[[i]][,1]=p_miu_2[[i]][1,1]*dnorm(e_2[i,1],mean=(1-lambda_2[[1]][east,])*miu_2[i,1],sd=noise_sigma_2[i,])+
                           p_miu_2[[i]][1,2]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[1]][east,]*miu_2[i,2],sd=noise_sigma_2[i,])
    p_s_initial_2[[i]][,2]=1-p_s_initial_2[[i]][,1]
                           #p_miu[2,2]*dnorm(e_2[i,1],mean=(1-lambda_2[[1]][east,])*miu_2[i,2],sd=noise_sigma_2[i,])+
                           #p_miu[2,1]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[1]][east,]*miu_2[i,2],sd=noise_sigma_2[i,])
    east=east+1
  }else if (i %in% mid_number)
  {
    p_s_initial_2[[i]][,1]=p_miu_2[[i]][1,1]*dnorm(e_2[i,1],mean=(1-lambda_2[[2]][mid,])*miu_2[i,1],sd=noise_sigma_2[i,])+
                           p_miu_2[[i]][1,2]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[2]][mid,]*miu_2[i,2],sd=noise_sigma_2[i,])
    p_s_initial_2[[i]][,2]=1-p_s_initial_2[[i]][,1]
                           # p_miu[2,2]*dnorm(e_2[i,1],mean=(1-lambda_2[[2]][mid,])*miu_2[i,2],sd=noise_sigma_2[i,])+
                           # p_miu[2,1]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[2]][mid,]*miu_2[i,2],sd=noise_sigma_2[i,])
    mid=mid+1  
  }
  else if (i %in% west_number)
  {
    p_s_initial_2[[i]][,1]=p_miu_2[[i]][1,1]*dnorm(e_2[i,1],mean=(1-lambda_2[[3]][west,])*miu_2[i,1],sd=noise_sigma_2[i,])+
                           p_miu_2[[i]][1,2]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[3]][west,]*miu_2[i,2],sd=noise_sigma_2[i,])
    p_s_initial_2[[i]][,2]=1-p_s_initial_2[[i]][,1]
  #                          p_miu[2,2]*dnorm(e_2[i,1],mean=(1-lambda_2[[3]][west,])*miu_2[i,2],sd=noise_sigma_2[i,])+
  #                          p_miu[2,1]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[3]][west,]*miu_2[i,2],sd=noise_sigma_2[i,])
    west=west+1
  }
  else if (i %in% north_number)
  {
    p_s_initial_2[[i]][,1]=p_miu_2[[i]][1,1]*dnorm(e_2[i,1],mean=(1-lambda_2[[4]][north,])*miu_2[i,1],sd=noise_sigma_2[i,])+
                           p_miu_2[[i]][1,2]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[4]][north,]*miu_2[i,2],sd=noise_sigma_2[i,])
    p_s_initial_2[[i]][,2]=1-p_s_initial_2[[i]][,1]
                           # p_miu[2,2]*dnorm(e_2[i,1],mean=(1-lambda_2[[4]][north,])*miu_2[i,2],sd=noise_sigma_2[i,])+
                           # p_miu[2,1]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[4]][north,]*miu_2[i,2],sd=noise_sigma_2[i,])
    north=north+1
  }
}



east=1
mid=1
west=1
north=1

for(i in 1:block)
{
  if(i%in%east_number)
  {
    state_variable_2[i,]=hamilton_filter(factor_value=t(e_2[i,]),p_s_initial = t(p_s_initial_2[[i]]),p_miu = p_miu_2[[i]] ,autoreg_w = lambda_2[[1]][east,],factor_miu = as.matrix(miu_2[i,]),autoreg_sd = noise_sigma_2[i,])
  }else if(i%in%mid_number)
  {
    state_variable_2[i,]=hamilton_filter(factor_value=t(e_2[i,]),p_s_initial = t(p_s_initial_2[[i]]),p_miu = p_miu_2[[i]] ,autoreg_w = lambda_2[[2]][mid,],factor_miu = as.matrix(miu_2[i,]),autoreg_sd = noise_sigma_2[i,])
  }else if(i%in%west_number)
  {
    state_variable_2[i,]=hamilton_filter(factor_value=t(e_2[i,]),p_s_initial = t(p_s_initial_2[[i]]),p_miu = p_miu_2[[i]] ,autoreg_w = lambda_2[[3]][west,],factor_miu = as.matrix(miu_2[i,]),autoreg_sd = noise_sigma_2[i,])
  }else if(i%in%north_number)
  {
    state_variable_2[i,]=hamilton_filter(factor_value=t(e_2[i,]),p_s_initial = t(p_s_initial_2[[i]]),p_miu = p_miu_2[[i]] ,autoreg_w = lambda_2[[4]][north,],factor_miu = as.matrix(miu_2[i,]),autoreg_sd = noise_sigma_2[i,])
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



p_s_initial_3=matrix(0,nrow = area,ncol=2)


for(i in 1:area)
{
    p_s_initial_3[i,1]=p_miu_3[[i]][1,1]*dnorm(e_3[i,1],mean=(1-lambda_3[i,])*miu_3[i,1],sd=noise_sigma_3[i,])+
                       p_miu_3[[i]][1,2]*dnorm(e_3[i,1],mean=miu_3[i,1]-lambda_3[i,]*miu_3[i,2],sd=noise_sigma_3[i,])
    p_s_initial_3[i,2]=1-p_s_initial_3[i,1]
    #p_miu[2,2]*dnorm(e_2[i,1],mean=(1-lambda_2[[1]][east,])*miu_2[i,2],sd=noise_sigma_2[i,])+
    #p_miu[2,1]*dnorm(e_2[i,1],mean=miu_2[i,1]-lambda_2[[1]][east,]*miu_2[i,2],sd=noise_sigma_2[i,])
}


for(i in 1:area)
{
  state_variable_3[i,]=hamilton_filter(factor_value=t(e_3[i,]),p_s_initial = as.matrix(p_s_initial_3[i,]),p_miu = p_miu_3[[i]] ,autoreg_w = lambda_3[i,],factor_miu = as.matrix(miu_3[i,]),autoreg_sd = noise_sigma_3[i,])
}


# p_s_initial=matrix(0,nrow = 1,ncol=2)
# 
# p_s_initial[,1]=p_miu[1,1]*dnorm(Factor[,1],mean=(1-lambda)*miu[,1],sd=noise_sigma)+
#                 p_miu[1,2]*dnorm(Factor[,1],mean=miu[,1]-lambda*miu[,2],sd=noise_sigma)
# p_s_initial[,2]=1-p_s_initial[,1]
# 
# 
# state_variable=hamilton_filter(factor_value=Factor,p_s_initial = t(p_s_initial),p_miu = p_miu ,autoreg_w = lambda,factor_miu = t(miu),autoreg_sd = noise_sigma)
