
alpha_model=rep(list(0),block)
#量测系数
for(i in 1:block)
{
  #alpha_model[[i]]=weight_estimation(y=y_for_alpha[[i]],x=x_for_alpha[[i]],sigma = as.matrix(noise_sigma_1[i,]),prior_miu = as.matrix(Alpha_miu[i,]),prior_sigma = as.matrix(Alpha_sigma[i,]))
  alpha_model[[i]]=weight_estimation(y=y_for_alpha[[i]],x=x_for_alpha[[i]],sigma = as.matrix(noise_sigma_1[i,]),prior_miu = 1,prior_sigma = 1)
  Alpha[i,]=alpha_model[[i]][[1]]
 #Alpha_miu[i,]=alpha_model[[i]][[2]]
 #Alpha_sigma[i,]=sqrt(alpha_model[[i]][[3]])
 #Alpha[i,1]=1
}


east=1
mid=1
west=1
north=1
beta_model=rep(list(0),block)
for(i in 1:block)
{
  
  if(i%in%east_number)
  {
    #beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu =as.matrix(Beta_miu[[1]][east,]),prior_sigma =as.matrix(Beta_sigma[[1]][east,]))
    beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 1,prior_sigma = 1)
    Beta[[1]][east,]=beta_model[[i]][[1]]
    #Beta_miu[[1]][east,]=beta_model[[i]][[2]]
    #Beta_sigma[[1]][east,]=sqrt(beta_model[[i]][[3]])
  }else if(i%in%mid_number)
  {
     
    #beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(Beta_miu[[2]][mid,]),prior_sigma = as.matrix(Beta_sigma[[2]][mid,]))
    beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 1,prior_sigma = 1)
    Beta[[2]][mid,]=beta_model[[i]][[1]]
    #Beta_miu[[2]][mid,]=beta_model[[i]][[2]]
    #Beta_sigma[[2]][mid,]=sqrt(beta_model[[i]][[3]])
    
  }else if(i%in%west_number)
  {
    #beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(Beta_miu[[3]][west,]),prior_sigma = as.matrix(Beta_sigma[[3]][west,]))
    beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 1,prior_sigma = 1)
    Beta[[3]][west,]=beta_model[[i]][[1]]
    #Beta_miu[[3]][west,]=beta_model[[i]][[2]]
    #Beta_sigma[[3]][west,]=sqrt(beta_model[[i]][[3]])
    
  }else if(i%in%north_number)
  {
    #beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(Beta_miu[[4]][north,]),prior_sigma =as.matrix(Beta_sigma[[4]][north,]))
    beta_model[[i]]=weight_estimation(y=t(y_for_beta[i,]),x=t(x_for_beta[i,]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 1,prior_sigma = 1)
    
    Beta[[4]][north,] =beta_model[[i]][[1]]
    #Beta_miu[[4]][north,] =beta_model[[i]][[1]]
    #Beta_sigma[[4]][north,] =sqrt(beta_model[[i]][[3]])
    
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


theta_model=rep(list(0),area)
for(i in 1:area)
{
  #theta_model[[i]]=weight_estimation(y=t(y_for_theta[i,]),x=t(x_for_theta[i,]),sigma = as.matrix(noise_sigma_3[i,]),prior_miu = as.matrix(Theta_miu[i,]),prior_sigma = as.matrix(Theta_sigma[i,]))
  theta_model[[i]]=weight_estimation(y=t(y_for_theta[i,]),x=t(x_for_theta[i,]),sigma = as.matrix(noise_sigma_3[i,]),prior_miu = 1,prior_sigma = 1)
  
  Theta[i,]=theta_model[[i]][[1]]
  #Theta_miu[i,]=theta_model[[i]][[2]]
  #Theta_sigma[i,]=sqrt(theta_model[[i]][[3]])
}


#自回归系数
lambda_1_model=rep(list(0),block)
for(i in 1:block)
{
  #lambda_1_model[[i]] =weight_estimation(y=y_for_lambda_1[[i]][,-1],x=y_for_lambda_1[[i]][,-time],sigma = as.matrix(noise_sigma_1[i,]),prior_miu = as.matrix(lambda_1_miu[i,]),prior_sigma =as.matrix(lambda_1_sigma[i,]))
  lambda_1_model[[i]] =weight_estimation(y=y_for_lambda_1[[i]][,-1],x=y_for_lambda_1[[i]][,-time],sigma = as.matrix(noise_sigma_1[i,]),prior_miu = 0.1,prior_sigma = 1)
  
  lambda_1[i,]=lambda_1_model[[i]][[1]]
  #lambda_1_miu[i,]=lambda_1_model[[i]][[2]]
  #lambda_1_sigma[i,]=sqrt(lambda_1_model[[i]][[3]])
}


east=1
mid=1
west=1
north=1
lambda_2_model=rep(list(0),block)

for(i in 1:block)
{
  
  if(i%in%east_number)
  {
    #lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(lambda_2_miu[[1]][east,]),prior_sigma = as.matrix(lambda_2_sigma[[1]][east,]))
    lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 0.1,prior_sigma = 1)
    
    lambda_2[[1]][east,]=lambda_2_model[[i]][[1]]
    #lambda_2_miu[[1]][east,]=lambda_2_model[[i]][[2]]
    #lambda_2_sigma[[1]][east,]=sqrt(lambda_2_model[[i]][[3]])
  }else if(i%in%mid_number)
  {
    #lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(lambda_2_miu[[2]][mid,]),prior_sigma = as.matrix(lambda_2_sigma[[2]][mid,]))
    lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 0.1,prior_sigma = 1)
    
    lambda_2[[2]][mid,]=lambda_2_model[[i]][[1]]
    #lambda_2_miu[[2]][mid,]=lambda_2_model[[i]][[2]]
    #lambda_2_sigma[[2]][mid,]=sqrt(lambda_2_model[[i]][[3]])
  }else if(i%in%west_number)
  {
    #lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(lambda_2_miu[[3]][west,]),prior_sigma = as.matrix(lambda_2_sigma[[3]][west,]))
    lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 0.1,prior_sigma = 1)
    
    lambda_2[[3]][west,]=lambda_2_model[[i]][[1]]
    #lambda_2_miu[[3]][west,]=lambda_2_model[[i]][[2]]
    #lambda_2_sigma[[3]][west,]=sqrt(lambda_2_model[[i]][[3]])
  }else if(i%in%north_number)
  {
    #lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = as.matrix(lambda_2_miu[[4]][north,]),prior_sigma = as.matrix(lambda_2_sigma[[4]][north,]))
    lambda_2_model[[i]]=weight_estimation(y=t(y_for_lambda_2[i,-1]),x=t(y_for_lambda_2[i,-time]),sigma = as.matrix(noise_sigma_2[i,]),prior_miu = 0.1,prior_sigma = 1)
    
    lambda_2[[4]][north,]=lambda_2_model[[i]][[1]]
    #lambda_2_miu[[4]][north,]=lambda_2_model[[i]][[2]]
    #lambda_2_sigma[[4]][north,]=sqrt(lambda_2_model[[i]][[3]])
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

lambda_3_model=rep(list(0),area)
for(i in 1:area)
{
  #lambda_3_model[[i]]=weight_estimation(y=t(y_for_lambda_3[i,-1]),x=t(y_for_lambda_3[i,-time]),sigma = as.matrix(noise_sigma_3[i,]),prior_miu = as.matrix(lambda_3_miu[i,]),prior_sigma = as.matrix(lambda_3_sigma[i,]))
  lambda_3_model[[i]]=weight_estimation(y=t(y_for_lambda_3[i,-1]),x=t(y_for_lambda_3[i,-time]),sigma = as.matrix(noise_sigma_3[i,]),prior_miu = 0.1,prior_sigma = 1)
  
  lambda_3[i,]=lambda_3_model[[i]][[1]]
  #lambda_3_miu[i,]=lambda_3_model[[i]][[2]]
  #lambda_3_sigma[i,]=sqrt(lambda_3_model[[i]][[3]])
}

#lambda_model=weight_estimation(y=t(y_for_lambda[,-1]),x=t(y_for_lambda[,-time]),sigma = as.matrix(noise_sigma),prior_miu = as.matrix(lambda_miu),prior_sigma = as.matrix(lambda_sigma))
lambda_model=weight_estimation(y=t(y_for_lambda[,-1]),x=t(y_for_lambda[,-time]),sigma = as.matrix(noise_sigma),prior_miu = 0.1,prior_sigma = 1)

lambda=lambda_model[[1]]
#lambda_miu=lambda_model[[2]]
#lambda_sigma=sqrt(lambda_model[[3]])






#sigma
for(i in 1:block)
{
  noise_sigma_1[i,]=sqrt(sigma2_estimation(y=province[[1]],autoreg_residual = parameter_for_noise_sigma2_1[[i]],prior_v = 6,prior_f = 8))
}

for(i in 1:block)
{
  noise_sigma_2[i,]=sqrt(sigma2_estimation(y=t(H[1,]),autoreg_residual =as.matrix(parameter_for_noise_sigma2_2[i,]),prior_v = 6,prior_f = 8))
}

for(i in 1:area)
{
  noise_sigma_3[i,]=sqrt(sigma2_estimation(y=t(G[1,]),autoreg_residual =as.matrix(parameter_for_noise_sigma2_3[i,]),prior_v = 6,prior_f = 8))
}


#miu
miu_2_model=rep(list(0),block)
for(i in 1:block)
{
  #miu_2_model[[i]]=miu_estimation(y=as.matrix(y_for_miu_2[i,]),x=x_for_miu_2[[i]],sigma =noise_sigma_2[i,],prior_miu =as.matrix( miu_2_miu[i,]),prior_sigma = diag(miu_2_sigma[i,]))
  miu_2_model[[i]]=miu_estimation(y=as.matrix(y_for_miu_2[i,]),x=x_for_miu_2[[i]],sigma =noise_sigma_2[i,],prior_miu = as.matrix(c(-1,1)),prior_sigma = diag(c(0.5,0.5)))
  
  miu_2[i,]=miu_2_model[[i]][[1]]
  #miu_2_miu[i,]=miu_2_model[[i]][[2]]
  #miu_2_sigma[i,1]=sqrt(miu_2_model[[i]][[3]][1,1])
  #miu_2_sigma[i,2]=sqrt(miu_2_model[[i]][[3]][2,2])
}


miu_3_model=rep(list(0),area)
for(i in 1:area)
{
  #miu_3_model[[i]]=miu_estimation(y=as.matrix(y_for_miu_3[i,]),x=x_for_miu_3[[i]],sigma =noise_sigma_3[i,],prior_miu =as.matrix( miu_3_miu[i,]),prior_sigma = diag(miu_3_sigma[i,]))
  miu_3_model[[i]]=miu_estimation(y=as.matrix(y_for_miu_3[i,]),x=x_for_miu_3[[i]],sigma =noise_sigma_3[i,],prior_miu = as.matrix(c(-1,1)),prior_sigma = diag(c(0.5,0.5)))

  miu_3[i,]=miu_3_model[[i]][[1]]
  # miu_3_miu[i,]=miu_3_model[[i]][[2]]
  # miu_3_sigma[i,1]=sqrt(miu_3_model[[i]][[3]][1,1])
  # miu_3_sigma[i,2]=sqrt(miu_3_model[[i]][[3]][2,2])
}


#miu_model=miu_estimation(y=t(y_for_miu),x=x_for_miu,sigma =as.numeric(noise_sigma),prior_miu = t(miu_miu),prior_sigma =diag(as.numeric(miu_sigma)))
# miu_model=miu_estimation(y=t(y_for_miu),x=x_for_miu,sigma =as.numeric(noise_sigma),prior_miu = as.matrix(c(-1,1)),prior_sigma = diag(c(0.5,0.5)))
# 
# miu=t(miu_model[[1]])
# # miu_miu=t(miu_model[[2]])
# # miu_sigma[,1]=sqrt(miu_model[[3]][1,1])
# # miu_sigma[,2]=sqrt(miu_model[[3]][2,2])
# 
# #p_miu
# for(i in 1:block)
# {
#   p_miu_2[[i]]=transition_p(state_value = t(state_variable_2[i,]))
# }
# 
# for(i in 1:area)
# {
#   p_miu_3[[i]]=transition_p(state_value = t(state_variable_3[i,]))
# }
# 
# 
# p_miu=transition_p(state_value = state_variable)




