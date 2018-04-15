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
time=50
block=8
indices=2
area=4
# set.seed(666)
true_miu=matrix(c(0,0),nrow=2,ncol=1)
true_state=t(as.matrix(sample(c(0,1),size = time,prob = c(0.8,0.2),replace = TRUE)))
true_lambda=0.8
true_Factor=matrix(0,nrow=1,ncol=time)
true_Factor[,1]=0.1
for(t in 2:time)
{
  true_Factor[,t]=true_miu[true_state[,t]+1,]+true_lambda*(true_Factor[,t-1]-true_miu[true_state[,t-1]+1,])+rnorm(n = 1,mean=0,sd=0.05)
}
# 
# 
true_miu_3=matrix(0,nrow=2,ncol=4)
true_miu_3[,1]=c(-1,1)
true_miu_3[,2]=c(-0.5,0.5)
true_miu_3[,3]=c(-0.6,0.6)
true_miu_3[,4]=c(-0.7,0.7)


true_state_3=matrix(0,nrow=4,ncol=time)
true_state_3[1,]=t(as.matrix(sample(c(0,1),size = time,prob = c(0.7,0.3),replace = TRUE)))
true_state_3[2,]=t(as.matrix(sample(c(0,1),size = time,prob = c(0.7,0.3),replace = TRUE)))
true_state_3[3,]=t(as.matrix(sample(c(0,1),size = time,prob = c(0.7,0.3),replace = TRUE)))
true_state_3[4,]=t(as.matrix(sample(c(0,1),size = time,prob = c(0.7,0.3),replace = TRUE)))



true_lambda_3=as.matrix(c(0.7,0.6,0.5,0.4))
# 
true_e_3=matrix(0,nrow=4,ncol=time)
true_e_3[1,1]=0.2
true_e_3[2,1]=0.3
true_e_3[3,1]=0.4
true_e_3[4,1]=0.5

for(t in 2:time)
{
  true_e_3[1,t]=true_miu_3[true_state_3[1,t]+1,1]+true_lambda_3[1,]*(true_e_3[1,t-1]-true_miu_3[true_state_3[1,t-1]+1,1])+rnorm(n = 1,mean=0,sd=0.11)
  true_e_3[2,t]=true_miu_3[true_state_3[2,t]+1,2]+true_lambda_3[2,]*(true_e_3[2,t-1]-true_miu_3[true_state_3[2,t-1]+1,2])+rnorm(n = 1,mean=0,sd=0.12)
  true_e_3[3,t]=true_miu_3[true_state_3[3,t]+1,3]+true_lambda_3[3,]*(true_e_3[3,t-1]-true_miu_3[true_state_3[3,t-1]+1,3])+rnorm(n = 1,mean=0,sd=0.13)
  true_e_3[4,t]=true_miu_3[true_state_3[4,t]+1,4]+true_lambda_3[4,]*(true_e_3[4,t-1]-true_miu_3[true_state_3[4,t-1]+1,4])+rnorm(n = 1,mean=0,sd=0.14)
}


true_theta=matrix(0,nrow=4,ncol=1)
true_theta[1,1]=1.9
true_theta[2,1]=1.8
true_theta[3,1]=1.7
true_theta[4,1]=1.6

true_G=matrix(0,nrow=4,ncol=time)

for(t in 1:time)
{
  true_G[1,t]=true_theta[1,1]*true_Factor[,t]+true_e_3[1,t]
  true_G[2,t]=true_theta[2,1]*true_Factor[,t]+true_e_3[2,t]
  true_G[3,t]=true_theta[3,1]*true_Factor[,t]+true_e_3[3,t]
  true_G[4,t]=true_theta[4,1]*true_Factor[,t]+true_e_3[4,t]
}


# 
# 
true_miu_2=matrix(0,nrow=2,ncol=8)
true_miu_2[,1]=c(-1,1)
true_miu_2[,2]=c(-1.1,1.1)
true_miu_2[,3]=c(-1.2,1.2)
true_miu_2[,4]=c(-1.3,1.3)
true_miu_2[,5]=c(-1.4,1.4)
true_miu_2[,6]=c(-1.5,1.5)
true_miu_2[,7]=c(-1.6,1.6)
true_miu_2[,8]=c(-1.7,1.7)


true_state_2=matrix(0,nrow=8,ncol=time)
for(i in 1:8)
{
  true_state_2[i,]=t(as.matrix(sample(c(0,1),size = time,prob = c(0.75,0.25),replace = TRUE)))
}
 
 true_lambda_2=as.matrix(c(0.75,0.65,0.55,0.45,0.35,0.25,0.15,0.8))
# 
true_e_2=matrix(0,nrow=8,ncol=time)
true_e_2[1,1]=0.15
true_e_2[2,1]=0.25
true_e_2[3,1]=0.35
true_e_2[4,1]=0.45
true_e_2[5,1]=0.55
true_e_2[6,1]=0.65
true_e_2[7,1]=0.75
true_e_2[8,1]=0.85

for(i in 1:8)
{
  for(t in 2:time)
  {
    true_e_2[i,t]=true_miu_2[true_state_2[i,t]+1,1]+true_lambda_2[i,]*(true_e_2[i,t-1]-true_miu_2[true_state_2[i,t-1]+1,1])+rnorm(n = 1,mean=0,sd=(i/80))
  }
}

 
true_Beta=matrix(0,nrow=8,ncol=1)
true_Beta[1,1]=1.91
true_Beta[2,1]=1.82
true_Beta[3,1]=1.73
true_Beta[4,1]=1.64
true_Beta[5,1]=1.55
true_Beta[6,1]=1.6
true_Beta[7,1]=1.7
true_Beta[8,1]=1.8
# 
# 
true_H=matrix(0,nrow=8,ncol=time)


for(t in 1:time)
{
  true_H[1,t]=true_Beta[1,1]*true_G[1,t]+true_e_2[1,t]
  true_H[2,t]=true_Beta[2,1]*true_G[2,t]+true_e_2[2,t]
  true_H[3,t]=true_Beta[3,1]*true_G[3,t]+true_e_2[3,t]
  true_H[4,t]=true_Beta[4,1]*true_G[4,t]+true_e_2[4,t]
  true_H[5,t]=true_Beta[5,1]*true_G[1,t]+true_e_2[5,t]
  true_H[6,t]=true_Beta[6,1]*true_G[2,t]+true_e_2[6,t]
  true_H[7,t]=true_Beta[7,1]*true_G[3,t]+true_e_2[7,t]
  true_H[8,t]=true_Beta[8,1]*true_G[4,t]+true_e_2[8,t]
}



# 
# 
# 
# 
# 
# 
 true_lambda_1=as.matrix(rnorm(16,mean=0,sd=0.33))
# 
true_e_1=matrix(0,nrow=16,ncol=time)
for(i in 1:16)
{
  true_e_1[i,1]=i/11
}

# 
for(i in 1:16)
{
  for(t in 2:time)
  {
    true_e_1[i,t]=true_lambda_1[i,]*true_e_1[i,t-1]+rnorm(n = 1,mean=0,sd=i/16)
  }
}
# 
# 
 true_alpha=matrix(0,nrow=16,ncol=1)

for(i in 1:16)
{
  true_alpha[i,1]=i/16+1
}

# 
true_y=matrix(0,nrow=16,ncol=time)
# 
# 
for(i in 1:8)
{
  for(t in 1:time)
  {
    true_y[i,t]=true_alpha[i,1]*true_H[i,t]+true_e_1[i,t]
    true_y[i+8,t]=true_alpha[i+8,1]*true_H[i,t]+true_e_1[i+8,t]
  }
}



# plot(t(true_Factor),type='l')
# 
# #lines(true_e_1[16,],type='l',col='red')
# lines(true_G[4,],type='l',col='blue')
# lines(true_H[8,],type='l',col='green')
# lines(t(true_Factor),type='l')
# 
# plot(true_G[4,],type='l',col='blue')
# plot(true_H[8,],type='l',col='green')
# 
# plot(true_y[16,],type='l',col='red')




#数据预处理
setwd("C:/Users/Administrator/Desktop/markov switching hierarchical dynamic factors model/函数形式")

Data=rep(list(0),indices)
#读取数据
# for(i in 1:indices)
# {
#   Data[[i]] =read.xlsx(xlsxFile = "GDP.xlsx",sheet = i,detectDates = TRUE, colNames = TRUE)
# }

  Data[[1]]=cbind(true_y[1,],true_y[2,],true_y[3,],true_y[4,],true_y[5,],true_y[6,],
                true_y[7,],true_y[8,])
  Data[[2]]=cbind(true_y[9,],true_y[10,],true_y[11,],true_y[12,],true_y[13,],true_y[14,],
                true_y[15,],true_y[16,])
  
# invest=read.xlsx(xlsxFile = "GDP.xlsx",sheet = 2,detectDates = TRUE, colNames = TRUE)
# consume=read.xlsx(xlsxFile = "GDP.xlsx",sheet = 3,detectDates = TRUE, colNames = TRUE)

#去除第一列并将其作为日期列,矩阵形式：列为省，行为年
# dates=Data[[1]][,1]
# for(i in 1:indices)
# {
#   Data[[i]]=Data[[i]][,-1]
# }
# GDP=GDP[,-1]
# invest=invest[,-1]
# dates=consume[,1]
# consume=consume[,-1]

#调整为各个省的形式，每个列表都是一个省矩阵
province=rep(list(matrix(0,nrow=indices,ncol=time)),block)

for(i in 1:block)
{
  for(j in 1:indices)
  {
    province[[i]][j,]=Data[[j]][,i]
  }
}
# 
# province_mean=matrix(0,nrow=block,ncol=indices)
# 
# for(i in 1:block)
# {
#   for(j in 1:indices)
#   {
#     province_mean[i,j]=mean(province[[i]][j,])
#   }
# }
# 
# for(i in 1:block)
# {
#   for(j in 1:indices)
#   province[[i]][j,]=province[[i]][j,]-province_mean[i,j]
# }




#主成分变量作为变量初始值

##主成分分析（每一列为变量，从变量中提取）

##一个列表中有31个省的回归对象
province_principal_object=rep(list(0),block)

for(i in 1:block)
{
  province_principal_object[[i]]=fa(t(province[[i]]),nfactors=1,rotate="oblimin")
}

#一张表中有31个省每个省3个指标的主成份权重
weights_for_province=matrix(0,nrow=block,ncol=indices)

for(i in 1:block)
{
  weights_for_province[i,]=t(as.matrix(province_principal_object[[i]]$weights))
}

#省份的主成份值，一个矩阵中包含31个省的主成份序列
province_principal_variable=matrix(0,nrow=block,ncol=time)

i=1
j=1

for(i in 1:block)
{
  for(j in 1:time)
  {
    province_principal_variable[i,j] =province_principal_variable[i,j]+(province[[i]][1,j]*weights_for_province[i,1]+province[[i]][2,j]*weights_for_province[i,2])/2
    #+weights_for_province[i,3]*province[[i]][3,j] 
  }
}

# 
# plot(province_principal_variable[7,],type='l',col='red')
# lines(province[[7]][1,])
##一个列表中有4个区域的回归对象


area_principal_object=rep(list(0),area)

# east_number=c(1,2,3,9,10,11,13,15,19)
# mid_number=c(4,5,12,14,16,17,18,20,21)
# west_number=c(22:31)
# north_number=c(6:8)

east_number=c(1,5)
mid_number=c(2,6)
west_number=c(3,7)
north_number=c(4,8)


area_variable=list(matrix(0,nrow=length(east_number),ncol=time),
                             matrix(0,nrow=length(mid_number),ncol=time),
                             matrix(0,nrow=length(west_number),ncol=time),
                             matrix(0,nrow=length(north_number),ncol=time))

east=1
mid=1
west=1
north=1
for(i in 1:block)
{
  if(i%in%east_number)
  {
    area_variable[[1]][east,]=province_principal_variable[i,]
    east=east+1
  }
  else if(i%in%mid_number)
  {
    area_variable[[2]][mid,]=province_principal_variable[i,]
    mid=mid+1
  } 
  else if(i%in%west_number)
  {
    area_variable[[3]][west,]=province_principal_variable[i,]
    west=west+1
  }
  else if(i%in%north_number)
  {
    area_variable[[4]][north,]=province_principal_variable[i,]
    north=north+1
  }
}



for(i in 1:area)
{
  area_principal_object[[i]] =fa(t(area_variable[[i]]),nfactors=1,rotate="oblimin")
}

#一张表中有4个区域的主成份权重
weights_for_area=rep(list(0),area)

for(i in 1:area)
{
  weights_for_area[[i]]=as.matrix(area_principal_object[[i]]$weights)
}

#4个区域的主成份值
area_principal_variable=matrix(0,nrow=area,ncol=time)

i=1
j=1

for(i in 1:area)
{

  area_principal_variable[i,] =area_principal_variable[i,]+(area_variable[[i]][1,]*weights_for_area[[i]][1,]+
                               area_variable[[i]][2,]*weights_for_area[[i]][2,])/2
}

# 
# plot(area_principal_variable[2,],type='l',col='red')
# lines(true_G[2,])





##国家主成份回归对象
country_principal_object=list(0)



country_principal_object=fa(t(area_principal_variable[(1:area),]),nfactors=1,rotate="oblimin")

#国家主成份权重
weights_for_country=list(0)

weights_for_country=t(as.matrix(country_principal_object$weights))


#国家主成份序列
country_principal_variable=matrix(0,nrow=1,ncol=time)


country_principal_variable=country_principal_variable+(area_principal_variable[1,]*weights_for_country[,1]+
  area_principal_variable[2,]*weights_for_country[,2]+area_principal_variable[3,]*weights_for_country[,3]+
  area_principal_variable[4,]*weights_for_country[,4])/4

# plot(t(country_principal_variable),type='l',col='red')
# lines(t(true_Factor))

H=province_principal_variable

G=area_principal_variable

Factor=country_principal_variable


