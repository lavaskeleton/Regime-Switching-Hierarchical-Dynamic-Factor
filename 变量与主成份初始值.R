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
# set.seed(666)
# true_miu=matrix(c(-1,1),nrow=2,ncol=1)
# true_state=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.8,0.2),replace = TRUE)))
# true_lambda=0.8
# true_Factor=matrix(0,nrow=1,ncol=60)
# true_Factor[,1]=0.1
# for(t in 2:60)
# {
#   true_Factor[,t]=true_miu[true_state[,t]+1,]+true_lambda*(true_Factor[,t-1]-true_miu[true_state[,t-1]+1,])+rnorm(n = 1,mean=0,sd=1)
# }
# 
# 
# true_miu_3=matrix(0,nrow=2,ncol=2)
# true_miu_3[,1]=c(-2,2)
# true_miu_3[,2]=c(-3,3)
# 
# true_state_3=matrix(0,nrow=2,ncol=60)
# true_state_3[1,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.7,0.3),replace = TRUE)))
# true_state_3[2,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.7,0.3),replace = TRUE)))
# 
# true_lambda_3=as.matrix(c(0.7,0.6))
# 
# true_e_3=matrix(0,nrow=2,ncol=60)
# true_e_3[1,1]=0.1
# true_e_3[2,1]=0.2
# for(t in 2:60)
# {
#   true_e_3[1,t]=true_miu_3[true_state_3[1,t]+1,1]+true_lambda_3[1,]*(true_e_3[1,t-1]-true_miu_3[true_state_3[1,t-1]+1,1])+rnorm(n = 1,mean=0,sd=2)
#   true_e_3[2,t]=true_miu_3[true_state_3[2,t]+1,2]+true_lambda_3[2,]*(true_e_3[2,t-1]-true_miu_3[true_state_3[2,t-1]+1,2])+rnorm(n = 1,mean=0,sd=3)
# }
# 
# 
# true_theta=matrix(0,nrow=2,ncol=1)
# true_theta[1,1]=2
# true_theta[2,1]=3
# 
# true_G=matrix(0,nrow=2,ncol=60)
# 
# for(t in 1:60)
# {
#   true_G[1,t]=true_theta[1,1]*true_Factor[,t]+true_e_3[1,t]
#   true_G[2,t]=true_theta[2,1]*true_Factor[,t]+true_e_3[2,t]
# }
# 
# 
# true_miu_2=matrix(0,nrow=2,ncol=4)
# true_miu_2[,1]=c(-2.5,2.5)
# true_miu_2[,2]=c(-3.5,3.5)
# true_miu_2[,3]=c(-1.5,1.5)
# true_miu_2[,4]=c(-0.5,0.5)
# 
# true_state_2=matrix(0,nrow=4,ncol=60)
# for(i in 1:4)
# {
#   true_state_2[i,]=t(as.matrix(sample(c(0,1),size = 60,prob = c(0.75,0.25),replace = TRUE)))
# }
# 
# true_lambda_2=as.matrix(c(0.75,0.65,0.55,0.45))
# 
# true_e_2=matrix(0,nrow=4,ncol=60)
# true_e_2[1,1]=0.15
# true_e_2[2,1]=0.25
# true_e_2[3,1]=0.3
# true_e_2[4,1]=0.4
# 
# for(i in 1:4)
# {
#   for(t in 2:60)
#   {
#     true_e_2[i,t]=true_miu_2[true_state_2[i,t]+1,1]+true_lambda_2[i,]*(true_e_2[i,t-1]-true_miu_2[true_state_2[i,t-1]+1,1])+rnorm(n = 1,mean=0,sd=true_miu_2[2,i])
#   }
# }
# 
# 
# true_Beta=matrix(0,nrow=4,ncol=1)
# true_Beta[1,1]=1.25
# true_Beta[2,1]=2.25
# true_Beta[3,1]=1.75
# true_Beta[4,1]=0.75
# 
# 
# true_H=matrix(0,nrow=4,ncol=60)
# 
# 
# for(t in 1:60)
# {
#   true_H[1,t]=true_Beta[1,1]*true_G[1,t]+true_e_2[1,t]
#   true_H[2,t]=true_Beta[2,1]*true_G[1,t]+true_e_2[2,t]
#   true_H[3,t]=true_Beta[3,1]*true_G[2,t]+true_e_2[3,t]
#   true_H[4,t]=true_Beta[4,1]*true_G[2,t]+true_e_2[4,t]
# }
# 
# 
# 
# 
# 
# 
# true_lambda_1=as.matrix(c(0.72,0.53,0.44,0.12,0.22,0.32,0.5,0.8))
# 
# true_e_1=matrix(0,nrow=8,ncol=60)
# true_e_1[1,1]=0.15
# true_e_1[2,1]=0.25
# true_e_1[3,1]=0.3
# true_e_1[4,1]=0.4
# true_e_1[5,1]=0.5
# true_e_1[6,1]=0.55
# true_e_1[7,1]=0.45
# true_e_1[8,1]=0.31
# 
# for(i in 1:8)
# {
#   for(t in 2:60)
#   {
#     true_e_1[i,t]=true_lambda_1[i,]*true_e_1[i,t-1]+rnorm(n = 1,mean=0,sd=i/2+0.6)
#   }
# }
# 
# 
# true_alpha=matrix(0,nrow=8,ncol=1)
# true_alpha[1,1]=1.25
# true_alpha[2,1]=2.25
# true_alpha[3,1]=1.75
# true_alpha[4,1]=3.2
# true_alpha[5,1]=4.8
# true_alpha[6,1]=2.1
# true_alpha[7,1]=2.65
# true_alpha[8,1]=0.75
# 
# true_y=matrix(0,nrow=8,ncol=60)
# 
# 
# for(t in 1:60)
# {
#   true_y[1,t]=true_alpha[1,1]*true_H[1,t]+true_e_1[1,t]
#   true_y[2,t]=true_alpha[2,1]*true_H[2,t]+true_e_1[2,t]
#   true_y[3,t]=true_alpha[3,1]*true_H[3,t]+true_e_1[3,t]
#   true_y[4,t]=true_alpha[4,1]*true_H[4,t]+true_e_1[4,t]
#   true_y[5,t]=true_alpha[5,1]*true_H[1,t]+true_e_1[5,t]
#   true_y[6,t]=true_alpha[6,1]*true_H[2,t]+true_e_1[6,t]
#   true_y[7,t]=true_alpha[7,1]*true_H[3,t]+true_e_1[7,t]
#   true_y[8,t]=true_alpha[8,1]*true_H[4,t]+true_e_1[8,t]
# }

#数据预处理
setwd("C:/Users/Administrator/Desktop/markov switching hierarchical dynamic factors model/函数形式")

#读取数据
GDP=read.xlsx(xlsxFile = "GDP.xlsx",sheet = 1,detectDates = TRUE, colNames = TRUE)
invest=read.xlsx(xlsxFile = "GDP.xlsx",sheet = 2,detectDates = TRUE, colNames = TRUE)
consume=read.xlsx(xlsxFile = "GDP.xlsx",sheet = 3,detectDates = TRUE, colNames = TRUE)

#去除第一列并将其作为日期列,矩阵形式：列为省，行为年
GDP=GDP[,-1]
invest=invest[,-1]
dates=consume[,1]
consume=consume[,-1]

#调整为各个省的形式，每个列表都是一个省矩阵
province=rep(list(matrix(0,nrow=3,ncol=nrow(GDP))),ncol(GDP))

for(i in 1:ncol(GDP))
{

    province[[i]][1,]=GDP[,i]
    province[[i]][2,]=invest[,i]
    province[[i]][3,]=consume[,i]
}







#主成分变量作为变量初始值

##主成分分析（每一列为变量，从变量中提取）

##一个列表中有31个省的回归对象
province_principal_object=rep(list(0),ncol(GDP))

for(i in 1:ncol(GDP))
{
  province_principal_object[[i]]=principal(t(province[[i]]),nfactors=1,rotate="oblimin")
}

#一张表中有31个省每个省3个指标的主成份权重
weights_for_province=matrix(0,nrow=ncol(GDP),ncol=3)

for(i in 1:ncol(GDP))
{
  weights_for_province[i,]=t(as.matrix(province_principal_object[[i]]$weights))
}

#省份的主成份值，一个矩阵中包含31个省的主成份序列
province_principal_variable=matrix(0,nrow=ncol(GDP),ncol=nrow(GDP))

i=1
j=1

for(i in 1:ncol(GDP))
{
  for(j in 1:nrow(GDP))
  {
    province_principal_variable[i,j] =province_principal_variable[i,j]+weights_for_province[i,1]*province[[i]][1,j] +weights_for_province[i,2]*province[[i]][2,j] +weights_for_province[i,3]*province[[i]][3,j] 
  }
}






##一个列表中有4个区域的回归对象
area_number=4

area_principal_object=rep(list(0),4)
east_number=c(1,2,3,9,10,11,13,15,19)
mid_number=c(4,5,12,14,16,17,18,20,21)
west_number=c(22:31)
north_number=c(6:8)

east_province=matrix(0,nrow=length(east_number),ncol=nrow(GDP))
mid_province=matrix(0,nrow=length(mid_number),ncol=nrow(GDP))
west_province=matrix(0,nrow=length(west_number),ncol=nrow(GDP))
north_province=matrix(0,nrow=length(north_number),ncol=nrow(GDP))

i=1
east=1
mid=1
west=1
north=1
for(i in 1:ncol(GDP))
{
  if(i%in%east_number)
  {
    east_province[east,]=province_principal_variable[i,]
    east=east+1
  }
  else if(i%in%mid_number)
  {
    mid_province[mid,]=province_principal_variable[i,]
    mid=mid+1
  } 
  else if(i%in%west_number)
  {
    west_province[west,]=province_principal_variable[i,]
    west=west+1
  }
  else if(i%in%north_number)
  {
    north_province[north,]=province_principal_variable[i,]
    north=north+1
  }
}




area_principal_object[[1]] =principal(t(east_province),nfactors=1,rotate="oblimin")

area_principal_object[[2]]=principal(t(mid_province),nfactors=1,rotate="oblimin")

area_principal_object[[3]]=principal(t(west_province),nfactors=1,rotate="oblimin")

area_principal_object[[4]]=principal(t(north_province),nfactors=1,rotate="oblimin")

#一张表中有4个区域的主成份权重
weights_for_area=rep(list(0),4)

for(i in 1:4)
{
  weights_for_area[[i]]=as.matrix(area_principal_object[[i]]$weights)
}

#4个区域的主成份值
area_principal_variable=matrix(0,nrow=4,ncol=nrow(GDP))

i=1
j=1

for(i in 1:4)
{
    area_principal_variable[1,] =area_principal_variable[1,]+t(weights_for_area[[1]])%*%east_province
    area_principal_variable[2,] =area_principal_variable[2,]+t(weights_for_area[[2]])%*%mid_province
    area_principal_variable[3,] =area_principal_variable[3,]+t(weights_for_area[[3]])%*%west_province
    area_principal_variable[4,] =area_principal_variable[4,]+t(weights_for_area[[4]])%*%north_province

}









##国家主成份回归对象
country_principal_object=list(0)




country_principal_object=principal(t(area_principal_variable[(1:4),]),nfactors=1,rotate="oblimin")

#国家主成份权重
weights_for_country=list(0)

weights_for_country=t(as.matrix(country_principal_object$weights))


#国家主成份序列
country_principal_variable=matrix(0,nrow=1,ncol=nrow(GDP))


country_principal_variable=country_principal_variable+weights_for_country%*%as.matrix(area_principal_variable)





