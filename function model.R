
y=alpha*H+e_1
H=Beta*G+e_2
G=theta*Fator+e_3

e_1[,t]=lambda_1*e_1[,t-1]+eposilon_1
e_2[,t]-miu_2[state_2[,t]+1,]=lambda_2*(e_2[,t-1]-miu_2[state_2[,t-1]+1,])+eposilon_2
e_3[,t]-miu_3[state_3[,t]+1,]=lambda_3*(e_3[,t-1]-miu_3[state_3[,t-1]+1,])+eposilon_3
Factor[,t]-miu[state_F[,t]+1,]=lambda*(Factor[,t-1]-miu[state_F[,t-1]+1,])+eposilon



