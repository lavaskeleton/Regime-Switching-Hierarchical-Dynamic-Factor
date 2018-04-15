for(i in 1:area)
{
  G[i,]=Theta[i,]*Factor+e_3[i,]
}

east=1
mid=1
west=1
north=1

for(i in 1:block)
{

    if(i%in%east_number)
    {
      H[i,]=Beta[[1]][east,]*G[1,t]+e_2[i,]
    }else if(i%in%mid_number)
    {
      H[i,]=Beta[[2]][mid,]*G[2,t]+e_2[i,]
    }else if(i%in%west_number)
    {
      H[i,]=Beta[[3]][west,]*G[3,t]+e_2[i,]
    }else if(i%in%north_number)
    {
      H[i,]=Beta[[4]][north,]*G[4,t]+e_2[i,]
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


