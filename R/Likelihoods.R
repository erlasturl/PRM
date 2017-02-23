lAAH<-function(NY1h,NY2h,a1h,a2h,c1,b,c2,f,I){
  Lsum<-0
  for (i in 1:5){  
  L<-lgamma(a1h[i]+a2h[i]+1)-lgamma(a1h[i]+1)-lgamma(a2h[i]+1)+ 
    a1h[i]*log(NY1h[i]*(1-exp(-b+c1*I[i]))/(NY1h[i]*(1-exp(-b+c1*I[i]))+NY2h[i]*(1-exp(-b+c1*I[i]))))+
    a2h[i]*log(NY2h[i]*(1-exp(-b+c1*I[i]))/(NY1h[i]*(1-exp(-b+c1*I[i]))+NY2h[i]*(1-exp(-b+c1*I[i]))))
  Lsum<-L+Lsum
  }
  for (i in 8:length(NY2h)){  
    L<-lgamma(a1h[i]+a2h[i]+1)-lgamma(a1h[i]+1)-lgamma(a2h[i]+1)+ 
      a1h[i]*log(NY1h[i]*(1-exp(-c2*f[i]))/(NY1h[i]*(1-exp(-c2*f[i]))+NY2h[i]*(1-exp(-c2*f[i]))))+
      a2h[i]*log(NY2h[i]*(1-exp(-c2*f[i]))/(NY1h[i]*(1-exp(-c2*f[i]))+NY2h[i]*(1-exp(-c2*f[i]))))
    Lsum<-L+Lsum
  }
  return(Lsum)
}


lAAHv<-function(NY1h,NY2h,a1v,a2v,e,d,f,I1){
  Lsum<-0
  for (i in 1:5){  
  L<-lgamma(a1v[i]+a2v[i]+1)-lgamma(a1v[i]+1)-lgamma(a2v[i]+1)+ 
    a1v[i]*log(NY1h[i]/(NY1h[i]+NY2h[i]*(e-d*I1[i])))+
    a2v[i]*log(1-NY1h[i]/(NY1h[i]+NY2h[i]*(e-d*I1[i])))
  Lsum<-L+Lsum
  }
  for (i in 6:(length(NY2h))){  
    L<-lgamma(a1v[i]+a2v[i]+1)-lgamma(a1v[i]+1)-lgamma(a2v[i]+1)+ 
      a1v[i]*log(NY1h[i]/(NY1h[i]+NY2h[i]*(e-d*I1[i])))+
      a2v[i]*log(1-NY1h[i]/(NY1h[i]+NY2h[i]*(e-d*I1[i])))
    Lsum<-L+Lsum
  }
  return(Lsum)
} 


lcatch<-function(NY1h,NY2h,c1,b,c2,h,f,I){
    Nh<-NY1h+NY2h
  Lsum<-0
  for (i in 1:5){
    L <- lgamma(Nh[i]+1) - lgamma(h[i]+1) - lgamma(Nh[i]-h[i]+1) + 
      h[i]*log((NY1h[i]*(1-exp(-b+c1*I[i]))+NY2h[i]*(1-exp(-b+c1*I[i])))/Nh[i]) + 
      (Nh[i]-h[i])*log(1-(NY1h[i]*(1-exp(-b+c1*I[i]))+NY2h[i]*(1-exp(-b+c1*I[i])))/Nh[i])
    Lsum<-L+Lsum 
  }
  for (i in 8:length(NY2h)){
    L <- lgamma(Nh[i]+1) - lgamma(h[i]+1) - lgamma(Nh[i]-h[i]+1) + 
      h[i]*log((NY1h[i]*(1-exp(-c2*f[i]))+NY2h[i]*(1-exp(-c2*f[i])))/Nh[i]) + 
      (Nh[i]-h[i])*log(1-(NY1h[i]*(1-exp(-c2*f[i]))+NY2h[i]*(1-exp(-c2*f[i])))/Nh[i])
    Lsum<-L+Lsum 
  }
  return(Lsum)  
}


lindex<-function(NY1h,NY2h,I,a,s2){
  Nh<-NY2h+NY1h
  n<-length(NY2h)
  L <- -n/2*log(2*pi)-n/2*log(s2)-1/(2*s2)*sum((I-(a*Nh))^2)
  return(L)  
} 

