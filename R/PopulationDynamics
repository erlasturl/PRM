popdy<-function(NY1h,N12h,c1,b,c2,e,d,Sc,f,I,I1){
  NY2h<-rep(NA,length(NY1h))
  NY2h[1]<-N12h
  for (i in 2:5){
    NY2h[i]<-Sc*(NY1h[i-1]*exp(-b+c1*I[i-1])+NY2h[i-1]*exp(-b+c1*I[i-1])*(e-d*I1[i]))
}
  for (i in 6:length(NY1h)){
  NY2h[i]<-Sc*(NY1h[i-1]*exp(-c2*f[i-1])+NY2h[i-1]*exp(-c2*f[i-1])*(e-d*I1[i-1]))
}
return(NY2h)
} 
