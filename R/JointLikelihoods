brom<-function(logpar,dat){
  a1v<-dat$a1v;a2v<-dat$a2v
  a1h<-dat$a1h;a2h<-dat$a2h
  h<-dat$h;f<-dat$f
  I<-dat$I;I1<-dat$I1;I1p<-dat$I1p
  yrs<-length(I)
  par<-exp(logpar)
  NY1h<-par[1:yrs]
  N12h<-par[yrs+1]
  c1<-par[yrs+2]
  c2<-par[yrs+3]
  Sc<-par[yrs+4]
  a<-par[yrs+5];s2<-par[yrs+6]
  b<-par[yrs+7]
  e<-par[yrs+8];d<-par[yrs+9]
  NY2h<-popdy(NY1h,N12h,c1,b,c2,e,d,Sc,f,I,I1)
  like1<- lcatch(NY1h,NY2h,c1,b,c2,h,f,I)
  like2<- lAAH(NY1h,NY2h,a1h,a2h,c1,b,c2,f,I)
  #like3<- lradio(c,T1,T2,h1,h2,f)
  like4<-lindex(NY1h,NY2h,I,a,s2)
  like5 <- lAAHv(NY1h,NY2h,a1v,a2v,e,d,f,I1)
  logL<- -1*(like1+like2+like4+like5)
  return(logL)
}
