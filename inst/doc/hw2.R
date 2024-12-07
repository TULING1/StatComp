## -----------------------------------------------------------------------------
n=10000
Beta.cdf = function(x){
  y=runif(n)
  return(mean(dbeta(x*y,3,3))*x)
}
t<-seq(0.1,0.9,0.1)
x1<-sapply(t,function(t) pbeta(t,3,3))
x2<-sapply(t,function(t) Beta.cdf(t))
round(rbind(x1,x2),4)

## -----------------------------------------------------------------------------
n=10000
a = 2#value of σ
pdf1.Rayleigh = function(x){
  u1=runif(n/2)
  return(a*(-2*log(1-u1))**0.5)
}
pdf2.Rayleigh = function(x){
  u2=runif(n/2)
  return(a*(-2*log(u2))**0.5)
}

## -----------------------------------------------------------------------------
g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
x = seq(0,10,0.1)
y = g(x)
plot(x,y,type="l")


## -----------------------------------------------------------------------------
g = function (x) {
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
x = seq(1,10,0.1)
y = g(x)
plot(x,y,type="l",ylim=c(0,1))
y_norm<-dnorm(x,mean = 1.5, sd= 1)
y_weibull<-dweibull(x,shape = 2,scale = 2)
lines(x,y_norm,col="red",ylim=c(0,1))
lines(x,y_weibull,col="blue",ylim=c(0,1))
sd_norm<-sd(y/y_norm)
sd_weibull<-sd(y/y_weibull)
rbind(sd_norm,sd_weibull)

## ----chunk-name, eval=FALSE, echo=TRUE,results='asis'-------------------------
#  n=10000
#  t<-c(n,2*n,4*n,6*n,8*n)
#  
#  ##快速排序算法
#  quicksort=function(x){
#      if(length(x)<=1)return(x)
#      x0<-x[1]
#      loc<-1
#      low<-1
#      n<-length(x)
#      high<-n
#      while(low!=high){
#          if(loc==low){
#              if(x[high]<x[loc]){
#                  tmp<-x[high]
#                  x[high]<-x[loc]
#                  x[loc]<-tmp
#                  low=low+1
#                  loc=high
#              }else{
#                      high=high-1
#                  }
#              }else{
#                  if(x[low]>x[loc]){
#                      tmp<-x[low]
#                      x[low]<-x[loc]
#                      x[loc]<-tmp
#                      high=high-1
#                      loc=low
#                  }else{
#                      low=low+1
#                  }
#             }
#          }
#      L=c()
#      R=c()
#      if(low>1) L=x[1:(low-1)]
#      if(low<length(x)) R=x[(low+1):n]
#      return(c(quicksort(L),x[low],quicksort(R)))
#  }
#  
#  time<-numeric(5)
#  k<-0
#  for(i in t){
#    k<-k+1
#    random_sample=sample(1:n,i,replace=TRUE)
#    x<-numeric(100)
#    for(j in 1:100){
#      start<-Sys.time()
#      y<-quicksort(random_sample)
#      end<-Sys.time()
#      x[j]<-(end-start)
#    }
#    time[k]<-mean(x)
#  }
#  ##回归分析
#  x_log_x<-t*log(t)
#  model<-lm(time ~ x_log_x)
#  y_pred<-predict(model)
#  polt_data<-data.frame(x_log_x=x_log_x,y=time,y_pred=y_pred)
#  ggplot(plot_data,aes(x=x_log_x))+
#    geom_point(aes(y=y),color="blue")+
#    geom_line(aes(y=y_pred),color="red") +
#    labs(title="Regression of y on x * log(x)",
#         x="x * log(x)",
#         y="y")+
#    theme_minimal()

