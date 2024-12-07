## -----------------------------------------------------------------------------
set.seed(111)
theta=1
eta=0


f<-function(x){
  stopifnot(theta > 0)
  return(1/(theta*pi*(1+((x-eta)/theta)**2)))
}

m<-1e4
k<-0
u<-runif(m)
x<-numeric(m)

x[1]<-rnorm(1,mean = 1)
for(i in 2:m){
  xt<-x[i-1]
  y<-rnorm(1,mean = xt)
  r=f(y)*dnorm(x=xt,mean=y)/(f(xt)*dnorm(x=y,mean = xt))
  if(u[i]<=r){
    x[i]=y
  }else{
    k=k+1
    x[i]=xt
  }
}
is = 5001:5500
par(mfrow = c(1,2))
plot(is, x[is], type="l")
a<-ppoints(100)
QR<-tan(pi*(a-0.5))
Q<-quantile(x, a)
qqplot(QR, Q, main="",
        xlab="Cauty Quantiles", ylab="Sample Quantiles")
abline(0,1,col='blue',lwd=2)
hist(x, probability = TRUE, breaks = 100)
plot.x = seq(min(x), max(x), 0.01)
lines(plot.x, f(plot.x))
par(mfrow = c(1,1))

## -----------------------------------------------------------------------------
n<-100
a<-20
b<-30
f=function(x,y){
  return(factorial(n)/(factorial(x)*factorial(n-x))*(y**(x+a-1))*((1-y)**(n-x+b+1)))
}
m<-5000
X=matrix(0,nrow = m,ncol = 2)
for(i in 2:m){
  xt=X[i-1,]
  xt[1]=rbinom(1,n,xt[2])
  xt[2]=rbeta(1,xt[1]+a,n-xt[1]+b)
  X[i,]=xt
}
x<-X[1001:m,]
cat('Means: ',round(colMeans(x),2))
cat('Standard errors: ',round(apply(x,2,sd),2))
cat('Correlation coefficients: ', round(cor(x[,1],x[,2]),2))

plot(x[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(x[,2],col=2,lwd=2)
legend('bottomright',c(expression(X[1]),expression(X[2])),col=1:2,lwd=2)

