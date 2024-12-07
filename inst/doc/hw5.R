## -----------------------------------------------------------------------------
library(bootstrap)
sigma<-cov(scor)
eigenvalue<-eigen(sigma)
value<-eigenvalue$values[order(-(eigenvalue$values))]
theta<-value[1]/sum(value)

n<-88
theta.hat<-numeric(n)
for(i in 1:n){
  jack.scor<-scor[-i,]
  sigma.hat<-cov(jack.scor)
  ss<-eigen(sigma.hat)
  vv<-ss$values[order(-(ss$values))]
  theta.hat[i]<-vv[1]/sum(vv)
}
jack.bias<-(n-1)*(mean(theta.hat)-theta)
jack.se<-sqrt((n-1)*mean((theta.hat-theta)**2))
print(round(c(jackknife.estimates.of.bias=jack.bias,jackknife.estimates.of.standerd.error=jack.se),4))

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n<-length(magnetic)   
e1<-e2<-e3<-e4<-numeric(n)
yhat1<-yhat2<-yhat3<-yhat4<-numeric(n)
SSR1<-SSR2<-SSR3<-SSR4<-SST1<-SST2<-SST3<-SST4<-numeric(n)
ybar<-mean(magnetic)
for (k in 1:n) {
  y<-magnetic[-k]
  x<-chemical[-k]

  J1<-lm(y ~ x)
  yhat1[k]<-J1$coef[1]+J1$coef[2]*chemical[k]
  e1[k]<-magnetic[k]-yhat1[k]
  SSR1[k]<-(yhat1[k]-ybar)^2
  SST1[k]<-(magnetic[k]-ybar)^2

  J2<-lm(y ~ x + I(x^2)) 
  yhat2[k]<-J2$coef[1]+J2$coef[2]*chemical[k]+
    J2$coef[3]*chemical[k]^2
  e2[k]<-magnetic[k]-yhat2[k]
  SSR2[k]<-(yhat2[k]-ybar)^2
  SST2[k]<-(magnetic[k]-ybar)^2

  J3<-lm(log(y) ~ x) 
  logyhat3<-J3$coef[1]+J3$coef[2]*chemical[k]
  yhat3[k]<-exp(logyhat3)
  e3[k]<-magnetic[k]-yhat3[k]
  SSR3[k]<-(yhat3[k]-ybar)^2
  SST3[k]<-(magnetic[k]-ybar)^2

  J4<-lm(y ~ x+I(x^2)+I(x^3)) 
  yhat4[k]<-J4$coef[1]+J4$coef[2]*chemical[k] +
  J4$coef[3]*chemical[k]^2+J4$coef[4]*chemical[k]^3
  e4[k]<-magnetic[k]-yhat4[k]
  SSR4[k]<-(yhat4[k]-ybar)^2
  SST4[k]<-(magnetic[k]-ybar)^2
}
print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))

print(c(51/50*sum(SSR1)/sum(SST1),51/49*sum(SSR2)/sum(SST2),51/50*sum(SSR3)/sum(SST3),51/48*sum(SSR4)/sum(SST4))) 

## ----eval = FALSE-------------------------------------------------------------
#  
#  library(RVAideMemoire)
#  attach(chickwts)
#  
#  x1=sort(weight[feed =="soybean"])
#  ys=sort(weight[feed == "linseed"])
#  xs=sample(x1, size = length(ys))
#  rep=1000
#  
#  zs=c(xs, ys)
#  n1=length(xs)
#  n=n1*2
#  
#  Ts=numeric(rep)
#  for (i in 1:rep) {
#    ks=sample(1:n, n1, replace = FALSE)
#    zs1=zs[ks]
#    zs2=zs[-ks]
#    Ts[i]=cramer.test(zs1, zs2)$statistic
#  }
#  
#  (cvm=cramer.test(x, y))
#  T.hat=cvm$statistic
#  (p.hat=mean(abs(T.hat) < abs(Ts)))
#  
#  hist(Ts)

## -----------------------------------------------------------------------------
soybean=chickwts$weight[chickwts$feed=="soybean"]
linseed =chickwts$weight[chickwts$feed=="linseed"]
n=length(soybean)
m=length(linseed)

tmp=min(n, m)
soybean=sort(soybean[1:tmp])
linseed=sort(linseed[1:tmp])

zs=c(soybean, linseed)
spearman.cor.test=cor.test(x = soybean, y = linseed, method = "spearman")

B=1000
k=length(zs)
rep<-1000
rhos=numeric(rep)

for(b in 1:B) {
  i=sample(1:k, k/2, replace = FALSE)
  xs=zs[i]
  ys=zs[-i]
  rhos[b]=cor(x = xs, y = ys, method = "spearman")
}

hist(rhos, breaks = 100)
(theta.hat=spearman.cor.test$estimate)
spearman.cor.test$p.value
(p.hat=mean(abs(rhos) > abs(theta.hat)))
(alpha=0.05)

