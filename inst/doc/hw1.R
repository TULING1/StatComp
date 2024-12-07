## -----------------------------------------------------------------------------
set.seed(122)
n<- 1000
t1<-1 #σ's value
t2<-2
t3<-3
u<-runif(n,min=0,1-0.0001)
x1<-sqrt(-2*(t1**2)*log(1-u))
x2<-sqrt(-2*(t2**2)*log(1-u))
x3<-sqrt(-2*(t3**2)*log(1-u))
y<-seq(0,10,.01)
par(mfrow=c(1,3))
hist(x1,prob=TRUE,breaks=20,main ='σ=1')
lines(y,y/(t1**2)*exp(-y**2/(2*t1**2)))
hist(x2,prob=TRUE,breaks=20,main ='σ=2')
lines(y,y/(t2**2)*exp(-y**2/(2*t2**2)))
hist(x3,prob=TRUE,breaks=20,main ='σ=3')
lines(y,y/(t3**2)*exp(-y**2/(2*t3**2)))

## -----------------------------------------------------------------------------
set.seed(123)
p11=0.75
p21=0.5
p31=0.25
p12=1-p11
p22=1-p21
p32=1-p31
n<-1e4
x1<-rnorm(n,0,1)
x2<-rnorm(n,3,1)
r1<-sample(c(0,1),size = n,replace = TRUE,prob = c(p12,p11))
r2<-sample(c(0,1),size = n,replace = TRUE,prob = c(p22,p21))
r3<-sample(c(0,1),size = n,replace = TRUE,prob = c(p32,p31))
z1<-r1*x1+(1-r1)*x2
z2<-r2*x1+(1-r2)*x2
z3<-r3*x1+(1-r3)*x2
par(mfrow=c(1,3))
hist(z1,breaks = 50,main ="p1=0.75")
hist(z2,breaks = 50,main ="p1=0.5")
hist(z3,breaks = 50,main ="p1=0.25")

## -----------------------------------------------------------------------------
size<-1e4
t=10
lambda =3#the parameter of poisson process
shape<-1
scale<-1#the parameters of Gamma distribution
n<-qpois(1-1e-9,lambda =lambda*t)#the largest number which the poisson process can get
poip=function(z){
  
}
xn<-rpois(size,lambda*t)
xs<-c()
for(i in 1:size){
  yi=cumsum(c(rgamma(n,shape = shape,scale = scale)))
  xs<-c(xs,yi[xn[i]])
}
print("Mean and Variance of the sample")
print(mean(xs))
print(var(xs))
print("Theoretical mean and variance")
print(lambda*t*shape*scale)
print(lambda*t*(shape+1)*shape*scale**2)

