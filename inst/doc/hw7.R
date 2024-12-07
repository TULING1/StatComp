## -----------------------------------------------------------------------------
a<-c(1,2)
k_term = function(a,k){
  d<-length(a)
  return((-1)^k * exp((2*k+2)*log(norm(a,type="2"))-lgamma(k+1)-k*log(2)-log(2*k+1)-log(2*k+2)+lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1)))
}
n<-500
z<-numeric(n)
thesum = function(a){
  for (i in 1:n) {
    z[i]<-k_term(a,i-1)
  }
  return(sum(z))
}
thesum(a)

## -----------------------------------------------------------------------------
m <- c(5:25, 100, 500, 1000)
n <- c(4:25, 100)
A <- numeric(24)
B <- numeric(23)

for (k in m) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}

g <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2),lower = sqrt((a^2)*(k-1)/(k-a^2)), upper = Inf)$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), lower = sqrt((a^2)*k/(k+1-a^2)), upper = Inf)$value))
}
A[k] <- uniroot(g, lower = 0.01, upper = 1+sqrt(k)/2)$root
}

for (k in n) {
f <- function(x){
       return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
}
h <- function(a){
       return((f(k)*integrate(function(x)(1 + (x^2)/(k-1))^(-k/2), lower = 0, upper = sqrt((a^2)*(k-1)/(k-a^2)))$value) - (f(k+1)*integrate(function(x)(1 + (x^2)/k)^(-(k+1)/2), lower = 0, upper = sqrt((a^2)*k/(k+1-a^2)))$value))
}
B[k] <- uniroot(h, lower = 0.01, upper = 1+sqrt(k)/2)$root
}

A <- c(NA, A[m])
B <- c(B[n], NA, NA)
k <- c(4:25, 100, 500, 1000)
result <- data.frame(k, A, B)
knitr::kable(result)

## -----------------------------------------------------------------------------
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85) 
tau <- 1
censored <- Y == tau 
n <- length(Y)  

lambda <- 1 

tolerance <- 1e-6  
max_iter <- 1000  
for (iter in 1:max_iter) {
  Y_complete <- Y
  Y_complete[censored] <- tau + 1 / lambda
  lambda_new <- mean(1/Y_complete)
  
  if (abs(lambda - lambda_new) < tolerance) {
    cat("steps:", iter, "\n")
    break
  }

  lambda <- lambda_new
}


cat("lambda：", lambda, "\n")

lambda_mle_observed <- mean(1/Y)
cat("MLE：", lambda_mle_observed, "\n")


