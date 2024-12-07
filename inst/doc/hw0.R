## -----------------------------------------------------------------------------
data(precip)
hist(precip,
     main = "美国城市年降水量",
     xlab = "降水量",
     ylab = "频数"
     )


## -----------------------------------------------------------------------------
data(airmiles)
plot(airmiles)

## -----------------------------------------------------------------------------
df<-data.frame(Year = as.numeric(time(airmiles)),Miles = as.numeric(airmiles))
model <- lm(Miles~Year,data =df)
plot(df$Year, 
     df$Miles, 
     main="Air Miles Over Time",
     xlab="Year", 
     ylab="Miles",
     col="blue"
     )
abline(model, col="red")

## -----------------------------------------------------------------------------
x<-rweibull(10000,shape=1,scale=0.06)
hist(x,
     xlab='频数',
     ylab='取值',
     density=10,
     nclass=100)

