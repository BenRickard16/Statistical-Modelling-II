data(stackloss)
pairs(stackloss)


fit1 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss)
summary(fit1)
#SE error of Bhat2 is 0.1349

as.numeric(c(1,50,21,85)%*%fit1$coefficients)
#Predicted value is 10.13293

fit2 <- lm(log(stack.loss)~ Air.Flow*Water.Temp + I((Air.Flow)^2), data=stackloss)
x0 = data.frame(Air.Flow = 50, Water.Temp = 21, Acid.Conc. = 85)
exp(predict(fit2, newdata=x0))
#Predicted value is 7.880239

s2 <- sqrt(sum((fit2$residuals)^2)/(21-5))
#Residual SE s=0.1314076, df=5
s1 <- sqrt(sum((fit1$residuals)^2)/(21-4))
#Residual SE s=3.243364, df=4
m<-colMeans(stackloss)
sigma<-var(stackloss)
d<- mahalanobis(stackloss,m,sigma)
which(d>qchisq(0.95,4))

par(mfrow=c(1,1))
plot(qchisq( (1:21-0.5)/21,4),sort(d))
abline(a=0,b=1)



#Simulation 2
x <- runif(100,0,3)

e<-rnorm(100,mean=0,sd=0.5)
y<- 3*x+e-1

ahat <- rep(0,500)
bhat <- rep(0,500)
shat <- rep(0,500)

for (j in 1:500){
  x <- runif(100,0,3)
  y<-3*x+rnorm(100,mean=0,sd=0.5)-1
  fit<-lm(y~x)
  shat[j]<-summary(fit)$sigma
  ahat[j]<-fit$coef[1]
  bhat[j]<-fit$coef[2]  
}

shat
ahat
bhat
mean(ahat)
mean(bhat)
mean(shat)


schi<- shat^2*(100-2)/0.5^2
plot(qchisq( (1:500-0.5)/500,98),sort(schi))
abline(0,1,lwd=2,col='red')
