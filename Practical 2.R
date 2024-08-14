library(sm2data)
data(engine)
?engine

head(engine)
dim(engine)
names(engine)

#Exercise 12
X<-as.matrix(cbind(rep(1,46),engine[,1:2]))
Y<-engine[,3]

#Exercise 13
betahat <- solve(t(X)%*%X) %*%t(X) %*%Y
betahat

#Exercise 14
fit<-lm(NOX~CO+HC, data=engine)
fit

#Exercise 15
beta <- fit$coef
beta

s<-sqrt(sum((fit$residuals)^2)/fit$df)
s
#summary(fit)$sigma gives same answer

Sigma<-solve(t(X)%*% X)*s^2
#Sigma<-s^2*summary(fit)%cov.unscaled gives same answer

sehatbeta2<-sqrt(Sigma[2,2])
sehatbeta3<-sqrt(Sigma[3,3])

summary(fit)


#Exercise 16
beta[2]+c(-1,1)*qt(0.975,43)*sehatbeta2
beta[3]+c(-1,1)*qt(0.975,43)*sehatbeta3

confint(fit,2,level=0.95)
confint(fit,3,level=0.95)

#Exercise 17
summary(fit)
#p-value = 0.000407<0.05 hence we reject H0

t<-abs(beta[2]/sehatbeta2)
#t=3.83

qt(0.975,43)
#critical value = 2.017
#Hence reject H0 as 3.83>2.017

confint(fit,2)
#CI [-0.135,-0.0421] doesn't contain 0 so reject H0

#Exercise 18
t<-abs((1-beta[3]/sehatbeta3))
t

confint(fit,3)

#Exercsie 19
test.s<-function(lmobject, sigmo){
  n<-length(lmobject$res)
  p<-length(lmobject$coef)
  s<-summary(lmobject)$sigma
  v<-s^2/sigmo^2*(n-p)
  pvalue<- 1 - pchisq(v, n-p)
  return(pvalue)
}
  
test.s(fit, 0.30) 
    
#Exercise 20
predict(fit,newdata=data.frame("CO"=12.2, "HC"=0.4))

predict(fit,newdata=data.frame("CO"=12.2, "HC"=0.4), interval = "confidence")

predict(fit,newdata=data.frame("CO"=12.2, "HC"=0.4), interval = "prediction")
