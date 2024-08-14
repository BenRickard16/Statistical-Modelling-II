library(sm2data)
data(missile)

#Exercise 29
summary(missile)
missile.lm <- lm(Battery.Life ~ Temperature*Material, data=missile)
summary(missile.lm)

#Exercise 30
anova(missile.lm)
anova(Battery.Life ~ Material*Temperature, data=missile)

#Exercise 31
missile.material <- lm(Battery.Life ~ Material, data=missile)
missile.materialtemp <- lm(Battery.Life ~ Material+Temperature, data=missile)
anova(missile.material, missile.materialtemp)

#Exercise 32
anova(lm(Battery.Life ~ Material*Temperature, data=missile))

sse <- 18231
ssr <- 10684+39119+9614
Rsquared <- ssr/(ssr+sse)
summary(missile.lm)


#Cement Data Set
library(MASS)
data(cement, package='MASS')
cement=cement[,c(5,1,2,3,4)]
names(cement)=c("heat", "aluminate", "tri.silicate", "ferite", "di.silicate")

#Exercise 33
cement.fit.full <- lm(heat ~ aluminate+tri.silicate+ferite+di.silicate, data=cement)
s<-summary(cement.fit.full)$sigma


#Exercise 34
anova(cement.fit.full)
anova(lm(heat ~ di.silicate+aluminate+tri.silicate+ferite, data=cement))
anova(lm(heat ~ ferite+aluminate+di.silicate+tri.silicate, data=cement))

#Exercise 35
CI <- function(model, var){
  pI <- length(model$coef)
  n <- length(model$residuals)
  RSSI <- sum((model$residuals)^2)
  ci <- RSSI/var+2*pI-n
  return(ci)
}

#Exercise 36
cement.fit.1.3 <- lm(heat~., cement[,c(1,2,4)])
CI(cement.fit.1.3,s^2)

cement.fit.1.3.4 <- lm(heat~., cement[,c(1,2,4,5)])
CI(cement.fit.1.3.4,s^2)