gemstones <- read.table(file="http://tmaturi.github.io/sm2/gemstones.txt", header=T, sep='\t')

dim(gemstones)
colMeans(gemstones)

#Regression Analysis 1
fit1 <- lm(value ~ weight+clarity+colour, data=gemstones)

summary(fit1)
#p-value for colour is 0.226 so not significant
anova(fit1)
#p-value for colour is 0.227 so not significant again

predict(fit1, newdata=data.frame('weight'=9.8,'clarity'=1.0,'colour'=5), interval='prediction',level= 0.99)

#Regression Analysis 2
fit2 <- lm(value~weight*clarity, data=gemstones)
summary(fit2)
#p-value for interaction term <0.1 but for main effects >0.1

gemstones$weight.centred <- gemstones$weight - mean(gemstones$weight)
gemstones$clarity.centred <- gemstones$clarity - mean(gemstones$clarity)

fit3 <- lm(value ~ weight.centred*clarity.centred, data=gemstones)
summary(fit3)
#Now p-values for maine effects and interaction term <0.1

#Analysis of Variation and Model Selection
mod1 <- lm(value~weight+clarity, data=gemstones)
mod2 <- lm(value~weight+clarity+colour, data=gemstones)

anova(mod1,mod2)
anova(mod2,mod1)

#Tukey's rule
rss1 <- sum(summary(mod1$residual^2))
rss2 <- sum(summary(mod2$residual^2))

T1 <- rss1/(120-3)^2
T2 <- rss2/(120-4)^2

#Mallow's CI
CI<-function(model, var){
  pI <- length(model$coef)
  n  <- length(model$residuals)
  RSSI <- sum((model$residuals)^2)
  ci <- RSSI/var+2*pI-n
  return(ci)
}

s <- summary(fit1)$sigma

gem.fit.cl.col <- lm(value~clarity+colour, data=gemstones)
gem.fit.w.col <- lm(value~weight+colour, data=gemstones)
gem.fit.w.cl <- lm(value~weight+clarity, data=gemstones)

CI(fit1,s^2)
CI(gem.fit.cl.col, s^2)
CI(gem.fit.w.col, s^2)
CI(gem.fit.w.cl, s^2)

#Remove term colour

gem.fit.cl <- lm(value~clarity, data=gemstones)
gem.fit.w <- lm(value~weight, data=gemstones)

CI(gem.fit.cl, s^2)
CI(gem.fit.w, s^2)

#Do not remove any terms

