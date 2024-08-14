#Exercise 21
insects0 <- c(10,7,20,14,14,12,10,23,17,20,14,13,11,17,21,11,16,14,17,17,19,21,7,13,0,1,7,2,3,1,2,1,3,0,1,4,3,5,12,6,4,3,5,5,5,5,2,4,3,5,3,5,3,6,1,1,3,2,6,4,11,9,15,22,15,16,13,10,26,26,24,13)
insects<- data.frame("spray"= c(rep("A",12), rep("B",12), rep("C",12),rep("D",12),rep("E",12), rep("F",12)), "insects" = insects0)

insects$spray = as.factor(insects$spray)

fit1.insects <- lm(insects~spray, data=insects)

summary(fit1.insects)

model.matrix(fit1.insects)
#Spray A is the reference

#Exercise 22
insects$spray2<- relevel(insects$spray, ref='F')
fit2.insects <- lm(insects~spray2, data=insects)

summary(fit2.insects)

model.matrix(fit2.insects)

#Exercise 23
predict(fit1.insects, newdata=data.frame(spray='C'), interval='prediction')

predict(fit2.insects, newdata=data.frame(spray2='C'), interval='prediction')

#Exercise 25
library(sm2data)
data(missile)
missile$Temperature<-factor(missile$Temperature, levels=c('Low','Medium','High'))

#Exercise 26
missile.lm<-lm(Battery.Life~Temperature*Material, data=missile)
summary(missile.lm)

#Exercise 27
interaction.plot( missile$Temperature, missile$Material,missile$Battery.Life)

#Exercise 28
new<-data.frame(Temperature=c('Low','Low','High'),Material=c("1","2","1"))

predict(missile.lm, newdata=new, interval='confidence', level=0.99)

predict(missile.lm, newdata=new, interval='prediction', level=0.99)
