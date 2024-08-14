library(sm2data)
data(scallops)

dim(scallops)
names(scallops)

scallops[1:6,]
head(scallops)

#Exercise 1
m<-colMeans(scallops[c('long','lat')])
Sigma<-var(scallops[c('long','lat')])
eigen(Sigma)

#Exercise 2
plot(scallops$long,scallops$lat)
points(m[1],m[2], col=2, pch="+")

#Exercise 3
hist(scallops$tcatch)
hist(scallops$y)

#Exercise 4
x<-seq(-74,-71, length=15)
y<-seq(38,41, length=15)


dens<-matrix(0,15,15)

for (i in 1:15){
  for (j in 1:15){
    dens[i,j]<-1/(2*pi*sqrt(det(Sigma)))*exp(-1/2*c(x[i]-m[1], y[j]-m[2])%*%solve(Sigma)%*%c(x[i]-m[1], y[j]-m[2]))
  }
}
dens

#Exercise 5
par(mfrow=c(1,1))
contour(x,y,dens)
points(scallops$long, scallops$lat)


library(sm2data)
data(engine)
head(engine)


#Exercise 6
M <- colMeans(engine)
M <- round(M, 2)
M

S <- var(engine)
S <- round(S,2)
S

#Exercise 7
pairs(engine)

win.graph()
plot(engine$CO, engine$HC)
identify(engine$CO, engine$HC)

win.graph()
plot(engine$CO, engine$NOX)
identify(engine$CO, engine$NOX)

win.graph()
plot(engine$HC, engine$NOX)
identify(engine$HC, engine$NOX)

#Exercise 8
d<-mahalanobis(engine,M,S)
d[5]

#Exercise 9
which(d> qchisq(0.95,3))
which(d> qchisq(0.975,3))

#Exercise 10
plot(qchisq((1:46-0.5)/46,3),sort(d))
abline(a=0,b=1)

#Exercise 11
which(d> qchisq(1-0.5/46,3))
