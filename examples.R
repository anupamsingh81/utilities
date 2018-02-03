x= rnorm(30,30,8)

y= c(0,1,2,rnorm(7,70,10),rnorm(10,5,6),rnorm(10,20,4))

z = c(rnorm(10,100,15),rnorm(10,40,14),rnorm(10,30,5))



mout= as.vector(t(replicate(10,c("high","medium","low"))))
# First generate by replicate each element 10 times then conver df to vector by as.vector, but after transposing(t) otherwise high ,med,low 
# repat 100 times instated of high 100 then med 100

source('anovat.R')

anovafinal(x,mout)
anovafinal(y,mout)

anovafinal(z,mout)

summary(aov(z~mout))

kruskal.test(z~as.factor(mout))

mout