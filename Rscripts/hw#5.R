# Natan Alper - Hw 5
# 1

x1 = rexp(4000,10) 
x2 = rgamma(4000,10) 
x3 = 4 - 2*x2+rnorm(4000,sd=4) 
y = 1000 - 2*x1 - 5*x2^(-.2) -4*x3 + rnorm(4000,sd=10) 
data = data.frame(x1,x2,x3,y)

par(mfrow=c(3,4))

plot(x2^1.9,y)
plot(x2^(1.5),y)
plot(x2^(1.2),y)
plot(x2^(.8),y)
plot(x2^(.6),y)
plot(x2^(.2),y)
plot(x2^(-.2),y)
plot(x2^(-.6),y)
plot(x2^(-.8),y)
plot(x2^(-1.2),y)
plot(x2^(-1.5),y)
plot(x2^(-1.9),y)

# 1b
library(MASS)
boxcox(lm(y~x2), plotit = FALSE) # Find largest y-value. The corresponding x-value is lamda
# Largest value is where x = 2
lam <- 2
tx2 <- ((x2^lam)-1)/2
lm(y ~ x1 + tx2 + x3)

# 1c
x1cent <- x1-mean(x1)
x2cent <- x2-mean(x2)
x3cent <- x3-mean(x3)
cor(data.frame(x1cent, x2cent, x3cent))

data <- cbind(x1cent,x2cent,x3cent)
data
pca <- prcomp(data)
summary(pca)
pca$x 
cor(pca$x)
pca$rotation

# Estimated Model: 
# PC1 = (-0.0001618872*x1cent)+(-0.3544056131*x2cent)+(0.9350917790*x3cent)
# PC2 = (-3.220287e-05*x1cent)+(-9.350918e-01*x2cent)+(-3.544056e-01*x3cent)
# PC3 = (1.000000e+00*x1cent)+(-8.748636e-05*x2cent)+(1.399665e-04*x3cent)

lm(y~pca$x)
# Estimated Model:
# y = 1060.737 + (-3.761*PC1) + (1.440*PC2) + (-2.603*PC3)

# 1d
screeplot(pca,type="lines") # Use only PCA1 since that's before the steep drop in variance
lm(y~pca$x[,1])

# 3a
x1 = sample(1:10,200,replace=TRUE) 
x2 = sample(1:10,200,replace=TRUE) 
prob = 1/(1+exp(-(2+4*x1-5*x2))) 
y = rbinom(n=200, size=1, prob=prob)
data = data.frame(x1,x2)

model <- glm(y~.,family=binomial(),data=data)
model


# 3b
Pred <- (fitted(model)>.5)*1
sum(y!=Pred)/200

# 3c
Pred <- (fitted(model)>.5)*1
Pred

# 3d
fitted(model)
