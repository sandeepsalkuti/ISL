setwd("E:/Fall2020/ISL-1/Lab/Lab2")
print(getwd())
#2. a) using lm() to perform linear regression
library(ISLR)
library(MASS)
data("Auto")
head(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
#predicting mpg values for different horsepowers
predict(lm.fit, data.frame(horsepower = c(98)), interval ="confidence")
predict(lm.fit, data.frame(horsepower = c(98)), interval ="prediction")

#2 b) Plotting response and predictor
attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="blue")

detach(Auto)

#2. c)plot() to produce diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

#3. a) scatterplot of all variables of auto dataset
pairs(Auto)
#3. b) correlation between variables using cor()
cor(Auto[, names(Auto) !="name"])

#3.c) use lm() to perform multiple linear regression
model = lm(mpg ~. -name, data = Auto)
summary(model)

#3. d)plot() to produce diagnostic plots
par(mfrow = c(2,2))
plot(model)
#3. e) Use symbols to fit linear regression
model = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(model)

model = lm(mpg ~.-name+displacement:cylinders+displacement:weight+acceleration:horsepower, data=Auto)
summary(model)

model = lm(mpg ~.-name+displacement:cylinders+displacement:weight+year:origin+acceleration:horsepower, data=Auto)
summary(model)

model = lm(mpg ~.-name-cylinders-acceleration+year:origin+displacement:weight+
             displacement:weight+acceleration:horsepower+acceleration:weight, data=Auto)
summary(model)

#3. f) Different transformations of variables
par(mfrow=c(2,2))
plot(log(Auto$horsepower),Auto$mpg)

plot(sqrt(Auto$horsepower),Auto$mpg)
plot((Auto$horsepower)^2,Auto$mpg)

#4. a) Multiple regression model to predict sales
?Carseats
head(Carseats)
str(Carseats)
data(Carseats)
lm.fit=lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

#4. e)
fit3 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit3)
#4. g)
confint(fit3)
#4. h)
par(mfrow = c(2, 2))
plot(fit3)

#5. a) simple linear regression of y onto x
set.seed(1) 
x = rnorm(100) 
y=2*x+rnorm (100) 
fit4 <- lm(y ~ x + 0)
summary(fit4)
#5. b)
fit5 <- lm(x ~ y + 0)
summary(fit5)
#5. c)
par(mfrow=c(2,2))
plot(y~x):abline(fit4)
plot(x~y):abline(fit5)
#5. d)
n <- length(x)
t <- sqrt(n - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
as.numeric(t)

#5. f)
fit6 <- lm(y ~ x)
summary(fit6)

fit7 <- lm(x ~ y)
summary(fit7)