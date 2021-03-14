#2. a) Fit a logistic regression model that uses "income" and "balance" to predict "default".
library(ISLR)
attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#2. b)Using the valida?ion set approach, estimate the test error of this model. In order to do this, you must perform the following steps:
# i) Split the sample set into a training set and a validation set.

train <- sample(dim(Default)[1], dim(Default)[1] / 2)

#2. b) ii) Fit a?multiple logistic regression model using only the training observations.
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)


#2.b) iii) Obtain a prediction of default status for each individual?in the validation set by 
#computing the posterior probability of default for that individual, and classifying the 
#individual to the "default" category if the posterior probability is greater than 0.5.

probs <- predict(fit.glm, newdata = Default[-train,?], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"

#2. b) iv) Compute the validation set error, which is the fraction of the observations in the validation
#set that are misclassified.
mean(pred.glm != Default[-train? ]$default)

#2. c)Repeat the process in (b) three times, using three different splits of the observations into a training set 
#and a validation set. Comment on the results obtained.

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(de?ault ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$d?fault)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", le?gth(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(f?t.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

#2. d)Now consider a logistic regression model that predicts the probability of "default??? using
#"income", "balance", and a dummy variable for "student". Estimate the test error for this model 
#using the validation set approach. Comment on whether or not including a dummy variable for "student" 
#leads to a reduction in the test error rate.

?rain <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
pred.glm <- rep("No", length(probs))
probs <- predict(fit.glm, newdata = Default[-train, ], type ? "response")
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

#3. a) Using the summary() and glm() functions, determine the estimated standard errors for the coefficients
#associated with "income" and "balance" in a multiple logi?tic regression model that uses both predictors.

set.seed(1)
attach(Default)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

#3. b)Write a function, boot.fn(), that takes as input the "Default" data set as ?ell as an index of the
#observations, and that outputs the coefficient estimates for "income" and "balance" in the multiple logistic regression model.

boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data, family = "binom?al", subset = index)
  return (coef(fit))
}

#3. c)Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic 
#regression coefficients for "income" and "balance".

library(boot)
boot(Default, boot.fn, 100?)

#4. a) Based on this data set, provide an estimate for the population mean of "medv". Call this estimate ??^.
library(MASS)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

#4. b)Provide an estimate of the standard error of ??^. Interpret this result.
se.hat <- sd(medv) / sqrt(dim(Boston)[1])
se.hat

#4. c)Now estimate the standard error of ??^ using the bootstrap. How does this compare to your answer from (b) ?
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)

#4. d)Based on your bootstrap estimate from (c), provide a 95% confid?nce interval for the mean of "medv". 
#Compare it to the results obtained using t.test(Boston$medv).

t.test(medv)

CI.mu.hat <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
CI.mu.hat

#4. e)Based on this data set, provide an estimate, ??^med, for the median value of "medv" in the population.
med.hat <- median(medv)
med.hat

#4. f)We now would like to estimate the standard error of ??^med. Unfortunately, there is no simple formula for
#computing the standard error of the median. Instead, estimate the standard error of the median using the 
#bootstrap. Comment on your findings.

boot.fn <- function(data, index) {
  mu <- median(data[ind?x])
  return (mu)
}
boot(medv, boot.fn, 1000)

#4. g)Based on this data set, provide an estimate for the tenth percentile of "medv" in Boston suburbs.
#Call this quantity ??^0.1.
percent.hat <- quantile(medv, c(0.1))
percent.hat


#4. h)Use the bootstrap to estimate the standard error of ??^0.1. Comment on your findings.

boot.fn <- function(data, index) {
  mu <- quantile(data[index], c(0.1))
  return (mu)
}
boot(medv, boot.fn, 1000)




















