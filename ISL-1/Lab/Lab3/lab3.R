#3. a) numerical summaries of weekly data
library(ISLR)
summary(Weekly)
cor(Weekly[, -9])
#Graphical summaries of weekly data
attach(Weekly)
plot(Volume)

#3. b) logistic regresson with direction as response and 5 log variables + volume
fit.glm <- glm(Dire?tion ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit.glm)

#3. c) confusion mtarix and overall fraction of correct predictions
probs <- predict(fit.glm, type = "response")
pred.glm <- rep("Down", length(probs))
pr?d.glm[probs > 0.5] <- "Up"
table(pred.glm, Direction)

#3. d) Now fit the logistic regression model using a training data period from 1990 to 2008, with "Lag2"
#as the only predictor. Compute the confusion matrix and the overall fraction of correct predict?ons for 
#the held out data (that is, the data from 2009 to 2010).
train <- (Year < 2009)
Weekly.20092010 <- Weekly[!train, ]
Direction.20092010 <- Direction[!train]
fit.glm2 <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summar?(fit.glm2)

probs2 <- predict(fit.glm2, Weekly.20092010, type = "response")
pred.glm2 <- rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] <- "Up"
table(pred.glm2, Direction.20092010)

#3. e) Repeat (d) using LDA.
library(MASS)
fit.lda <- lda(Direction ~?Lag2, data = Weekly, subset = train)
fit.lda
pred.lda <- predict(fit.lda, Weekly.20092010)
table(pred.lda$class, Direction.20092010)

#3. f) Repeat (d) using QDA.
fit.qda <- qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda

pred.qda <- predict(?it.qda, Weekly.20092010)
table(pred.qda$class, Direction.20092010)

#3. g) Repeat (d) using KNN with K = 1.
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
pred.knn <- knn(t?ain.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction.20092010)

#3. i)Experiment with different combinations of predictors, 
#including possible transformations and interactions, for each of the
#methods. Report the variables, method, and assoc?ated confusion matrix 
#that appears to provide the best results on the held out data. Note that 
#you should also experiment with values for K in the KNN classifier.

#logistic regression with Lag2:Lag1
fit.glm3 <- glm(Direction ~ Lag2:Lag1, data = Weekly? family = binomial, subset = train)
probs3 <- predict(fit.glm3, Weekly.20092010, type = "response")
pred.glm3 <- rep("Down", length(probs3))
pred.glm3[probs3 > 0.5] = "Up"
table(pred.glm3, Direction.20092010)

mean(pred.glm3 == Direction.20092010)

# LDA w?th Lag2 interaction with Lag1
fit.lda2 <- lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
pred.lda2 <- predict(fit.lda2, Weekly.20092010)
mean(pred.lda2$class == Direction.20092010)

# QDA with sqrt(abs(Lag2))
fit.qda2 <- qda(Direction ~ Lag2 + s?rt(abs(Lag2)), data = Weekly, subset = train)
pred.qda2 <- predict(fit.qda2, Weekly.20092010)
table(pred.qda2$class, Direction.20092010)

mean(pred.qda2$class == Direction.20092010)

# KNN k =10
pred.knn2 <- knn(train.X, test.X, train.Direction, k = 10)
ta?le(pred.knn2, Direction.20092010)

mean(pred.knn2 == Direction.20092010)

# KNN k = 100
pred.knn3 <- knn(train.X, test.X, train.Direction, k = 100)
table(pred.knn3, Direction.20092010)

mean(pred.knn3 == Direction.20092010)

#3. a)Create a binary variable,?"mpg01", that contains a 1 if "mpg" contains a value above its
#median, and a 0 if "mpg" contains a value below its median. You can compute the median using the
#median() function. Note you may find it helpful to use the data.frame() function to create a s?ngle 
#data set containing both "mpg01" and the other "Auto" variables.

attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

#3. b)Explore the data graphically in order to investigate the association bet?een "mpg01" and the
#other features. Which of the other features seem most likely to be useful in predictiong "mpg01" ? 
#Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.

cor(Auto[, -9])

pairs(Auto)

boxplot(?ylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")

boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")

boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")

boxplot(weight ~ mpg01, data = Auto, main = "We?ght vs mpg01")

boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")

boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

#3. c) Split the data into a training set and a test set.
train <- (year %% 2 == 0)
Auto.train <- Auto[t?ain, ]
Auto.test <- Auto[!train, ]
mpg01.test <- mpg01[!train]

#3. d) Perform LDA on the training data in order to predict mpg01 using the
#variables that seemed most associated with mpg01 in (b). What is the test error
#of the model obtained?

fit.lda <-?lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda

pred.lda <- predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)

mean(pred.lda$class != mpg01.test)

#3. e) Perform QDA on the training data in orde? to predict "mpg01" using the variables that seemed
#most associated with "mpg01" in (b). What is the test error of the model obtained ?

fit.qda <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda

pred.qda ?- predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)

mean(pred.qda$class != mpg01.test)

#3. f)Perform logistic regression on the training data in order to predict "mpg01" using the variables
#that seemed most associated with "mpg01" in (b). Wh?t is the test error of the model obtained ?

fit.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
summary(fit.glm)
  
probs <- predict(fit.glm, Auto.test, type = "response")
pred.glm <- rep(?, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, mpg01.test)

mean(pred.glm != mpg01.test)

#3. g) Perform KNN on the training data, with several values of K, in order to predict "mpg01" 
#using the variables that seemed most associated with "mp?01" in (b). What test errors do you obtain ? 
#Which value of K seems to perform the best on this data set ?
train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[!train, ]
train?mpg01 <- mpg01[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test)

pred.knn <- knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test?

pred.knn <- knn(train.X, test.X, train.mpg01, k = 100)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test)
















