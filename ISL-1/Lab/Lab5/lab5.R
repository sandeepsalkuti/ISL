#2. a) Use the rnorm() function to generate a predictor X of length n = 100,
# as well as a noise vector ?? of length n = 100.
set.seed(1)
X <- rnorm(100)
noise <- rnorm(100)
#2. b)Generate a response vector Y of length n = 100 according to the model
#Y = ??0 + ??1X + ??2X2 + ??3X3 + ??, where ??0, ??1, ??2, and ??3 are constants of your choice.
Y <- 3 + 1*X + 4*X^2 - 1*X^3 + noise

#2. c)Use the regsubsets() function to perform best subset selection in
#order to choose the best model containing the predictors X, X2, . . . , X10. What
#is the best model obtained acc?rding to Cp, BIC, and adjusted R2? Show some
#plots to provide evidence for your answer, and report the coefficients of the best
#model obtained. Note you will need to use the data.frame() function to create
#a single data set containing both X and Y .

re?uire(leaps)
df <- data.frame(Y, X)
fit <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)
fit_summary <- summary(fit)
require(tidyverse);require(ggplot2);require(ggthemes);

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           Ad?R2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = '?ree') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

#2. d) using forward stepwise selection and also using backwards
#stepwise selection. How does your answer compare to the results in (c)?

require(caret)

model_back <- train(Y ~ poly(X, 10), dat? = df, 
                    method = 'glmStepAIC', direction = 'backward', 
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_back, df), df$Y)

summary(model_back$?inalModel)

x_poly <- poly(df$X, 10)

colnames(x_poly) <- paste0('poly', 1:10)
model_forw <- train(y = Y, x = x_poly,
                    method = 'glmStepAIC', direction = 'forward',
                    trace = 0,
                    trControl = trainCont?ol(method = 'none', verboseIter = FALSE))

postResample(predict(model_forw, data.frame(x_poly)), df$Y)

summary(model_forw$finalModel)

#2. e)Now fit a lasso model to the simulated data, again using X, X2, . . . , X10
#as predictors. Use cross-validation t? select the optimal value of ??. Create plots
#of the cross-validation error as a function of ??. Report the resulting coefficient
#estimates, and discuss the results obtained.
lasso_model <- train(Y ~ poly(X, 10), data = df,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
           ?         tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_model)

plot(varImp(lasso_model))

coef(lasso_model$finalModel, lasso_model$bestTune$lambda)
postResample(predict(lass?_model, df), df$Y)

#2. f)Now generate a response vector Y according to the model Y = ??0 + ??7X7 + ??,
#and perform best subset selection and the lasso. Discuss the results obtained.
Y_7 <- 3 + 8*X^7 + noise
df_2 <- data_frame(Y_7 = Y_7, X = df[,-1])

fit <- regsubsets(Y_7 ~ poly(X, 10), data = df_2, nvmax = 10)

fit_summary <- summary(fit)

data_frame(Cp?= fit_summary$cp,
           BIC = fit_summary$bic,
           R2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Num?er of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

lasso_y7_model <- train(Y_7 ~ poly(X, 10), data = df_2,
                        method = 'glmnet', 
                        trContro? = trainControl(method = 'cv', number = 5),
                        tuneGrid = expand.grid(alpha = 1, 
                                               lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_y7_model)
plot(varImp(lasso_y7_model))
coef(lasso_y7_mo?el$finalModel, lasso_y7_model$bestTune$lambda)
postResample(predict(lasso_y7_model, df_2), df_2$Y_7)

#3. a) Split the data set into a training set and a test set.
require(ISLR)
require(caret)
require(tidyverse)
data('College')
set.seed(1)

inTrain <- crea?eDataPartition(College$Apps, p = 0.75, list = FALSE)

training <- College[inTrain,]
testing <- College[-inTrain,]

preObj <- preProcess(training, method = c('center', 'scale'))

training <- predict(preObj, training)
testing <- predict(preObj, testing)

y_t?ain <- training$Apps
y_test <- testing$Apps

one_hot_encoding <- dummyVars(Apps ~ ., data = training)
x_train <- predict(one_hot_encoding, training)
x_test <- predict(one_hot_encoding, testing)

#3. b)Fit a linear model using least squares on the training ?et, and report
#the test error obtained.

lin_model <- lm(Apps ~ ., data = training)

pred <- predict(lin_model, testing)

(lin_info <- postResample(pred, testing$Apps))

#3. c)Fit a ridge regression model on the training set, with ?? chosen by
#cross-validation. Report the test error obtained.
ridge_fit <- train(x = x_train, y = y_train,
                   method = 'glmnet', 
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.?rid(alpha = 0,
                                          lambda = seq(0, 10e2, length.out = 20)))

(ridge_info <- postResample(predict(ridge_fit, x_test), y_test))

coef(ridge_fit$finalModel, ridge_fit$bestTune$lambda)

plot(ridge_fit)

plot(varImp(ridge_f?t))
#3. d) Fit a lasso model on the training set, with ?? chosen by crossvalidation.
#Report the test error obtained, along with the number of non-zero coefficient estimates.
lasso_fit <- train(x = x_train, y = y_train, 
                   method = 'glmnet',
                   trControl = trainControl(method = '?v', number = 10),
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.0001, 1, length.out = 50)))

(lasso_info <- postResample(predict(lasso_fit, x_test), y_test))

coef(lasso_fit$finalModel, lasso_?it$bestTune$lambda)

plot(lasso_fit)

plot(varImp(lasso_fit))

#3. e) Fit a PCR model on the training set, with M chosen by crossvalidation.
#Report the test error obtained, along with the value of M selected by crossvalidation.
pcr_model <- train(x = x_tr?in, y = y_train,
                   method = 'pcr',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pcr_info <- postResample(predict(pcr_model, x_test), y_test))

coef(pcr_m?del$finalModel)

plot(pcr_model)

plot(varImp(pcr_model))

#3. f)Fit a PLS model on the training set, with M chosen by crossvalidation.
#Report the test error obtained, along with the value of M selected by crossvalidation.
pls_model <- train(x = x_train, ? = y_train,
                   method = 'pls',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
(pls_info <- postResample(predict(pls_model, x_test), y_test))

coef(pls_model$?inalModel)

plot(pls_model)

#3. g)Comment on the results obtained. How accurately can we predict the
#number of college applications received? Is there much difference among the test
#errors resulting from these five approaches?
as_data_frame(rbind(lin_in?o,
                    ridge_info,
                    lasso_info,
                    pcr_info,
                    pls_info)) %>%
  mutate(model = c('Linear', 'Ridge', 'Lasso', 'PCR', 'PLS')) %>%
  select(model, RMSE, Rsquared)

testing %>%
  summarize(s? = sd(Apps))

require(ggthemes)

residfunc <- function(fit, data) {
  predict(fit, data) - testing$Apps
}

data_frame(Observed = testing$Apps,
           LM = residfunc(lin_model, testing),
           Ridge = residfunc(ridge_fit, x_test),
           Lasso ? residfunc(lasso_fit, x_test),
           PCR = residfunc(pcr_model, x_test),
           PLS = residfunc(pls_model, x_test)) %>%
  gather(Model, Residuals, -Observed) %>%
  ggplot(aes(Observed, Residuals, col = Model)) +
  geom_hline(yintercept = 0, lty = ?) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'loess', alpha = 0.01, col = 'lightsalmon2') +
  facet_wrap(~ Model, ncol = 5) +
  theme_tufte() +
  theme(legend.position = 'top') +
  coord_flip()
view(x_test)

#4. a)Generate a data set with p = 20 ?eatures, n = 1, 000 observations, and an associated
#quantitative response vector generated according to the model Y = X?? + ??, where ?? has some
#elements that are exactly equal to zero.
require(tidyverse)
set.seed(1)
df <- data.frame(replicate(20, rnorm(n = 1000)))

df %>%
  reduce(function(y, x) y + ifelse(runif(1) < 0.5,
                                   rnorm(1, mean = 5, sd = 1), 
 ?                                 0)*x + rnorm(1000)) -> df$Y
#4. b)Split your data set into a training set containing 100 observations and
#a test set containing 900 observations.
require(caret)

inTrain <- createDataPartition(df$Y, p = 0.1, list = F)

x_t?ain <- df[inTrain, -21]
y_train <- df[inTrain, 21]
x_test <- df[-inTrain, -21]
y_test <- df[-inTrain, 21]

#4. c)Perform best subset selection on the training set, and plot the training
#set MSE associated with the best model of each size.

require(leaps);?require(ggplot2); require(dplyr); require(ggthemes)

best_set <- regsubsets(x = x_train, y = y_train, nvmax = 20)

best_set_summary <- summary(best_set)

data_frame(MSE = best_set_summary$rss/900) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, MSE)) ?
  geom_line() + geom_point(type = 9) +
  xlab('Number of Variables Used') +
  ggtitle('MSE on training set') +
  theme_tufte() +
  scale_x_continuous(breaks = 1:20)

data_frame(train_error = best_set_summary$rss/900, vars = 1:20) %>%
  spread(vars, train_?rror)

#4.d)Plot the test set MSE associated with the best model of each size. 
test_errors = rep(NA,19)
test.mat <- model.matrix(Y ~ ., data = df[-inTrain,])
for (i in 1:20){
  coefs = coef(best_set, id=i)
  pred = test.mat[,names(coefs)]%*%coefs
  test_e?rors[i] = mean((y_test-pred)^2)
}


data_frame(MSE = test_errors) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(id, MSE)) +
  geom_line() + geom_point(type = 9) +
  xlab('Number of Variables Used') +
  ggtitle('MSE on testing set') +
  theme_tufte() +
 ?scale_x_continuous(breaks = 1:20)

which.min(test_errors)
data_frame(test_errors, vars = 1:20) %>%
  spread(vars, test_errors)

#4. e) For which model size does the test set MSE take on its minimum value?
#Comment on your results. If it takes on its minimu? value for a model containing
#only an intercept or a model containing all of the features, then play around with
#the way that you are generating the data in (a) until you come up with a scenario
#in which the test set MSE is minimized for an intermediate?model size.
which.min(test_errors)

require(corrplot)
corrplot(cor(df), method = 'color', type = 'lower',diag = F)

#4. f)How does the model at which the test set MSE is minimized compare
#to the true model used to generate the data? Comment on the coeffic?ent values.
coef(regfit.full, which.min(val.errors))


