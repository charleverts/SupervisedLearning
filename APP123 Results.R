rm(list=ls())
setwd("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 1")

### Q1 ###
#Multiple linear regression

#Read in all 244 observations
fulldata <- read.csv("tipdata.csv", header = TRUE) 
head(fulldata)

#Drop "X" column
drops <- c("X")
fulldata <- fulldata[ , !(names(fulldata) %in% drops)] 
head(fulldata)

#Rename columns
colnames(fulldata) <- c("Total_Bill", "Tip", "Sex_of_Tipper", "Smoker",
                        "Day", "Meal", "Size_of_Table")
head(fulldata)

#Create "Tip_Percent" column
Tip_Percent <- (fulldata$Tip / fulldata$Total_Bill)*100
fulldata[, "Tip_Percent"] <- Tip_Percent
head(fulldata)
fulldata <- fulldata[, c(1, 2, 8, 3, 4, 5, 6, 7)]
head(fulldata)

#Create 200 observation sample
set.seed(23)
tip_data <- fulldata[sample(1:nrow(fulldata),200, replace=FALSE), ]
head(tip_data)
dim(tip_data)

#stet graphical parameter
par(mfrow=c(2,2))

#Tukeys test for outliers
summary(tip_data$Tip_Percent)
tukeys_IQR <- IQR(tip_data$Tip_Percent)
Q_1 <- quantile(tip_data$Tip_Percent, 0.25)
Q_3 <- quantile(tip_data$Tip_Percent, 0.75)
tukeys_outlier <- Q_3 + 1.5*tukeys_IQR
tukeys_farout <- Q_3 + 3*tukeys_IQR
tukeys_outlier
tukeys_farout

#Standard deviation check for outliers
SD <- sd(tip_data$Tip_Percent)
x_bar <- mean(tip_data$Tip_Percent)
SD_Max <- x_bar + 3*SD
SD_Max
tukeys_farout

#reset the dataset: without outliers (entries [134],[48])
tip_data <- tip_data[-c(134, 48),]
head(tip_data)
dim(tip_data) #confirm correct dimenions after row removals

#Create train and test sets excluding outliers and Tip Percent
tip_data <- tip_data[,-c(3)]
head(tip_data)
set.seed(23)
ind <- sample(1:nrow(tip_data), 0.8*nrow(tip_data))
train <- tip_data[ind,]
test <- tip_data[-ind,]

#conduct multiple linear regression
model1 <- lm(Tip ~ .,data = train) 
model1
summary(model1)

#use MSE to measure accuracy of the model on the test set
yhat1 <- predict(model1, newdata = test)
tip.test <- test$Tip
plot(tip.test, yhat1)
abline(0,1)
MSE_1 <- mean((yhat1-tip.test)^2) #MSE
MSE_1

### Q2 ###
#Model improvement by variable selection
#Simple linear regression

SLR_coeff1 <- lm(Tip ~ Total_Bill,data = train) 
summary(SLR_coeff1)

SLR_coeff2 <- lm(Tip ~ Sex_of_Tipper,data = train) 
summary(SLR_coeff2)

SLR_coeff3 <- lm(Tip ~ Smoker,data = train) 
summary(SLR_coeff3)

SLR_coeff4 <- lm(Tip ~ Day,data = train) 
summary(SLR_coeff4)

SLR_coeff5 <- lm(Tip ~ Meal,data = train) 
summary(SLR_coeff5)

SLR_coeff6 <- lm(Tip ~ Size_of_Table,data = train) 
summary(SLR_coeff6)

model2 <- lm(Tip ~ Total_Bill, data = train)
summary(model2)

yhat2 <- predict(model2, newdata = test)
plot(tip.test, yhat2)
abline(0,1)
MSE_2 <- mean((yhat2-tip.test)^2) #MSE
MSE_2

model3 <- lm(Tip ~ Size_of_Table, data = train)
summary(model3)

yhat3 <- predict(model3, newdata = test)
plot(tip.test, yhat3)
abline(0,1)
MSE_3 <- mean((yhat3-tip.test)^2) #MSE
MSE_3

model4 <- lm(Tip ~ Meal, data = train)
summary(model4)

yhat4 <- predict(model4, newdata = test)
plot(tip.test, yhat4)
abline(0,1)
MSE_4 <- mean((yhat4-tip.test)^2) #MSE
MSE_4

#Best Subset Selection

#indicates best predictive variables
library(leaps)
#gives data for 8 best variables in the models
regfit.full = regsubsets(Tip ~ ., data = tip_data) 
summary(regfit.full)

#for max 8 variables 
reg.summary = summary(regfit.full)
regfit.full = regsubsets(Tip ~., data = tip_data, nvmax = 8) 

#plotting RSS and adjusted R^2 vs # of variables
par(mfrow =c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted R^2", type = "l")

#plot max point on adjusted R^2
which.max(reg.summary$adjr2)
points(2, reg.summary$adjr2[2], col = "red", cex = 2, pch = 20)

#plot Cp vs # variables, red dot at min point
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp",
     type = "l")
which.min(reg.summary$cp)
points(2, reg.summary$cp[2], col = "red", cex = 2, pch = 20)

#plot BIC vs # variables, red dot at min point
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC",
     type="l")
which.min(reg.summary$bic)
points(1, reg.summary$bic[1], col = "red", cex = 2, pch = 20)

par(mfrow=c(2,2))
#regfit legoblock display of variables and stats
plot(regfit.full, scale = "r2", col=gray(seq(0, 0.9,
                                             length = 10)))
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

#gives coefficients for best number variables used (1-8)
coef(regfit.full, 1) 
coef(regfit.full, 2)
coef(regfit.full, 3)
coef(regfit.full, 4) #max number of coefficients considered

model5 <- lm(Tip ~ Total_Bill + Size_of_Table, data = train)
summary(model5)

yhat5 <- predict(model5, newdata = test)
plot(tip.test, yhat5)
abline(0,1)
MSE_5 <- mean((yhat5-tip.test)^2) #MSE
MSE_5

#Forward and Backward Stepwise Selection

#indicates best predictive variables
regfit.fwd = regsubsets(Tip ~., data = tip_data, nvmax = 8,
                        method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Tip ~., data = tip_data, nvmax = 8,
                        method = "backward")
summary(regfit.bwd)

#full/fwd/bwd yields same variables and coefficients
#1 variable used
coef(regfit.full, 1)
coef(regfit.fwd, 1)
coef(regfit.bwd, 1)

#2 variables used
coef(regfit.full, 2)
coef(regfit.fwd, 2)
coef(regfit.bwd, 2)

#3 variables used
coef(regfit.full, 3)
coef(regfit.fwd, 3)
coef(regfit.bwd, 3)

model6 <- lm(Tip ~ Total_Bill + Size_of_Table + Smoker, data = train)
summary(model6)

yhat6 <- predict(model6, newdata = test)
plot(tip.test, yhat6)
abline(0,1)
MSE_6 <- mean((yhat6-tip.test)^2) #MSE
MSE_6

#Validation Set Approach and Cross-Validation

#Set seed to split train and test data sets
set.seed(23)
cv.train = sample(c(TRUE,FALSE), nrow(tip_data), rep = TRUE)
cv.test = (!cv.train)

#perform best subset selection
regfit.best = regsubsets(Tip ~., data = tip_data[cv.train ,], nvmax = 8)

#make a model matrix from the test data
test.mat = model.matrix(Tip ~., data = tip_data[cv.test ,])

# Run a loop and for each i, extract the coefficients from 
# regfit.best for the best model of that size,
# multiply into the appropriate columns of the test matrix 
# to form the predictions, and compute the test MSE.

val.errors = rep(NA ,8)

for(i in 1:8) {
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((tip_data$Tip[cv.test] - pred)^2)
}
val.errors #test MSE

#best model is the one that contains 1 variable
which.min(val.errors)
coef(regfit.best, 1)

#now run best subset selection on full data set (previously train)
regfit.best = regsubsets(Tip ~ ., data = tip_data, nvmax = 8)
coef(regfit.best ,1)

#Choose among the models of different sizes using cross-validation

#k = 10 folds,create a matrix to store the results
k = 10
set.seed(23)
folds = sample(1:k, nrow(tip_data), replace = TRUE)
cv.errors = matrix(NA, k, 8, dimnames = list(NULL, paste(1:8)))

predict.regsubsets <- function(object, newdata, id) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

for(j in 1:k){
  best.fit = regsubsets(Tip ~ ., data = tip_data[folds != j,], nvmax = 8)
  for(i in 1:8) {
    pred = predict.regsubsets(best.fit, tip_data[folds == j,], id = i)
    cv.errors[j,i] = mean((tip_data$Tip[folds == j] - pred)^2)
  }
}

#mean cross-validation error
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

#plot of mean cv errors vs number of variables
par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b") 

#We now perform best subset selection on the full data set
reg.best = regsubsets(Tip ~., data = tip_data, nvmax = 8)
coef(reg.best, 1)
coef(reg.best, 2)
coef(reg.best, 3)
coef(reg.best, 4)
coef(reg.best, 5)

#Ridge Regression 

#Pass in an x matrix and y vector
x = model.matrix(Tip ~ .,tip_data )[,-1]
y = tip_data$Tip

#build a Ridge Regression model
library(glmnet)
grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))

#split the samples into a training set and a test set
set.seed(23)
rr_train = sample(1:nrow(x), nrow(x)/2)
rr_test = (-rr_train)
y.test = y[rr_test]

#use cross-validation to choose the tuning parameter ??
set.seed(23)
cv.out = cv.glmnet(x[rr_train,], y[rr_train], alpha = 0)
plot(cv.out)
extralam = cv.out$lambda.min
bestlam_rr = cv.out$lambda.1se #use this one
bestlam_rr

#test MSE associated with this value of ??
ridge.pred = predict(ridge.mod, s = bestlam_rr , newx = x[rr_test,])
mean((ridge.pred - y.test)^2)

#Refit the Ridge Regression model on the full data set,
#using the value of ?? chosen by cross-validation, 
#examine the coefficient estimates for # of variables used

out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam_rr)[1:9,]

#LASSO

#build a LASSO model
lasso.mod = glmnet(x, y, alpha = 1, standardize = TRUE)
plot(lasso.mod, label = TRUE, xvar = "lambda")
legend("bottomright", lwd = 1, col = 2:11, legend = colnames(x), cex = .7)

#perform cross-validation and compute test error
set.seed (23)
cv.out = cv.glmnet(x[rr_train,], y[rr_train], alpha = 1)
plot(cv.out)
bestlam_lasso = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam_lasso, newx = x[rr_test,])
MSE_7 <- mean((lasso.pred - y.test)^2)
MSE_7

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients",s = bestlam_lasso)[1:9,]
lasso.coef
model7 <- lasso.coef[lasso.coef != 0]


#introducing interaction terms

model8 <- lm(Tip ~ Total_Bill*Meal + Size_of_Table, data = train)
summary(model8)

yhat8 <- predict(model8, newdata = test)
plot(tip.test, yhat8)
abline(0,1)
MSE_8 <- mean((yhat8-tip.test)^2) #MSE
MSE_8

model9 <- lm(Tip ~ Total_Bill*Smoker + Meal + Size_of_Table, data = train)
summary(model9)

yhat9 <- predict(model9, newdata = test)
plot(tip.test, yhat9)
abline(0,1)
MSE_9 <- mean((yhat9-tip.test)^2) #MSE
MSE_9

MSE_reduction_percent <- function(MSE_n) {
  percent_reduction <- (MSE_1 - MSE_n)/MSE_1*100
  return(percent_reduction)
}

MSE_reduction_percent(MSE_2)
MSE_reduction_percent(MSE_3)
MSE_reduction_percent(MSE_4)
MSE_reduction_percent(MSE_5)
MSE_reduction_percent(MSE_6)
MSE_reduction_percent(MSE_7)
MSE_reduction_percent(MSE_8)
MSE_reduction_percent(MSE_9)

### Q3 ###
#Residual diagnostic analysis

par(mfrow=c(2,2))

#residuals vs fitted values
plot(model8,1) 
hist(residuals(model8), breaks = 20, col = "grey75")
ytick<-seq(0, 100, by=5)
axis(side=2, at=ytick, labels = TRUE)

#normal Q-Q plot
plot(model8,2)



