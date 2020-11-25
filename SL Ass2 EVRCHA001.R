#### LOAD AND SPLIT DATA ####
rm(list=ls())
setwd("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 2")

#Read in train and test sets
occtrain <- read.csv("occupancy_training.csv", header = TRUE) 
occtrain$date <- NULL
occtrain$Occupancy <- as.factor(occtrain$Occupancy)
head(occtrain)
str(occtrain) #inspect structure

finaltest <- read.csv("occupancy_testing.csv", header = TRUE) 
finaltest$date <- NULL
finaltest$Occupancy <- as.factor(finaltest$Occupancy)
head(finaltest) 
str(finaltest) #inspect structure

#Split occtrain into test and train sets
library(caret)
set.seed(23)
samp <- createDataPartition(occtrain[,"Occupancy"],1,.8)[[1]]
samp

#80% train split
train = occtrain[samp,]
train$Occupancy <- as.factor(train$Occupancy)
head(train)
str(train)

#20% test split
test = occtrain[-samp,]
test$Occupancy <- as.factor(test$Occupancy)
head(test)
str(test)

#### 0. DESCRIPTIVE STATISTICS ####

# Since levels of $Occupancy (0&1) have unequal distributions;
# createDataPartition allows you to create a sample that maintains
# the ratio or balance of the factor classes.

#Number of 0 vs 1 entries
#Same proportions maintained
table(occtrain$Occupancy)#0:5092 1:1659
round(5092/1659, 2) #3.07

table(train$Occupancy) #0:4074 1:1328
round(4074/1328, 2) #3.07

table(test$Occupancy) #0:1018 1:331
round(1018/331, 2) #3.08

#Correlation Table
num_occtrain <- read.csv("occupancy_training.csv", header = TRUE) 
num_occtrain$date <- NULL
correlation <- cor(num_occtrain)
t <- round(correlation, 2)
t
# write.table(t, file = "correlation.txt", sep = ",", 
#           quote = FALSE, row.names = F)

#Correlation cluster plot
library(corrplot)
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,)

#Correlation heatmap
col <- colorRampPalette(c("black", "white", "magenta"))(20)
heatmap(x = correlation, col = col, symm = TRUE, margins = c(10, 10))
legend(x = "topright", legend = c("High Correlation", "Low Correlation"), 
       fill = colorRampPalette(c("magenta", "black"))(2))

#GGplot table comparing features (as per Candenado, L., 
#Occupancy-detection-data, (2016), GitHub repository: 
#https://github.com/LuisM78/Occupancy-detection-data)

library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
pushViewport(viewport(layout = grid.layout(6, 1)))

datatesting <- read.table("datatest.txt",header=TRUE,sep=",")
datatesting$Occupancy <- datatesting$Occupancy 
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting$date <- as.POSIXct(datatesting$date,tz="UTC") 
str(datatesting) #inspect

myplot1 <- ggplot(datatesting,aes(date)) + 
  geom_line(color="Red", aes(y = Temperature)) + 
  ylab("Temperature") + xlab("Time") +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), tz="GMT")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))        

myplot2 <- ggplot(datatesting, aes(date)) + 
  geom_line(color = "Blue", aes(y = Humidity)) +
  ylab("Humidity") + xlab("Time")+
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), 
                                       tz = "GMT")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))        

myplot3 <- ggplot(datatesting, aes(date)) + 
  geom_line(color = "deepskyblue1", aes(y = HumidityRatio)) + 
  ylab("HumidityRatio") + xlab("Time") +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), 
                                       tz="GMT")) +
  scale_y_continuous(breaks = seq(0.003, 0.006, by = 0.003),  
                     limits = c(0.003, 0.006)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text.y = element_text(angle = 90, hjust = 1))        

myplot4 <- ggplot(datatesting, aes(date)) + 
  geom_line(color = "Green", aes(y = CO2))+ 
  ylab("CO2 (ppm)") + xlab("Time") +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), 
                                       tz = "GMT")) +
  scale_y_continuous(breaks = seq(400, 1400, by = 500),  
                     limits = c(400, 1400)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 1))   

myplot5 <- ggplot(datatesting, aes(date)) + 
  geom_line(color = "gold4", aes(y = Light)) +
  ylab("Light (Lux)") + xlab("Time") +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), 
                                       tz = "GMT")) +
  scale_y_continuous(breaks = seq(0, 700, by = 150),  
                     limits = c(0, 700)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1))        

myplot6 <- ggplot(datatesting, aes(date)) + 
  geom_line(color = "Black", aes(y = as.numeric(Occupancy) - 1)) + 
  ylab("Occupancy") + xlab("Time") +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = date_format("%H:%M"),
                   limits = as.POSIXct(c("2015-02-03 7:00","2015-02-04 8:00"), 
                                       tz = "GMT"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(myplot5, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))

myplot1 <- ggplot_gtable(ggplot_build(myplot1))
myplot2 <- ggplot_gtable(ggplot_build(myplot2))
myplot3 <- ggplot_gtable(ggplot_build(myplot3))
myplot4 <- ggplot_gtable(ggplot_build(myplot4))
myplot5 <- ggplot_gtable(ggplot_build(myplot5))
myplot6 <- ggplot_gtable(ggplot_build(myplot6))

maxWidth = unit.pmax(myplot1$widths[2:3], myplot2$widths[2:3],
                     myplot3$widths[2:3], myplot4$widths[2:3],
                     myplot5$widths[2:3],myplot6$widths[2:3])

myplot1$widths[2:3] <- maxWidth
myplot2$widths[2:3] <- maxWidth
myplot3$widths[2:3] <- maxWidth
myplot4$widths[2:3] <- maxWidth
myplot5$widths[2:3] <- maxWidth
myplot6$widths[2:3] <- maxWidth

#Temperature, Humidity, HumidityRatio & CO2
grid.arrange(myplot1, myplot2, myplot3, myplot4, ncol=1)

#Light & Occupancy
grid.arrange(myplot5, myplot6, ncol=1)

# occtrain <- read.csv("occupancy_training.csv", header = TRUE) 
# occtrain$date <- NULL
# cols2 <- character(nrow(occtrain))
# cols2[] <- "black"
# cols2[occtrain$Occupancy %in% c("0")] <- "black"
# cols2[occtrain$Occupancy %in% c("1")] <- "gold"
# pairs(occtrain[1:5], col = cols2, cex = 1.1, cex.labels = 1.5)
# legend("bottomright", fill = occtrain$Occupancy, legend = c(levels(occtrain$Occupancy)))

#Generalized pairs plot
library(GGally)
occtrain <- read.csv("occupancy_training.csv", header = TRUE) 
occtrain$date <- NULL
ggpairs(occtrain, aes(color = as.factor(Occupancy)), columns = 1:5) +
  scale_fill_manual(values=c("black", "#FF00CC")) +
  scale_color_manual(values=c("black", "#FF00CC"))  

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(viridis)
library(dplyr)
library(RColorBrewer)
data <- occtrain %>% 
  mutate(Occupancy = as.factor(Occupancy),
         Temperature = as.numeric(Temperature))

#Temperature vs Occupancy
ggplot(data, aes(x = as.numeric(Temperature), fill = Occupancy)) +
  geom_histogram(binwidth=0.1, color="#e9ecef", alpha=0.9) +
  theme_classic() +
  xlab("Temperature") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.9, 0.7)) +
  #scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Paired") + #check all available palettes
  ggtitle("Temperature vs Occupancy") +
  scale_x_continuous(name = "Temperature (C)", 
                     breaks = seq(19, 23, by = 0.5), 
                     limits=c(19, 23))

#Light vs Occupancy
ggplot(data, aes(x = as.numeric(Light), fill = Occupancy)) +
  geom_histogram(binwidth = 25, color="#e9ecef", alpha = 0.9) +
  theme_classic() +
  xlab("Light") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.9, 0.7)) +
  scale_fill_brewer(palette = "Dark2") + #check all available palettes
  ggtitle("Light vs Occupancy") +
  scale_x_continuous(name = "Light (Lux)", 
                     breaks = seq(0, 600, by = 50), 
                     limits=c(0, 600)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100), 
                     limits=c(0, 700))

#CO2 vs Occupancy
ggplot(data, aes(x = as.numeric(CO2), fill = Occupancy)) +
  geom_histogram(binwidth = 50, color="#e9ecef", alpha = 0.9) +
  theme_classic() +
  xlab("CO2") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.9, 0.7)) +
  scale_fill_hue(h = c(180, 280), c = 80, l = 50, h.start = 0, direction = 1, aesthetics = "fill") +
  ggtitle("CO2 vs Occupancy") +
  scale_x_continuous(name = "CO2 (ppm)", 
                     breaks = seq(400, 2000, by = 100), 
                     limits=c(400, 2000)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50), 
                     limits=c(0, 250))

#Humidity vs Occupancy
ggplot(data, aes(x = as.numeric(Humidity), fill = Occupancy)) +
  geom_histogram(binwidth = 1, color="#e9ecef", alpha = 0.9) +
  theme_classic() +
  xlab("Humidity") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.9, 0.7)) +
  scale_fill_hue(h = c(180, 280), c = 80, l = 50, h.start = 0, direction = 1, aesthetics = "fill") +
  ggtitle("Humidity vs Occupancy") +
  scale_x_continuous(name = "Humidity (%)", 
                     breaks = seq(15, 40, by = 1), 
                     limits=c(15, 40)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100), 
                     limits=c(0, 1000))

#HumidityRatio vs Occupancy
ggplot(data, aes(x = as.numeric(HumidityRatio), fill = Occupancy)) +
  geom_histogram(binwidth = 0.1e-3, color="#e9ecef", alpha = 0.9) +
  theme_classic() +
  xlab("HumidityRatio") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.9, 0.7)) +
  scale_fill_hue(h = c(180, 280), c = 80, l = 50, h.start = 0, direction = 1, aesthetics = "fill") +
  ggtitle("HumidityRatio vs Occupancy") +
  scale_x_continuous(name = "HumidityRatio", 
                     breaks = seq(2.5e-3, 6.5e-3, by = 1e-3), 
                     limits=c(2.5e-3, 6.5e-3)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100), 
                     limits=c(0, 600))

#### 1. LOGISTIC REGRESSION ####

# Logistic Regression without Lasso or Ridge Penalty (Standard LR)
# All predictors
model1 <- glm(Occupancy ~ ., data = train, family = 'binomial')
print(model1)
summary(model1)
round(exp(coef(model1)),3) 

# Excluding Temperature
model2 <- glm(Occupancy ~ .-Temperature, data = train, family = 'binomial')
print(model2)
summary(model2)
round(exp(coef(model2)),3) # Odds effect

# Excluding Temperature & HumidityRatio
model3 <- glm(Occupancy ~ .-Temperature-HumidityRatio, data = train, family = 'binomial')
print(model3)
summary(model3)
round(exp(coef(model3)),3) # Odds effect

# Excluding Temperature & HumidityRatio & Humidity
model4 <- glm(Occupancy ~ .-Temperature-HumidityRatio-Humidity, data = train, family = 'binomial')
print(model4)
summary(model4)
round(exp(coef(model4)),3) # Odds effect

model5 <- glm(Occupancy ~ .-HumidityRatio-Humidity, data = train, family = 'binomial')
print(model5)
summary(model5)
round(exp(coef(model5)),3) # Odds effect

# Logistic Regression with Lasso Penalty
library(glmnet)

X <- train[ , -c(6)]
head(X)
X <- as.matrix(X)
head(X)
Y <- train[ , -c(1:5)]
head(Y)
Y <- as.matrix(Y)
head(Y)

testX <- test[ , -c(6)]
testX <- as.matrix(testX)
head(testX)
testY <- test[ , -c(1:5)]
testY <- as.matrix(testY)
head(testY)

#model 6
modelCV <- glmnet(X, Y, alpha = 1, standardize = TRUE, family = 'binomial')
plot(modelCV, xvar = 'lambda', label=TRUE)

X_dropped <- train[ , -c(2,5,6)]
X_dropped <- as.matrix(X_dropped)
head(X_dropped)

testX_dropped <- test[ , -c(2,5,6)]
testX_dropped <- as.matrix(testX_dropped)
head(testX_dropped)

#model 7
modelCV_dropped <- glmnet(X_dropped, Y, alpha = 1, standardize = TRUE, family = 'binomial')
plot(modelCV_dropped, xvar = 'lambda', label=TRUE)


#model 6
# 10-fold CV results for Logistic Regression with Lasso Penalty
set.seed(23)
cv <- cv.glmnet(X, Y, alpha = 1, nfolds = 10, type.measure = 'mse', 
                standardize = T, family = 'binomial')
plot(cv)
abline(v = log(cv$lambda.min), lwd = 2)
cv$lambda.min
log(cv$lambda.min)
coef(modelCV, s = cv$lambda.min)

#model 7
# 10-fold CV results for Logistic Regression with Lasso Penalty
set.seed(23)
cv_dropped <- cv.glmnet(X_dropped, Y, alpha = 1, nfolds = 10, type.measure = 'mse', 
                standardize = T, family = 'binomial')
plot(cv_dropped)
abline(v = log(cv_dropped$lambda.min), lwd = 2)
cv_dropped$lambda.min
log(cv_dropped$lambda.min)
coef(modelCV_dropped, s = cv_dropped$lambda.min)

# Predict under standard LR:
pi_hat_1 <- predict(model1, newdata = test, type = 'response')
head(pi_hat_1)

pi_hat_2 <- predict(model2, newdata = test, type = 'response')
head(pi_hat_2)

pi_hat_3 <- predict(model3, newdata = test, type = 'response')
head(pi_hat_3)

pi_hat_4 <- predict(model4, newdata = test, type = 'response')
head(pi_hat_4)

pi_hat_5 <- predict(model5, newdata = test, type = 'response')
head(pi_hat_5)

# LASSO with lambda chosen by CV:
set.seed(23)
pi_hat_cv <- predict(modelCV, newx = testX, 
                     s = cv$lambda.min, type = 'response')

set.seed(23)
pi_hat_cv_dropped <- predict(modelCV_dropped, newx = testX_dropped, 
                     s = cv_dropped$lambda.min, type = 'response')

Y_hat_1 <- ifelse(pi_hat_1 >= 0.5, "1", "0")
Y_hat_2 <- ifelse(pi_hat_2 >= 0.5, "1", "0")
Y_hat_3 <- ifelse(pi_hat_3 >= 0.5, "1", "0")
Y_hat_4 <- ifelse(pi_hat_4 >= 0.5, "1", "0")
Y_hat_5 <- ifelse(pi_hat_5 >= 0.5, "1", "0")
Y_hat_cv <- ifelse(pi_hat_cv >= 0.5, "1", "0")
Y_hat_cv_dropped <- ifelse(pi_hat_cv_dropped >= 0.5, "1", "0")

# Misclassification error rate:
N <- length(testY)
Error_1 <- sum((Y_hat_1!=testY)/N)*100 #model1
round((Accuracy_1 <- 100 - Error_1), 4)
Error_2 <- sum((Y_hat_2!=testY)/N)*100 #model2
round((Accuracy_2 <- 100 - Error_2), 4)
Error_3 <- sum((Y_hat_3!=testY)/N)*100  #model3
round((Accuracy_3 <- 100 - Error_3), 4)
Error_4 <- sum((Y_hat_4!=testY)/N)*100  #model4
round((Accuracy_4 <- 100 - Error_4), 4)
Error_5 <- sum((Y_hat_5!=testY)/N)*100  #model5
round((Accuracy_5 <- 100 - Error_5), 4)
Error_cv <- sum((Y_hat_cv!=testY)/N)*100  #model6
round((Accuracy_cv <- 100 - Error_cv), 4)
Error_cv_dropped <- sum((Y_hat_cv_dropped!=testY)/N)*100  #model7
round((Accuracy_cv_dropped <- 100 - Error_cv_dropped), 4)

# Create confusion matrices
(conf_mod1 <- cbind(table(Y_hat_1, testY, dnn = c("predict", "true")), Accuracy_1))
(conf_mod2 <- cbind(table(Y_hat_2, testY, dnn = c("predict", "true")), Accuracy_2))
(conf_mod3 <- cbind(table(Y_hat_3, testY, dnn = c("predict", "true")), Accuracy_3))
(conf_mod4 <- cbind(table(Y_hat_4, testY, dnn = c("predict", "true")), Accuracy_4))
(conf_mod5 <- cbind(table(Y_hat_5, testY, dnn = c("predict", "true")), Accuracy_5))
(conf_modcv <- cbind(table(Y_hat_cv, testY, dnn = c("predict", "true")), Accuracy_cv))
(conf_modcv_dropped <- cbind(table(Y_hat_cv_dropped, 
                                  testY, dnn = c("predict", "true")),
                             Accuracy_cv_dropped))

# ROC curves
library(ROCR)

# Model1:
pred_1 <- prediction(pi_hat_1, testY)
perf_1  <- performance(pred_1, 'tpr', 'fpr')
plot(perf_1, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_1, measure = 'auc')@y.values[[1]]*100 #area under curve

# Model2:
pred_2 <- prediction(pi_hat_2, testY)
perf_2  <- performance(pred_2, 'tpr', 'fpr')
plot(perf_2, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_2, measure = 'auc')@y.values[[1]]*100 #area under curve

# Model3:
pred_3 <- prediction(pi_hat_3, testY)
perf_3  <- performance(pred_3, 'tpr', 'fpr')
plot(perf_3, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_3, measure = 'auc')@y.values[[1]]*100 #area under curve

# Model4:
pred_4 <- prediction(pi_hat_4, testY)
perf_4  <- performance(pred_4, 'tpr', 'fpr')
plot(perf_4, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_4, measure = 'auc')@y.values[[1]]*100 #area under curve

# Model5:
pred_5 <- prediction(pi_hat_5, testY)
perf_5  <- performance(pred_5, 'tpr', 'fpr')
plot(perf_5, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_5, measure = 'auc')@y.values[[1]]*100 #area under curve

# Model6 (with Lasso)
pred_cv <- prediction(pi_hat_cv, testY)
perf_cv  <- performance(pred_cv, 'tpr', 'fpr')
plot(perf_cv, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_cv, measure = 'auc')@y.values[[1]]*100

# Model7 (With Lasso)
pred_cv_dropped <- prediction(pi_hat_cv_dropped, testY)
perf_cv_dropped  <- performance(pred_cv_dropped, 'tpr', 'fpr')
plot(perf_cv_dropped, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_cv_dropped, measure = 'auc')@y.values[[1]]*100

#### 2. CLASSIFICATION TREE ####

library(tree)
# Using gini index reduction...
tree_occtrain_g <- tree(Occupancy ~ ., data = train, split = 'gini') 
# and using deviance reduction as splitting criterion
tree_occtrain_d <- tree(Occupancy ~ ., data = train, split = 'deviance')

# First using Gini index
summary(tree_occtrain_g) 
tree_occtrain_g
plot(tree_occtrain_g, type = c("uniform"))
text(tree_occtrain_g, cex = 0.9) #BIG, messy tree!
# Gini doesn't take into account the number of observations in each resulting node
# "Good" gini splits early on lead to very little reduction in the deviance

# Checking deviance
summary(tree_occtrain_d) 
tree_occtrain_d
plot(tree_occtrain_d, type = c("uniform"))
text(tree_occtrain_d, cex = 0.9, pretty = 1) #Much more sensible

# Grow the tree deeper
tree_occtrain <- tree(Occupancy ~ ., data = train, split = 'deviance',
                      control = tree.control(nrow(train), 
                                             mindev = 0.005))
summary(tree_occtrain) #Checks out
tree_occtrain
plot(tree_occtrain, type = c("uniform"))
text(tree_occtrain, cex = 0.9, pretty = 1) #Much more sensible

# Cost complexity pruning
set.seed(23)
cv_occtrain <- cv.tree(tree_occtrain, FUN = prune.misclass) #use classification error rate for pruning

# Aesthetics
plot(cv_occtrain$size, cv_occtrain$dev, type='o', pch = 16, col = 'navy', lwd = 2,
     xlab='Number of terminal nodes', ylab='CV error')

cv_occtrain$k[1] <- 0 #by default is neg inf., change to 0
alpha <- round(cv_occtrain$k,1)
axis(3, at = cv_occtrain$size, lab = alpha, cex.axis  =0.8)
mtext(expression(alpha), 3, line = 2.5, cex = 1.2)
axis(side = 1, at = 1:max(cv_occtrain$size))
T <- cv_occtrain$size[which.min(cv_occtrain$dev)] #The minimum CV Error (8 nodes)
abline(v = T, lty = 2, lwd = 2, col = 'red')
abline(v = 4, lty = 2, lwd = 2, col = 'green')

#Model8
# Prune the tree with 8 nodes
pr_tree_8 <- prune.misclass(tree_occtrain, best = T) #8 nodes
plot(pr_tree_8, type = c("uniform"))
text(pr_tree_8, pretty = 0)

# Predict on test set
yhat_8 <- predict(pr_tree_8, test, type = 'class') #type argument = classification!
(c_mat <- table(yhat_8, test$Occupancy)) #predicted vs actaul YES and NO
sum(diag(c_mat))/nrow(test)*100 #classification accuracy %

#REPEAT

#Model9
# Prune the tree with 4 nodes
pr_tree_4 <- prune.misclass(tree_occtrain, best = 4) #4 nodes
plot(pr_tree_4, type = c("uniform"))
text(pr_tree_4, pretty = 0)

# Predict on test set
yhat_4 <- predict(pr_tree_4, test, type = 'class') #type argument nb for classification!
(c_mat <- table(yhat_4, test$Occupancy)) #predicted vs actaul YES and NO
sum(diag(c_mat))/nrow(test)*100 #classification accuracy %

#### 3. BAGGED TREES & RANDOMFOREST ####
library(randomForest)

#With Light
set.seed(23)
bag_occtrain <- randomForest(Occupancy ~ ., data = train, 
                mtry = ncol(train) - 1, #for bagging, use all predictors(# features to use at each split)
                ntree = 1000, #number of trees
                importance = TRUE, #keep track of reduction in loss function
                do.trace = 100)  #print out regular progress
bag_occtrain

## Choose number of trees:
head(bag_occtrain$err.rate)
plot(bag_occtrain$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

## Variable importance plot
varImpPlot(bag_occtrain, type = 2) #type = 2: Reduction in gini index
set.seed(23)
bag_varimp <- randomForest::importance(bag_occtrain, type = 2)
bag_varimp <- bag_varimp[order(bag_varimp, decreasing = FALSE),]
barplot(bag_varimp, horiz = T, col = 'purple', las = 1,
        las = 0,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2, 
        main = 'Variable Importance', cex.main = 1.8, cex.names = 0.8)

#Model10
#Without Humidity and HumidityRatio
set.seed(23)
bag_occtrain_exHHR <- randomForest(Occupancy ~ .-Humidity-HumidityRatio, data = train, 
                             mtry = ncol(train) - 4, #features to use at each split
                             ntree = 1000, #number of trees
                             importance = TRUE, #keep track of reduction in loss function
                             do.trace = 100)  #print out regular progress
bag_occtrain_exHHR

## Choose number of trees:
head(bag_occtrain_exHHR$err.rate)
plot(bag_occtrain_exHHR$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')

## Variable importance plot
varImpPlot(bag_occtrain_exHHR, type = 2) #type = 2: Reduction in gini index
set.seed(23)
bag_varimp_exHHR <- randomForest::importance(bag_occtrain_exHHR, type = 2)
bag_varimp_exHHR <- bag_varimp_exHHR[order(bag_varimp_exHHR, decreasing = FALSE),]
barplot(bag_varimp_exHHR, horiz = T, col = 'purple', las = 1,
        las = 0,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.5, cex.axis = 1.2, 
        main = 'Variable Importance \n excl Humidity & HumidityRatio', cex.main = 1.8, cex.names = 1.2)

# Model11
# Fit a Random Forest (1000 trees)
# Using all predictors
set.seed(23)
rf_occtrain_1 <- randomForest(Occupancy ~ ., data = train, 
                            ntree = 1000, 
                            importance = TRUE, 
                            do.trace = 100) 
# For classification tree, default mtry = floor(sqrt(ncol(x)))
rf_occtrain_1

## Fit a Random Forest (1500 trees)
set.seed(23)
rf_occtrain_2 <- randomForest(Occupancy ~ ., data = train, 
                              ntree = 1500, 
                              importance = TRUE, 
                              do.trace = 100) 
# For classification tree, default mtry = floor(sqrt(ncol(x)))
rf_occtrain_2

## Fit a Random Forest (2000 trees)
set.seed(23)
rf_occtrain_3 <- randomForest(Occupancy ~ ., data = train, 
                              ntree = 2000, 
                              importance = TRUE, 
                              do.trace = 100) 
# For classification tree, default mtry = floor(sqrt(ncol(x)))
rf_occtrain_3

## Fit a Random Forest (2000 trees) excluding Humidity and HumidityRatio
set.seed(23)
rf_occtrain_4 <- randomForest(Occupancy ~ .-Humidity-HumidityRatio, data = train, 
                              ntree = 2000, 
                              importance = TRUE, 
                              do.trace = 100) 
# For classification tree, default mtry = floor(sqrt(ncol(x)))
rf_occtrain_4

## Variable importance plot
varImpPlot(rf_occtrain_1, type = 2)

# compare OOB errors:
par(mfrow=c(1,1))
plot(rf_occtrain_1$err.rate[, 'OOB'], type = 's', xlab = 'Number of trees', ylab = 'OOB error')
lines(bag_occtrain$err.rate[, 'OOB'], col = 'red', type = 's')
lines(rf_occtrain_4$err.rate[, 'OOB'], col = 'green', type = 's')
lines(bag_occtrain_exHHR$err.rate[, 'OOB'], col = 'purple', type = 's')
legend('topright', legend = c('Bagging', 'Bagging excl HHR', 
                              'Random Forest', 'Random Forest excl HHR'), 
       col = c('red', 'purple', 'black', 'green'), lwd = 2)

## Use both models for prediction
bag_pred <- predict(bag_occtrain_exHHR, newdata = test) #Model10
rf_pred <- predict(rf_occtrain_1, newdata = test) #Model11

ytest <- test$Occupancy
table(bag_pred, ytest, dnn = c('Predicted', 'True')) #Model10
table(rf_pred, ytest, dnn = c('Predicted', 'True')) #Model11

#Misclassification rates
(bag_err <- mean(bag_pred != ytest)) #Model10
(rf_err <- mean(rf_pred != ytest)) #Model11

#Classification Accuracy

round((Accuracy_bag <- 100 - bag_err*100), 4) #Model10

round((Accuracy_rf <- 100 - rf_err*100), 4) #Model11

## Check answer using built in caret confusion matrix:
confusionMatrix(bag_pred, ytest) #Model10
confusionMatrix(rf_pred, ytest) #Model11

#### 4. GRADIENT BOOSTED TREES ####

library(gbm)

# Requires 0-1 for binary classification (without caret)
train_bin <- train
train_bin$Occupancy <- as.numeric(train_bin$Occupancy) - 1 

system.time(
  gbm_occtrain <- gbm(Occupancy ~ ., data = train_bin, 
                      distribution = 'bernoulli', #Use bernoulli for binary classification 
                      n.trees = 10000, #B
                      interaction.depth = 2, #d
                      shrinkage = 0.005, #lambda = learning rate
                      bag.fraction = 1, #default = 0.5 for extra randomisation. 
                      cv.folds = 10, #built-in CV
                      n.cores = 3, #which can be parallelised
                      verbose = F)
)
gbm_occtrain

#CV Errors per tree
best_B <- gbm.perf(gbm_occtrain, method = 'cv') 

#Variable importance
summary(gbm_occtrain) 

#Partial dependence
plot.gbm(gbm_occtrain, 1, best_B) #Temperature
plot.gbm(gbm_occtrain, 2, best_B) #Humidity
plot.gbm(gbm_occtrain, 3, best_B) #Light
plot.gbm(gbm_occtrain, 4, best_B) #CO2
plot.gbm(gbm_occtrain, 5, best_B) #HumidityRatio

#Grid search
set.seed(23)
ctrl <- trainControl(method = 'cv', number = 5, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(1000, 1500, 2000),
                        interaction.depth = c(1, 2, 6),
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = 1)

#Model12
gbm_gridsearch <- train(Occupancy ~ ., data = train, 
                        method = 'gbm', 
                        distribution = 'bernoulli', 
                        trControl = ctrl, 
                        verbose = F, 
                        tuneGrid = gbm_grid)
gbm_gridsearch
confusionMatrix(gbm_gridsearch)

# Prediction for Model12
gbm_pred <- predict(gbm_gridsearch, test)
confusionMatrix(gbm_pred, test$Occupancy)

#Model13
gbm_gridsearch_1 <- train(Occupancy ~ .-Humidity-HumidityRatio, data = train, 
                        method = 'gbm', 
                        distribution = 'bernoulli', 
                        trControl = ctrl, 
                        verbose = F, 
                        tuneGrid = gbm_grid)
gbm_gridsearch_1
confusionMatrix(gbm_gridsearch_1)

# Prediction for Model13
gbm_pred_1 <- predict(gbm_gridsearch_1, test)
confusionMatrix(gbm_pred_1, test$Occupancy)
#### 5. EVALUATE BEST MODELS ON TESTING.CSV ####

#best model from each section, apply same test on unseen test data
#finaltest from testing.csv

# Best Logistic Regression Model
#Model7

#Load unseen test data (finaltest: 2665 observations)

finaltestX_dropped <- finaltest[ , -c(2,5,6)]
finaltestX_dropped <- as.matrix(finaltestX_dropped)
head(finaltestX_dropped)

finaltestY <- finaltest[ , -c(1:5)]
finaltestY <- as.matrix(finaltestY)
head(finaltestY)

set.seed(23)
pi_hat_cv_dropped <- predict(modelCV_dropped, newx = finaltestX_dropped, 
                             s = cv_dropped$lambda.min, type = 'response')

Y_hat_cv_dropped <- ifelse(pi_hat_cv_dropped >= 0.5, "1", "0")

N_finaltest <- length(finaltestY)
Error_cv_dropped <- sum((Y_hat_cv_dropped!=finaltestY)/N_finaltest)*100  #model7
round((Accuracy_cv_dropped <- 100 - Error_cv_dropped), 4)

(conf_modcv_dropped <- cbind(table(Y_hat_cv_dropped, 
                                   finaltestY, dnn = c("predict", "true")),
                             Accuracy_cv_dropped))

#ROC
# Model7 (With Lasso)
pred_cv_dropped <- prediction(pi_hat_cv_dropped, finaltestY)
perf_cv_dropped  <- performance(pred_cv_dropped, 'tpr', 'fpr')
plot(perf_cv_dropped, colorize = FALSE, col = 'black')
lines(c(0,1), c(0,1), col = 'gray', lty = 4)
performance(pred_cv_dropped, measure = 'auc')@y.values[[1]]*100

#Best Classification Tree Model
#Model9

# Predict on final test set
yhat_4_finaltest <- predict(pr_tree_4, finaltest, type = 'class') #type: classification
(c_mat <- table(yhat_4_finaltest, finaltest$Occupancy)) #predicted vs actaul 
sum(diag(c_mat))/nrow(finaltest)*100 #classification accuracy %

#Best Bagging and Random Forest Model
#Model11

rf_pred <- predict(rf_occtrain_1, newdata = finaltest) #Model11

finalytest <- finaltest$Occupancy
table(rf_pred, finalytest, dnn = c('Predicted', 'True')) #Model11

#Misclassification rates
(rf_err <- mean(rf_pred != finalytest)) #Model11

#Classification Accuracy
round((Accuracy_rf <- 100 - rf_err*100), 4) #Model11

# Verify answer using built in caret confusion matrix:
confusionMatrix(rf_pred, finalytest) #Model11

#Gradient Boosted Trees
#Model13

gbm_pred_1 <- predict(gbm_gridsearch_1, finaltest)
confusionMatrix(gbm_pred_1, finaltest$Occupancy)
