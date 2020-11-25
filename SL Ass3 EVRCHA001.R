#### Exploratory Data Analysis ####
rm(list=ls())
setwd("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3")
getwd() 

library(e1071)
load("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3/framingham.RData")
full <- framingham.assignment
str(full)
head(full)
full$TenYearCHD
summary(full)

# NA values in the TenYearCHD column are assigned to finaltest
x <- which(is.na(full$TenYearCHD))
x
finaltest <- full[x,]
finaltest
row.names(finaltest) <- 1:nrow(finaltest)
finaltest$traintest <- NULL
str(finaltest)

#fulltrain built from all entries with known TenYearCHD column values
fulltrain <- full[-x,]
fulltrain
row.names(fulltrain) <- 1:nrow(fulltrain)
fulltrain$traintest <- NULL
str(fulltrain)

df <- fulltrain
library(tidyverse)
library(ggcorrplot)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(data.table)
library(e1071) 
library(scales)
library(stringr)
library(gridGraphics)
library(gridExtra)
library(cowplot)
library(lmtest)
library(gvlma)
library(RColorBrewer)
library(packHV)
library(caTools)
library(DataExplorer)
library(ggcorrplot)

## Target variable as.factor/as.numeric
# Heart disease as factor with levels "No" & "Yes"

df$TenYearCHD <- as.factor(df$TenYearCHD)
df <- df %>% mutate(HD = TenYearCHD)
df$TenYearCHD = factor(df$TenYearCHD,
                       labels = c('No', 'Yes'))

# rename columns 
df <- df %>% rename("Heart Disease" = TenYearCHD,
                    Gender = male,
                    Age = age,
                    Education = education,
                    Smoker = currentSmoker)

## Data Types

df$Gender = factor(df$Gender, 
                   labels = c("Female", "Male"))

# replace missing values with numeric
df$Education <- df$Education %>% replace_na(5)

# 1 = Some High School
# 2 = High School or GED
# 3 = Some College or Vocational School
# 4 = College
# 5 = Unknown

df$Education = factor(df$Education, 
                      labels = c("Some High School",
                                 "High School or GED",
                                 "Some College/Vocational School",
                                 "College", "Unknown"))

df$Smoker = factor(df$Smoker, labels = c("Smoker", "Non-Smoker"))

df$BPMeds <- df$BPMeds %>% replace_na(0)
df$BPMeds = factor(df$BPMeds, labels = c("No Medication", "Medication"))

df$prevalentStroke = factor(df$prevalentStroke,
                            labels = c("No", "Yes"))

df$prevalentHyp = factor(df$prevalentHyp, 
                         labels = c("No", "Yes"))

df$diabetes = factor(df$diabetes, labels = c("No", "Yes"))

# Age as a categorical data type

#hist(df$Age, col = "grey40")
b = seq(min(df$Age)-3, max(df$Age)+8, 10);
df$A <- cut(df$Age, breaks = b, right = T)
table(df$A)

# Organize dataset with factors first
df <- df %>% select(HD,`Heart Disease`, A, Gender, Education, Smoker, 
                    BPMeds, prevalentStroke, everything())

# Missing values
plot_missing(df[,-c(1,3)]) + theme_bw()

# glucose is the only variable with more than 2% missing values
# replace all NA's with the median of glucose
df$glucose = replace_na(df$glucose, median(df$glucose, na.rm = T))
df$glucose

## Barplots of Categorical Data  

# Heart Disease counts
a = ggplot(df, aes(`Heart Disease`, fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_bar(stat = "count", lwd = 2) + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) + 
  labs(title = "Heart Disease") +  theme_bw(base_size = 18) +
  theme(legend.position="bottom")

# Gender
b = ggplot(df, aes(Gender, fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge", lwd = 2) + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  labs(title = "Gender", x = "") +  theme_bw(base_size = 18) +
  theme(legend.position="bottom")

options(repr.plot.width=16, repr.plot.height=7)
plot_grid(a,b, ncol = 2, nrow = 1)

# Age 
c = ggplot(df, aes(A, fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge", lwd = 2) + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  ggtitle("Age") +  theme(plot.title = element_text(hjust = 0.5))
theme_bw(base_size = 18) + theme(legend.position="bottom")

options(repr.plot.width=16, repr.plot.height=7)
plot_grid(c, ncol = 1, nrow = 1)

## Boxplots by Age & Heart Disease   


# Systolic Blood Pressure
a = ggplot(df, aes(A, sysBP, fill = `Heart Disease`, 
                   color = `Heart Disease`)) +
  geom_boxplot(lwd = 2) + labs(title = "Age & Systolic BP",
                               y = "Systolic BP", x = "Age") + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  theme_bw(base_size = 18) + theme(legend.position="bottom")

# Diastolic Blood Pressure 
b = ggplot(df, aes(A, diaBP, fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_boxplot(lwd = 2) + labs(title = "Age & Diastolic BP",
                               y = "Diastolic BP", x = "Age") + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  theme_bw(base_size = 18) + theme(legend.position="bottom")

options(repr.plot.width=16, repr.plot.height=7)
plot_grid(a,b, ncol = 2, nrow = 1)

# Age by Total Cholesterol 
a = ggplot(df, aes(A, totChol , fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_boxplot(lwd = 2) + labs(title = "Age & Total Cholesterol", 
                               y = "Total Cholesterol", x = "Age") + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  theme_bw(base_size = 18) + theme(legend.position="bottom")

# Age by BMI 
b = ggplot(df, aes(A, BMI, fill = `Heart Disease`, 
                   color = `Heart Disease`)) + 
  geom_boxplot(lwd = 2) + labs(title = "Age & Body Mass Index", 
                               y = "BMI", x = "Age") + 
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  theme_bw(base_size = 18) + theme(legend.position="bottom")

options(repr.plot.width=16, repr.plot.height=7)
plot_grid(a,b, ncol = 2, nrow = 1)

## Visualize the Data Distributions/Interactions 

# Age & Heart Disease
a = ggplot(df,aes(Age, fill = `Heart Disease`, 
                  color = `Heart Disease`)) + 
  geom_density(lwd = 3, show.legend = T, alpha = 0.7) + 
  labs(title = "Age & Hypertension") + facet_grid(~prevalentHyp) +
  scale_fill_manual(values=c("grey69", "red")) + 
  scale_color_manual(values=c('grey29', "black")) +
  theme_bw(base_size = 18) + theme(legend.position="bottom") +
  theme(strip.background = element_rect(fill="snow1")) +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  geom_vline(xintercept = mean(df$Age), lwd = 1, 
             color = "black", linetype = 4)

options(repr.plot.width = 16, repr.plot.height = 7)
plot_grid(a, ncol = 1, nrow = 1)

## skewness of distributions
par(mfrow=c(2,4))
a = hist(df$Age, main = "Age", col = "#FF99FF");
b = hist(df$cigsPerDay, main = "Cigs Per Day", col = "#FF33FF");
c = hist(df$totChol, main = "Total Cholesterol", col = "#FF00FF");
d = hist(df$glucose, main = "Glucose", col = "#CC33CC");
a = hist(df$sysBP, main = "sysBP", col = "#CC33CC");
b = hist(df$diaBP, main = "diaBP",col = "#6600FF");
c = hist(df$BMI, main = "BMI", col = "#0000FF");
d = hist(df$heartRate, main = "heartRate", col = "#3333FF");

skewness(na.omit(df$cigsPerDay)) 
skewness(na.omit(df$totChol)) 
skewness(na.omit(df$sysBP)) 
skewness(na.omit(df$diaBP)) 
skewness(na.omit(df$BMI)) 
skewness(na.omit(df$heartRate)) 
skewness(na.omit(df$glucose)) #requires evaluating

## removing outliers

# Glucose
# 50% of the data falls between the quantiles
Q <- quantile(df$glucose, probs=c(.25, .75), na.rm = T);
iqr <- IQR(df$glucose, na.rm = T);

# as per Tukey's test
# remove the outlier beyond 1.5 * iqr for Glucose
df2 <- df %>% filter(glucose > (Q[1] - 1.5*iqr) & 
                       glucose < (Q[2] + 1.5*iqr))  

# visualize the new dataset without outliers

par(mfrow=c(2,1))
options(repr.plot.width=10, repr.plot.height=10)
boxplot(df$glucose, col = "grey40", horizontal = T, 
        main = "Glucose - Before")
boxplot(df2$glucose, col = "#33FF33", horizontal = T, 
        main = "Glucose - After")

# Cholesterol
# 50% of the data falls between the quantiles
Q <- quantile(df2$totChol, probs=c(.25, .75), na.rm = T);
iqr <- IQR(df2$totChol, na.rm = T);

# as per Tukey's test
# remove the outlier beyond 1.5 * iqr for Glucose
df2 <- df2 %>% filter(totChol > (Q[1] - 1.5*iqr) & 
                        totChol < (Q[2] + 1.5*iqr))  

# visualize the new dataset without outliers
par(mfrow=c(2,1))
boxplot(df$totChol, col = "grey40", horizontal = T, 
        main = "Cholesterol - Before ")
boxplot(df2$totChol, col = "#33FF33", horizontal = T, 
        main = "Cholesterol - After ")
par(mfrow=c(1,1))

## Correlations (outliers removed)

c2 <- df2 %>% select(everything(), - `Heart Disease`)
#c2 %>% select_if(is.factor) %>% names()
cols = c("HD",
         "Gender",
         "Education",
         "Smoker",         
         "BPMeds",
         "prevalentStroke",
         "prevalentHyp",
         "diabetes",       
         "A")
c2[, cols] <- c2 %>% select(all_of(cols)) %>% lapply(as.numeric)

cor <- cor(c2)
cols = c("HD",
         "Gender",
         "Age",
         "Education",      
         "Smoker",
         "cigsPerDay",
         "BPMeds",
         "prevalentStroke",
         "prevalentHyp",
         "diabetes",
         "totChol",
         "sysBP",          
         "diaBP",
         "BMI",
         "heartRate",
         "glucose",
)

cor <- as_tibble(reshape2::melt(cor, id = cols))
colnames(cor) <- c("Target", "Variable", "Correlation")

C <- cor[which(cor$Target == "HD"),]
C <- C[order(- abs(C$Correlation)), ]
C <- subset(C, abs(C$Correlation) > 0.10)

# Select & Order by correlation
cr2 <- c2 %>% select("HD",
                     "Age",
                     "sysBP",
                     "prevalentHyp",
                     "diaBP")

##  Correlation Plot & Pairs Panel   

par(mfrow=c(1,1))
col <- colorRampPalette(c("black", "white", "magenta"))(20)
heatmap(x = corr, col = col, symm = TRUE, margins = c(10, 10))
legend(x = "top", legend = c("High Corr", "Low Corr"), 
       fill = colorRampPalette(c("magenta", "black"))(2))

library(ggcorrplot)
corr <- round(cor(cr2, use="complete.obs"), 2)
ggcorrplot(corr, lab = TRUE, colors = 
             c("magenta", "white", "magenta"), 
           show.legend = F, outline.color = "gray",
           type = "upper", #hc.order = T,  
           tl.cex = 20, lab_size = 8, sig.level = .2) +
  labs(fill = "Correlation")
#### Support Vector Machines #### 
rm(list=ls())
setwd("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3")
getwd() 

# kernlab does not require data standardisation 
# before building SVM models 1-4
# Data only standardised for models 5-9 (e1071)

library(kernlab)
load("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3/framingham.RData")
full <- framingham.assignment
str(full)
head(full)
full$TenYearCHD
summary(full)

x <- which(is.na(full$TenYearCHD))
x
finaltest <- full[x,]
finaltest
row.names(finaltest) <- 1:nrow(finaltest)
finaltest$traintest <- NULL
str(finaltest)

fulltrain <- full[-x,]
fulltrain
row.names(fulltrain) <- 1:nrow(fulltrain)
fulltrain$traintest <- NULL
str(fulltrain)

#set 0 and 1 to female and male
for (i in 1:nrow(fulltrain)) {
  if (fulltrain$male[i] == 1) {
    fulltrain$male[i] <- "Male"
  } else {
    fulltrain$male[i] <- "Female"
  }
}
fulltrain[,1] 
fulltrain

#change column name: Male <- Gender
colnames(fulltrain)[1] <- "Gender"
colnames(fulltrain)

#set 0 and 1 to no and yes
for (i in 1:nrow(fulltrain)) {
  if (fulltrain$TenYearCHD[i] == 0) {
    fulltrain$TenYearCHD[i] <- "No"
  } else {
    fulltrain$TenYearCHD[i] <- "Yes"
  }
}
fulltrain[,16] 
fulltrain

#change column name: TenYearCHD <- RiskOfCHD
colnames(fulltrain)[16] <- "RiskOfCHD"
colnames(fulltrain)

#set 0 and 1 to female and male
for (i in 1:nrow(finaltest)) {
  if (finaltest$male[i] == 1) {
    finaltest$male[i] <- "Male"
  } else {
    finaltest$male[i] <- "Female"
  }
}
finaltest[,1] 
finaltest

#change column name: TenYearCHD <- RiskOfCHD
colnames(fulltrain)[16] <- "RiskOfCHD"
colnames(fulltrain)

#change column name: TenYearCHD <- RiskOfCHD
colnames(finaltest)[16] <- "RiskOfCHD"
colnames(finaltest)

#omit NA's
fulltrain
fulltrain <- na.omit(fulltrain)
row.names(fulltrain) <- 1:nrow(fulltrain)
str(fulltrain) # 2928 observations

finaltest
finaltest$RiskOfCHD <- 0 
finaltest$RiskOfCHD
finaltest #since entire column made of NA's, all will be deleted
finaltest <- na.omit(finaltest)
row.names(finaltest) <- 1:nrow(finaltest)
str(finaltest) # 730 observations

#80/20 train/test split on fulltrain
library(caret)
set.seed(23)
samp <- createDataPartition(fulltrain[,"RiskOfCHD"],1,.8)[[1]]
samp

#80% train split (2344 observations)
train = fulltrain[samp,]
train$RiskOfCHD <- as.factor(train$RiskOfCHD)
head(train)
str(train)

#20% test split (584 observationa)
test = fulltrain[-samp,]
test$RiskOfCHD <- as.factor(test$RiskOfCHD)
head(test)
str(test)

# Helper packages
library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics
library(rsample)  # for data splitting

# Modeling packages
library(caret)    # for classification and regression training
library(kernlab)  # for fitting SVMs

# Model interpretability packages
library(pdp)      # for partial dependence plots, etc.
library(vip)      # for variable importance plots

# Data standardisation not required when using kernlab
# Tune an SVM with radial basis kernel using 10-fold CV
set.seed(23) 
# takes approx 60 secs
svm_1 <- train(
  RiskOfCHD ~ ., 
  data = train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10)

svm_1
ggplot(svm_1) + theme_light() + 
  geom_vline(xintercept = 2, col = "red", lty = 5) + 
  annotate(geom = 'text', label = 'C = 2', x = 2, y = 0.805, col = "red")

svm_1$results
svm_1$bestTune
confusionMatrix(svm_1)

# Predicting the Test set results 
y_pred_1 = predict(svm_1, newdata = test)
y_pred_1

# Making the Confusion Matrix 
CM_1 = table(test$RiskOfCHD, y_pred_1) 
CM_1

# Control params for SVM
ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary  # also needed for AUC/ROC
)

# Tune a Radial SVM determined when AUC maximised
set.seed(23)  

svm_2 <- train(
  RiskOfCHD ~ ., 
  data = train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  metric = "ROC",  # area under ROC curve (AUC)       
  trControl = ctrl,
  tuneLength = 10)
svm_2

# Print results
svm_2$results
confusionMatrix(svm_2)

# Predicting the Test set results 
y_pred_2 = predict(svm_2, newdata = test)
y_pred_2

# Confusion Matrix 
CM_2 = table(test$RiskOfCHD, y_pred_2) 
CM_2

# create a funtion for vip
prob_yes <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "Yes"]
}

# variable importance plot
set.seed(23)  # for reproducibility
vip(svm_2, method = "permute", nsim = 5, train = train, 
    target = "RiskOfCHD", metric = "auc", reference_class = "Yes", 
    pred_wrapper = prob_yes)

features <- c("age","sysBP","diaBP", "BMI", 
              "heartRate","cigsPerDay",
              "totChol", "prevalentHyp")

# create a partial dependance plot function
pdps <- lapply(features, function(x) {
  partial(svm_2, pred.var = x, which.class = 2,  
          prob = TRUE, plot = TRUE, plot.engine = "ggplot2") +
    coord_flip() 
})
grid.arrange(grobs = pdps,ncol = 2) 

# SVM model 3 - Radial excl prevalentHyp, cigsPerDay, education, totChol
set.seed(23) 
# takes approx 60 secs
svm_3 <- train(
  RiskOfCHD ~ .-prevalentHyp-cigsPerDay-totChol-education, 
  data = train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10)
svm_3

svm_3$results
svm_3$bestTune
confusionMatrix(svm_3)

# Predicting the Test set results 
y_pred_3 = predict(svm_3, newdata = test)
y_pred_3

# Making the Confusion Matrix 
CM_3 = table(test$RiskOfCHD, y_pred_3) 
CM_3

# SVM model 4 - Radial with age + sysBP + diaBP + BMI + totChol
set.seed(23) 
# takes approx 60 secs
svm_4 <- train(
  RiskOfCHD ~ age + sysBP + diaBP + BMI + totChol, 
  data = train,
  method = "svmRadial",               
  preProcess = c("center", "scale"),  
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10)
svm_4

svm_4$results
svm_4$bestTune
confusionMatrix(svm_4)

# Predicting the Test set results 
y_pred_4 = predict(svm_4, newdata = test)
y_pred_4

# Making the Confusion Matrix 
CM_4 = table(test$RiskOfCHD, y_pred_4) 
CM_4

# Reload data and apply data normalisation for svm_5 - svm_9
# e1071 requires data standardisation

library(e1071)
load("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3/framingham.RData")
full <- framingham.assignment
str(full)
head(full)
full$TenYearCHD
summary(full)

x <- which(is.na(full$TenYearCHD))
x
finaltest <- full[x,]
finaltest
row.names(finaltest) <- 1:nrow(finaltest)
finaltest$traintest <- NULL
str(finaltest)

fulltrain <- full[-x,]
fulltrain
row.names(fulltrain) <- 1:nrow(fulltrain)
fulltrain$traintest <- NULL
str(fulltrain)

fulltrain[c(2,5,10,11,12,13,14,15)] <- 
  lapply(fulltrain[c(2,5,10,11,12,13,14,15)], 
  function(x) c(scale(x)))
str(fulltrain)

#change column name: TenYearCHD <- RiskOfCHD
colnames(fulltrain)[16] <- "RiskOfCHD"
colnames(fulltrain)

#change column name: TenYearCHD <- RiskOfCHD
colnames(finaltest)[16] <- "RiskOfCHD"
colnames(finaltest)

#omit NA's
fulltrain
fulltrain <- na.omit(fulltrain)
row.names(fulltrain) <- 1:nrow(fulltrain)
str(fulltrain) # 2928 observations

finaltest
finaltest$RiskOfCHD <- 0 
finaltest$RiskOfCHD
finaltest #since entire column made of NA's, all will be deleted
finaltest <- na.omit(finaltest)
row.names(finaltest) <- 1:nrow(finaltest)
str(finaltest) # 730 observations

#80/20 train/test split on fulltrain
library(caret)
set.seed(23)
samp <- createDataPartition(fulltrain[,"RiskOfCHD"],1,.8)[[1]]
samp

#80% train split (2343 observations)
train = fulltrain[samp,]
train$RiskOfCHD <- as.factor(train$RiskOfCHD)
row.names(train) <- 1:nrow(train)
head(train)
str(train)

#20% test split (585 observationa)
test = fulltrain[-samp,]
test$RiskOfCHD <- as.factor(test$RiskOfCHD)
row.names(test) <- 1:nrow(test)
head(test)
str(test)

# untuned linear kernal using all variables
set.seed(23)
svm_5 = svm(as.factor(RiskOfCHD) ~ .,
            data = train,
            type = 'C-classification',
            kernel = "linear",
            scale = FALSE,
            cost = 1, gamma = 0.1)
svm_5

train_pred_5 = predict(svm_5, train)
mean(train_pred_5 == as.factor(train$RiskOfCHD))

y_pred_5 = predict(svm_5, test)
mean(y_pred_5 == as.factor(test$RiskOfCHD))

# Making the Confusion Matrix 
CM_5 = table(test$RiskOfCHD, y_pred_5) 
CM_5

# tuned linear kernal using all variables
set.seed(23)
svm_6 = tune.svm(as.factor(RiskOfCHD) ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = "linear",
                 scale = FALSE,
                 cost = c(1e-7, 1e-3, 1, 10),
                 gamma = c(1e-7, 1e-3, 1))

svm_6$best.performance #lowest error
svm_6$best.model
svm_6$best.parameters

y_pred_6 = predict(svm_6$best.model, test)
mean(y_pred_6 == as.factor(test$RiskOfCHD))

# Making the Confusion Matrix 
CM_6 = table(test$RiskOfCHD, y_pred_6) 
CM_6

# untuned radial kernal using all variables
set.seed(23)
svm_7 = svm(as.factor(RiskOfCHD) ~ .,
            data = train,
            type = 'C-classification',
            kernel = "radial",
            scale = FALSE,
            cost = 1, gamma = 0.1)
svm_7

train_pred_7 = predict(svm_7, train)
mean(train_pred_7 == as.factor(train$RiskOfCHD))

y_pred_7 = predict(svm_7, test)
mean(y_pred_7 == as.factor(test$RiskOfCHD))

# Making the Confusion Matrix 
CM_7 = table(test$RiskOfCHD, y_pred_7) 
CM_7

# tuned radial kernal using all variables
set.seed(23)
svm_8 = tune.svm(as.factor(RiskOfCHD) ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = "radial",
                 scale = FALSE,
                 cost = c(1e-3, 1, 5, 10, 15),
                 gamma = c(1e-7, 1e-3, 1))
summary(svm_8)
svm_8$best.performance #lowest error
svm_8$best.model
svm_8$best.parameters

train_pred_8 = predict(svm_8$best.model, train)
mean(train_pred_8 == as.factor(train$RiskOfCHD))

y_pred_8 = predict(svm_8$best.model, test)
mean(y_pred_8 == as.factor(test$RiskOfCHD))

# Making the Confusion Matrix 
CM_8 = table(test$RiskOfCHD, y_pred_8) 
CM_8

# Tune radial kernal using only age + sysBP
set.seed(23)
svm_9 = tune.svm(as.factor(RiskOfCHD) ~ age + sysBP,
                  data = train,
                  type = 'C-classification',
                  kernel = "radial",
                  scale = FALSE,
                  cost = c(1e-3, 1, 5),
                  gamma = c(1e-7, 1e-3, 1))
summary(svm_9)
svm_9$best.performance
svm_9$best.model
svm_9$best.parameters

train_pred_9 = predict(svm_9$best.model, train)
mean(train_pred_9 == as.factor(train$RiskOfCHD))

y_pred_9 = predict(svm_9$best.model, test)
mean(y_pred_9 == as.factor(test$RiskOfCHD))

# Making the Confusion Matrix 
CM_9 = table(test$RiskOfCHD, y_pred_9) 
CM_9

#### Neural Networks ####

## neural net package
rm(list=ls())
library(neuralnet)
load("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3/framingham.RData")
full <- framingham.assignment
str(full)
head(full)
full$TenYearCHD
summary(full)

x <- which(is.na(full$TenYearCHD))
x
finaltest <- full[x,]
finaltest
row.names(finaltest) <- 1:nrow(finaltest)
finaltest$traintest <- NULL
str(finaltest)

finaltest[c(2,5,10,11,12,13,14,15)] <- 
  lapply(finaltest[c(2,5,10,11,12,13,14,15)], 
         function(x) c(scale(x)))
str(finaltest)

fulltrain <- full[-x,]
fulltrain
row.names(fulltrain) <- 1:nrow(fulltrain)
fulltrain$traintest <- NULL
str(fulltrain)

fulltrain[c(2,5,10,11,12,13,14,15)] <- 
  lapply(fulltrain[c(2,5,10,11,12,13,14,15)], 
         function(x) c(scale(x)))
str(fulltrain)

#change column name: TenYearCHD <- RiskOfCHD
colnames(fulltrain)[16] <- "RiskOfCHD"
colnames(fulltrain)

#change column name: TenYearCHD <- RiskOfCHD
colnames(finaltest)[16] <- "RiskOfCHD"
colnames(finaltest)

#omit NA's
fulltrain
fulltrain <- na.omit(fulltrain)
row.names(fulltrain) <- 1:nrow(fulltrain)
str(fulltrain) # 2928 observations

finaltest
finaltest$RiskOfCHD <- 0 
finaltest$RiskOfCHD
finaltest #since entire column made of NA's, all will be deleted
finaltest <- na.omit(finaltest)
row.names(finaltest) <- 1:nrow(finaltest)
str(finaltest) # 730 observations
finaltest[c(2,5,10,11,12,13,14,15)] <- 
  lapply(finaltest[c(2,5,10,11,12,13,14,15)], 
         function(x) c(scale(x)))
str(finaltest)

#80/20 train/test split on fulltrain
library(caret)
set.seed(23)
samp <- createDataPartition(fulltrain[,"RiskOfCHD"],1,.8)[[1]]
samp

#80% train split (2343 observations)
train = fulltrain[samp,]
train$RiskOfCHD <- as.factor(train$RiskOfCHD)
row.names(train) <- 1:nrow(train)
head(train)
str(train)

#20% test split (585 observationa)
test = fulltrain[-samp,]
test$RiskOfCHD <- as.factor(test$RiskOfCHD)
row.names(test) <- 1:nrow(test)
head(test)
str(test)

## Start of NN code

## model 1 - hidden = c(2) 
set.seed(23)
neuralnetmodel_1 = neuralnet(RiskOfCHD ~ ., 
                           data = train, 
                           linear.output = FALSE, 
                           act.fct = "logistic",
                           hidden = c(2), 
                           err.fct = "ce")
plot(neuralnetmodel_1)
neuralnetmodel_1$weights
neuralnetmodel_1$err.fct
neuralnetmodel_1$act.fct

train.predict_1 = predict(neuralnetmodel_1, train)
head(round(train.predict_1,3), 6)

maxprobability <- apply(train.predict_1, 1, which.max) 
maxprobability

train.predict.class_1 <- c(0, 1)[maxprobability] 
head(train.predict.class_1)
train.predict.class_1

table(train.predict.class_1, train$RiskOfCHD) 
mean(train.predict.class_1 == train$RiskOfCHD)*100

# classification accuracy on the test set

test.predict_1 = predict(neuralnetmodel_1, test)
head(test.predict_1)

maxprobability <- apply(test.predict_1, 1, which.max) 
test.predict.class_1 <- c(0,1)[maxprobability] 
head(test.predict.class_1)
table(test.predict.class_1, test$RiskOfCHD) 
mean(test.predict.class_1 == test$RiskOfCHD)*100

# final prediction on unseen test set (730 observations)
# (best model to be written to .csv)

test.predict_1_csv = predict(neuralnetmodel_1, finaltest)
head(test.predict_1_csv)

maxprobability <- apply(test.predict_1_csv, 1, which.max) 
test.predict.class_1_csv <- c(0,1)[maxprobability] 
head(test.predict.class_1_csv)
test.predict.class_1_csv

# write to .csv with student number
write.csv(test.predict.class_1_csv,
          "C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 3\\EVRCHA001.csv", 
          row.names = TRUE)

## model 2 - hidden = c(5)
set.seed(23)
neuralnetmodel_2 = neuralnet(RiskOfCHD ~ ., 
                             data = train, 
                             linear.output = FALSE, 
                             act.fct = "logistic",
                             hidden = c(5), 
                             err.fct = "ce",
                             stepmax = 10^6)
plot(neuralnetmodel_2)
neuralnetmodel_2$weights

train.predict_2 = predict(neuralnetmodel_2, train)
head(round(train.predict_2,3), 6)

maxprobability <- apply(train.predict_2, 1, which.max) 
maxprobability

train.predict.class_2 <- c(0, 1)[maxprobability] 
head(train.predict.class_2)
train.predict.class_2

table(train.predict.class_2, train$RiskOfCHD) 
mean(train.predict.class_2 == train$RiskOfCHD)*100

# classification accuracy on the test set

test.predict_2 = predict(neuralnetmodel_2, test)
head(test.predict_2)

maxprobability <- apply(test.predict_2, 1, which.max) 
test.predict.class_2 <- c(0,1)[maxprobability] 
head(test.predict.class_2)
table(test.predict.class_2, test$RiskOfCHD) 
mean(test.predict.class_2 == test$RiskOfCHD)*100

## model 3 - hidden = c(10)
set.seed(23)
neuralnetmodel_3 = neuralnet(RiskOfCHD ~ ., 
                             data = train, 
                             linear.output = FALSE, 
                             act.fct = "logistic",
                             hidden = c(10), 
                             err.fct = "ce")
plot(neuralnetmodel_3)
neuralnetmodel_3$weights

train.predict_3 = predict(neuralnetmodel_3, train)
head(round(train.predict_3,3), 6)

maxprobability <- apply(train.predict_3, 1, which.max) 
maxprobability

train.predict.class_3 <- c(0, 1)[maxprobability] 
head(train.predict.class_3)
train.predict.class_3

table(train.predict.class_3, train$RiskOfCHD) 
mean(train.predict.class_3 == train$RiskOfCHD)*100

# classification accuracy on the test set

test.predict_3 = predict(neuralnetmodel_3, test)
head(test.predict_3)

maxprobability <- apply(test.predict_3, 1, which.max) 
test.predict.class_3 <- c(0,1)[maxprobability] 
head(test.predict.class_3)
table(test.predict.class_3, test$RiskOfCHD) 
mean(test.predict.class_3 == test$RiskOfCHD)*100

## model 4 - hidden = c(2,2)
set.seed(23)
neuralnetmodel_4 = neuralnet(RiskOfCHD ~ age + sysBP + diaBP + diabetes, 
                             data = train, 
                             linear.output = FALSE, 
                             act.fct = "logistic",
                             hidden = c(2,2), 
                             err.fct = "ce",
                             stepmax = 10^5)
plot(neuralnetmodel_4)
neuralnetmodel_4$weights

train.predict_4 = predict(neuralnetmodel_4, train)
head(round(train.predict_4,3), 6)

maxprobability <- apply(train.predict_4, 1, which.max) 
maxprobability

train.predict.class_4 <- c(0, 1)[maxprobability] 
head(train.predict.class_4)
train.predict.class_4

table(train.predict.class_4, train$RiskOfCHD) 
mean(train.predict.class_4 == train$RiskOfCHD)*100

# classification accuracy on the test set

test.predict_4 = predict(neuralnetmodel_4, test)
head(test.predict_4)

maxprobability <- apply(test.predict_4, 1, which.max) 
test.predict.class_4 <- c(0,1)[maxprobability] 
head(test.predict.class_4)
table(test.predict.class_4, test$RiskOfCHD) 
mean(test.predict.class_4 == test$RiskOfCHD)*100