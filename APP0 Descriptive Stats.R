rm(list=ls())
setwd("C:/Users/user/Desktop/FinTech 2020/Supervised Learning/Assignment 2")

occtrain <- read.csv("occupancy_training.csv", header = TRUE) 
head(occtrain)

str(occtrain)
occtrain$Occupancy <- as.factor(occtrain$Occupancy)
str(occtrain)

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
                     breaks = c(19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23), 
                     limits=c(19, 23))

#Light vs Occupancy
ggplot(data, aes(x = as.numeric(Light), fill = Occupancy)) +
  geom_histogram(binwidth = 25, color="#e9ecef", alpha=0.9) +
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
  geom_histogram(binwidth = 50, color="#e9ecef", alpha=0.9) +
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
  geom_histogram(binwidth = 1, color="#e9ecef", alpha=0.9) +
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
  geom_histogram(binwidth = 0.75e-4, color="#e9ecef", alpha=0.9) +
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
















# Color by group
ggplot(occtrain, aes(factor(Occupancy), fill = factor(Occupancy))) + geom_bar()

#boxplots for entire training set
boxplot(Occupancy ~ Temperature, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 
boxplot(Occupancy ~ HumidityRatio, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 
boxplot(Occupancy ~ Temperature, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 
boxplot(Occupancy ~ Temperature, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 
boxplot(Occupancy ~ Temperature, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 
boxplot(Occupancy ~ Temperature, data = occtrain, main = "Temperature",
        xlab="Temperature", ylab="Occupancy") 

#set graphical parameters
par(mfrow=c(1,1))

#frequency distribution of Tip Percent
hist(occtrain$Temperature, main = "Frequency of Temperature", 
     xlab = "Temperature",breaks = 40, col = 'skyblue3', xaxt='n')
axis(side=1, at=seq(0,100,0.1), labels=seq(0,100,0.1))

#table 1.1
summary(tips)
summary(tips$Total_Bill)
summary(tips$Tip)
summary(tips$`Tip Percent`)
summary(tips$`Party Size`)

#find the mode of Party Size
sort(table(tips$`Party Size`),decreasing=TRUE)[1:6] 
#find the mode of Tip
sort(table(tips$Tip),decreasing=TRUE)[1:6] 

#scatterplot of "Tip Percent" vs "Total Bill" with trend line
plot(tips$`Tip Percent` ~ Total_Bill,
     data = tips, main="Tip Percent vs Total Bill", 
     xlab = "Total_Bill", ylab = "Tip Percent")
abline(lm(tips$`Tip Percent`~tips$Total_Bill), col="red")
cor(tips$Total_Bill, tips$`Tip Percent`)

#scatterplot of "Total Bill" vs "Party Size" with trend line
plot(tips$Total_Bill ~ tips$`Party Size`,
     data = tips, main="Total Bill vs Party Size", 
     xlab = "Party Size", ylab = "Total Bill")
abline(lm(tips$Total_Bill~tips$`Party Size`), col="red")

#scatterplot of "Tip Percent vs Total Bill by Party Size"
require(ggplot2)
ggplot(tips, aes(x = `Total_Bill`, y =  `Tip Percent`)) + 
  geom_point(aes(color = `Party Size`)) +
  xlab("Total Bill") + ylab("Tip Percent") + 
  scale_y_continuous(name="Tip Percent", 
                     limits=c(0, 30), breaks=seq(0,40,5)) +
  ggtitle("Tip Percent vs Total Bill by Party Size")

#ggplot2 scatterplot
ggScatPlot <- ggplot(tips,aes(x = Total_Bill,y = `Tip Percent`)) + 
  geom_point()  #basic scatterplot
ggScatPlot <- ggScatPlot + geom_point(aes(color = Meal)) + 
  scale_y_continuous(name="Tip Percent", 
                     limits=c(0, 30), breaks=seq(0,40,5))
ggScatPlot <- ggScatPlot + facet_grid(~ Meal)   
ggScatPlot <- ggScatPlot + geom_smooth(method = 'lm',formula = y~x) 
ggScatPlot <- ggScatPlot + 
  ggtitle("Tip Percent vs Total Bill by Meal Time") +
  xlab("Total Bill") #add title
ggScatPlot #output graphic

#boxplots of predictor variables
boxplot(`Tip Percent`~`Sex of Tipper`,data=tips, 
        main="Sex of Tipper Data",
        xlab="Sex of Tipper", ylab="Tip Percent") 
boxplot(`Tip Percent`~`Smoker`,data=tips, main="Smoker Data",
        xlab="Smoker", ylab="Tip Percent")
boxplot(`Tip Percent`~`Day`,data=tips, main="Day of Week Data",
        xlab="Day", ylab="Tip Percent")
boxplot(`Tip Percent`~`Meal`,data=tips, main="Mealtime Data",
        xlab="Meal", ylab="Tip Percent")
boxplot(`Tip Percent`~`Party Size`, data=tips, 
        main="Size of Table Data",
        xlab="Size of Table", ylab="Tip Percent")




