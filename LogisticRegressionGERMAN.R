library(readr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(caret)
library(lattice)
library(pROC)
library(dplyr)
library(tidyverse)
library(caTools)

install.packages("readr", type = "source")

loan <- read.csv("C:/Users/basti/Desktop/Analytical Statistics/data for practice.csv")
loan
setwd("C:/Users/basti/Desktop/Analytical Statistics")

loan[1:3,]

loan$Default

set.seed(123)
split = sample.split(loan$Default,SplitRatio = 0.8)
train_set = subset(loan, split = TRUE)
test_set = subset (loan, split= FALSE )


LogisticModel2 = glm(loan$Default ~.,
                    family=binomial, 
                    data = train_set)

summary(LogisticModel2)

library(ROCR)
fit_LG = predict(LogisticModel2,test_set, type =c('response' ))
true_fit = test_set[,1]
true_fit
data.frame(true_fit, fit_LG)

pred = prediction(fit_LG, true_fit,label.ordering = NULL)
perf = performance(pred, measure = 'tpr',x = 'fpr')
plot(perf, colorize= 'TRUE', main="ROC_LG", xlab="Specificity", 
     ylab="Sensitivity")  
abline(0, 1) #add a 45 degree line

#Setting Cut-off point of 0,5, a new variable gg1
gg22 = floor(fit_LG+ (1/2)) 
gg22=factor(gg22,levels = c(0,1))
data_frame(true_fit,gg22)
confusionMatrix(table(true_fit,gg22))

#________________________________________________________________________

#Linear regression model

model_lm = lm(loan$Default ~ loan$duration + loan$amount + 
                loan$installment + loan$age + loan$cards+ loan$liable, data = train_set)
summary(model_lm)

fit_lm = predict(model_lm,type =c('response' ))

pred_lm = prediction(fit_lm, loan$Default)
perf_lm = performance(pred_lm, measure = 'tpr',x = 'fpr')
plot(perf_lm, colorize= 'TRUE', main="ROC_lm", xlab="Specificity", 
     ylab="Sensitivity")  
abline(0, 1) #add a 45 degree line

gg22 = floor(fit_lm+ (1/2)) 
gg22=factor(gg22,levels = c(0,1))
data_frame(true_fit,gg22)
confusionMatrix(table(true_fit,gg22))
#Evaluating the model

AUCLog<-performance(pred_lm,measure='auc')@y.values[[1]]

