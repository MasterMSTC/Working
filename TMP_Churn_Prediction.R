# ORANGE Churn prediction

# LET's Start predicting Churn using Logistic Regression

file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

file_test =
  'C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-20.csv'
final_test_data <- read.csv(file_test, header=TRUE, sep=",")

## As lineary correlated remove
#'Total.day.charge',
#'Total.eve.charge',
#'Total.night.charge',
#'Total.intl.charge'

# And also State and Area.code as categorical...

# data.table for SET
library(data.table)
set(CV_data, j = c('State',
                   'Area.code',
                   'Total.day.charge',
                   'Total.eve.charge',
                   'Total.night.charge',
                   'Total.intl.charge') , value = NULL)


# change categorical to numerical
# ... discuss contrasts()

CV_data$International.plan =
  as.numeric(CV_data$International.plan)-1
CV_data$Voice.mail.plan =
  as.numeric(CV_data$Voice.mail.plan)-1


attach(CV_data)



# Logistic Regression
glm.fit=glm(Churn ~ . ,data=CV_data,family=binomial)
summary(glm.fit)

# Apply Logist Regression Model on Training Data
# type ="response" gives probabilities
glm.probs=predict(glm.fit,type="response")
glm.probs[15:20]
Churn[15:20]


# Basic Confusion matrix en MCE with Threshold 0.5
glm.pred=rep ("False" ,2666)
glm.pred[glm.probs >.5]="True"

table(glm.pred ,Churn)
mean(glm.pred==Churn)

# WHAT IF all predictions == False (majority class)
glm.pred=rep ("False" ,2666)
table(glm.pred ,Churn)
mean(glm.pred==Churn)


# TEST ERROR
# first prepare test data in a similar way
# as training data 

# Drop categorical & highly correlated data
set(final_test_data, j = c('State',
                   'Area.code',
                   'Total.day.charge',
                   'Total.eve.charge',
                   'Total.night.charge',
                   'Total.intl.charge') , value = NULL)

final_test_data$International.plan =
  as.numeric(final_test_data$International.plan)-1
final_test_data$Voice.mail.plan =
  as.numeric(final_test_data$Voice.mail.plan)-1

glm.probs =predict (glm.fit ,final_test_data , type="response")


glm.pred=rep("False" ,667)
glm.pred[glm.probs >.2]="True"

table(glm.pred ,final_test_data$Churn)

mean(glm.pred==final_test_data$Churn)

# WHAT IF all predictions == False (majority class)
glm.pred=rep ("False" ,667)
table(glm.pred ,final_test_data$Churn)
mean(glm.pred==final_test_data$Churn)

## DISCUSS unbalanced data !!!

# change threshold , plot ROC, DET Curves

library(ROCR)
pred <- prediction(glm.probs, final_test_data$Churn)
perf <- performance(pred,"tpr","fpr")

windows()
plot(perf)
dev.off()

# Let's analyze error as in DET curves
perf <- performance(pred,"miss","fpr")
perf@x.name
perf@y.name
perf@alpha.name

windows()
plot(perf)
dev.off()

plot(unlist(perf@alpha.values),unlist(perf@x.values),
     xlab=perf@alpha.name,ylab=perf@x.name,col='red')
lines(unlist(perf@alpha.values),unlist(perf@y.values),
      xlab=perf@alpha.name,ylab=perf@y.name)




## precision/recall curve (x-axis: recall, y-axis: precision)
windows()
perf1 <- performance(pred, "prec", "rec")
plot(perf1)
dev.off()

performance(pred, "auc")@y.values[[1]]

library(caret)

precision <- posPredValue(as.factor(glm.pred),
                          final_test_data$Chur, positive="True")
recall <- sensitivity(as.factor(glm.pred),
                      final_test_data$Chur, positive="True")

F1 <- (2 * precision * recall) / (precision + recall)

## SOME POSSIBLE STUDIES

# use only some features...

# TRY LDA  lda is part of MASS library

library (MASS)
lda.fit=lda(Churn ∼ . ,data=CV_data)
lda.fit

windows()
plot(lda.fit)
dev.off()

lda.predict=predict (lda.fit , final_test_data)


# class: contains LDA’s predictions 
# posterior: is a matrix whose kth column contains the
# posterior probability that the corresponding
# observation belongs to the kth class 
# x :  contains the linear discriminants

names(lda.predict)


lda.pred=rep("False" ,667)
lda.pred[lda.predict$posterior[,2] >.5]="True"

table(lda.pred ,final_test_data$Churn)

mean(lda.pred==final_test_data$Churn)

# Quadratic Discriminant Analysis

qda.fit=qda(Churn ∼ . ,data=CV_data)

qda.predict=predict (qda.fit , final_test_data)

qda.pred=rep("False" ,667)
qda.pred[qda.predict$posterior[,2] >.5]="True"

table(qda.pred ,final_test_data$Churn)

mean(qda.pred==final_test_data$Churn)





