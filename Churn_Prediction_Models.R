# ORANGE Churn prediction

# LET's Start predicting Churn using Logistic Regression

file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")


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

## Reading TEST DATA

file_test =
  'C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-20.csv'
final_test_data <- read.csv(file_test, header=TRUE, sep=",")

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
glm.pred[glm.probs >.5]="True"

table(glm.pred ,final_test_data$Churn)

mean(glm.pred==final_test_data$Churn)

# WHAT IF all predictions == False (majority class)
#glm.pred=rep ("False" ,667)
#table(glm.pred ,final_test_data$Churn)
#mean(glm.pred==final_test_data$Churn)

## DISCUSS unbalanced data !!!

# change threshold , plot ROC, DET Curves

library(ROCR)
pred <- prediction(glm.probs, final_test_data$Churn)
perf <- performance(pred,"tpr","fpr")

windows()
plot(perf)
dev.off()

# A SINGLE VALUE : AUC
add

AUC@y.values

# Let's analyze error as in DET curves
perf <- performance(pred,"miss","fpr")

windows()
plot(perf)
dev.off()

plot(perf@alpha.values,perf@x.values)

windows()
plot(unlist(perf@alpha.values),unlist(perf@x.values))


plot(unlist(perf@alpha.values),unlist(perf@x.values),
     xlab=perf@alpha.name,ylab=perf@x.name,col='red')



lines(unlist(perf@alpha.values),unlist(perf@y.values),
      xlab=perf@alpha.name,ylab=perf@y.name)


## ERR function
## Function to compute the EER through the convex hull

eer <- function(pfa, pmiss, index=NULL) {
  if (is.null(index)) {
    ## first, add a "corner" (1.1,1.1)
    n <- length(pfa)                      # number of points
    pfa <- c(pfa, 1.1)
    miss <- c(pmiss, 1.1)
    index <- chull(pfa, pmiss)               # find convex hull data points
    index <- sort(index[index<=n])          # remove corner
  }
  i <- which(diff(sign(pfa[index]-pmiss[index]))!=0)[1] # in case there are multiple cases...
  if (i==length(index))                   # this should not happen
    return ((pfa[i]+pmiss[i])/2)
  ## intersect convex hull segment with y=x
  ax <- pfa[index][i]
  bx <- pfa[index][i+1]
  ay <- pmiss[index][i]
  by <- pmiss[index][i+1]
  ax + (ax-ay)*(bx-ax) / (ax-ay-bx+by)
}

perf <- performance(pred,"fpr","miss")
EER= eer(unlist(perf@x.values), unlist(perf@y.values), index=NULL)

cat("Equal Error Rate:",EER)


## NOW ANALYZE: Precission, Recall
# ... we will use CARET library
library(caret)

glm.pred=rep("False" ,667)
glm.pred[glm.probs >.5]="True"

precision <- posPredValue(as.factor(glm.pred),
                          final_test_data$Chur, positive="True")
recall <- sensitivity(as.factor(glm.pred),
                      final_test_data$Chur, positive="True")

F1 <- (2 * precision * recall) / (precision + recall)

print(precision)
print(recall)
print(F1)

## SOME POSSIBLE STUDIES

# use only some features...

# TRY LDA and QDA (know their differences!) part of MASS library

library (MASS)
#lda.fit
lda.fit=lda(Churn ~ . ,data=CV_data)

# Plotting group probabilities
windows()
plot(lda.fit)
dev.off()

lda.predict=predict (lda.fit , final_test_data)


# class: contains LDA's predictions 
# posterior: is a matrix whose kth column contains the
# posterior probability that the corresponding
# observation belongs to the kth class 
# x :  contains the linear discriminants

names(lda.predict)


lda.pred=rep("False" ,667)
lda.pred[lda.predict$posterior[,2] >.5]="True"

table(lda.pred ,final_test_data$Churn)

mean(lda.pred==final_test_data$Churn)

### COMPARE ROC curves
lda.predict=predict (lda.fit , final_test_data)

pred_LR <- prediction(glm.probs, final_test_data$Churn)
perf_LR <- performance(pred,"tpr","fpr")

pred_LDA <- prediction(lda.predict$posterior[,2], final_test_data$Churn)
perf_LDA <- performance(pred_LDA,"tpr","fpr")

windows()
plot(perf_LR,col='blue')
plot(perf_LDA,col='red',add=TRUE)

legend('right', c('LR','LDA') , 
       lty=1, col=c('blue', 'red'), bty='n', cex=.75)

dev.off()

# Now COMPARE WITH QDA Quadratic Discriminant Analysis


qda.fit=qda(Churn ~ . ,data=CV_data)
qda.predict=predict (qda.fit , final_test_data)

qda.pred=rep("False" ,667)
qda.pred[qda.predict$posterior[,2] >.5]="True"

table(qda.pred ,final_test_data$Churn)

mean(qda.pred==final_test_data$Churn)

## COMPARE Logistic Regression, LDA and QDA ROC Curves...
# ....and other metrics ?

pred_LR <- prediction(glm.probs, final_test_data$Churn)
perf_LR <- performance(pred,"tpr","fpr")

pred_LDA <- prediction(lda.predict$posterior[,2], final_test_data$Churn)
perf_LDA <- performance(pred_LDA,"tpr","fpr")

pred_QDA <- prediction(qda.predict$posterior[,2], final_test_data$Churn)
perf_QDA <- performance(pred_QDA,"tpr","fpr")

windows()
plot(perf_LR,col='blue')
plot(perf_LDA,col='red',add=TRUE)
plot(perf_QDA,col='green',add=TRUE)
legend('right', c('LR','LDA','QDA') , 
       lty=1, col=c('blue', 'red', 'green'), bty='n', cex=.75)

dev.off()

## Explain WHY QDA is better ?? see plots feature space per class

# EVALUATE USING CROSS-VALIDATION
# ...you should concatenate CV_data and final_test_data but...

# For Logistic Regression
# https://stats.stackexchange.com/questions/77094/cost-function-in-cv-glm-for-a-fitted-logistic-model-when-cutoff-value-of-the-mo

library(boot)

mycost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
glm.fit=glm(Churn ~ . ,data=CV_data,family=binomial)

# Leave-one-out
cv.err =cv.glm(CV_data , cost=mycost , glm.fit)

cat('LOOV Estimated Classification ERROR' ,cv.err$delta)

# K-fold cross-validation k=10
cv.err =cv.glm(CV_data , cost=mycost , glm.fit, K=10)
cat('k-fold K=10 CV Estimated Classification ERROR' ,cv.err$delta)


## CROSS VALIDATION REGULARIZATION
#http://members.cbio.mines-paristech.fr/~jvert/svn/tutorials/practical/linearclassification/linearclassification.R
#https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

library(glmnet)

# alpha = 0 for ridge regression / 1 for lasso
glmmod <- glmnet(as.matrix(CV_data[c(-14)]),
                 Churn, alpha=1, family="binomial")

windows()
plot(glmmod, xvar="lambda")
plot(glmmod)

lambdas <- 10^seq(1, -3, by = -.1)

cv.glmmod <- cv.glmnet(as.matrix(CV_data[c(-14)]),
                              as.factor(Churn),nfolds=10,
                       lambda = lambdas,
                       alpha=1, family="binomial",
                       type.measure="auc")
windows()
plot(cv.glmmod)
dev.off()

# OPTIMAL Coefs and lambda
myCoefs <- coef(cv.glmmod, s="lambda.min");
opt_lambda <- cv.glmmod$lambda.min

# all the fitted models
fit <- cv.glmmod$glmnet.fit
summary(fit)

## Prediction on the TEST data using the model for optimum
# regularization lamda value

lr_reg.predict=predict (fit , s = opt_lambda,
                        newx=as.matrix(final_test_data[c(-14)]),
                        type="response")

pred_lr_reg <- prediction(lr_reg.predict, final_test_data$Churn)
perf_lr_reg <- performance(pred_lr_reg,"tpr","fpr")

windows()
plot(perf_LR,col='blue')
plot(perf_lr_reg,col='black',add=TRUE)
legend('right', c('LR','LR + Regularization') , 
       lty=1, col=c('blue', 'black'), bty='n', cex=.75)

## Finally some examples for
## FEATURE SELECTION using CROSSVALIDATION

library(caret)
# load the library
library(mlbench)
library(e1071)

# prepare training scheme
control <- trainControl(method="repeatedcv", 
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        number=10, repeats=3)
# train the model
model <- train(Churn~., data=CV_data,
               method="qda",
               metric = "ROC",
               preProcess="scale", trControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# define the control using a lda selection function
control <- rfeControl(functions=gamFuncs, method="cv",
                      number=10)
# run the RFE algorithm
results <- rfe(CV_data[,1:13], CV_data[,14], sizes=c(1:13),
               metric="ROC",
               rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
