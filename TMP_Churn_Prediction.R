file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

# data.table for SET
library(data.table)
set(CV_data, j = c('State',
                   'Area.code',
                   'Total.day.charge',
                   'Total.eve.charge',
                   'Total.night.charge',
                   'Total.intl.charge') , value = NULL)


# Logistic Regression
glm.fit=glm(Churn ~ . ,data=CV_data,family=binomial)
summary(glm.fit)

# Apply Logist Regression Model on Training Data
# type ="response" gives probabilities
glm.probs=predict(glm.fit,type="response")
