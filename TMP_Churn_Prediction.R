file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

summary(CV_data$State)

windows()
barplot(summary(CV_data$State))


