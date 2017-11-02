file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

summary(CV_data$State)

windows()
barplot(summary(CV_data$State))

windows()
pairs(CV_data)
dev.off()

# data.table for SET
library(data.table)
set(CV_data, j = c('State',
                   'Area.code',
                   'Total.day.charge',
                   'Total.eve.charge',
                   'Total.night.charge',
                   'Total.intl.charge') , value = NULL)

windows()
plot(CV_data$Churn,CV_data$Customer.service.calls)

library(ggplot2)
ggplot(CV_data, aes(Total.night.minutes, color=Churn)) +
  geom_histogram(position="identity", binwidth=3, aes(y=..density.., fill=Churn),  alpha=0.5) +
  geom_density()


kruskal.test(Total.night.minutes ~ Churn, data = CV_data)

P_values=apply(CV_data[,-20], 2, function(x) kruskal.test(x,CV_data[,20])$p.value)

windows()
barplot(P_values[P_values<=.001])
dev.off()
               


