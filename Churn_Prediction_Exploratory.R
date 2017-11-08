# ORANGE Churn prediction


file_train ='C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-80.csv'
CV_data <- read.csv(file_train, header=TRUE, sep=",")

file_test =
  'C:\\MSTC_BD\\NEW_2017_18\\CHURN\\DATA\\churn-bigml-20.csv'
final_test_data <- read.csv(file_test, header=TRUE, sep=",")

head(CV_data)
names(CV_data)
fix(CV_data)

summary(CV_data)

# Number states and number user per state
List_States=unique(CV_data$State)

List_States2=levels(CV_data$State)

barplot(summary(CV_data$State))

# Analyze number of churns per State
windows()
TT=tapply(CV_data$State, CV_data$Churn, summary)

# Look for a better way for plotting...
par(mfrow=c(3,1))
barplot(TT$True,col=c("red"))
barplot(TT$False,col=c("darkblue"))
barplot(TT$True/(TT$True+TT$False),col=c("green"))
dev.off()



# Scatter plots
windows()
pairs(CV_data)
dev.off()

## you can see that minutes and charge
# are lineary correlated so remove them... (see below)
#'Total.day.charge',
#'Total.eve.charge',
#'Total.night.charge',
#'Total.intl.charge'

# data.table for SET
library(data.table)
set(CV_data, j = c('State',
                   'Area.code',
                   'Total.day.charge',
                   'Total.eve.charge',
                   'Total.night.charge',
                   'Total.intl.charge') , value = NULL)

# Analyze Correlation Matrix: Categorical??
# install corrplot 
library(corrplot)
# for original CV_data : M <- cor(CV_data[c(-1,-4,-5,-20)])
# after removing factors
M <- cor(CV_data[c(-2,-3,-14)])

windows()
#corrplot(M, method="number")
corrplot(M, method="circle")

dev.off()

# Do something bettwer that correlations for classification?

windows()

boxplot(CV_data$Total.night.minutes ~ CV_data$Churn)
title('Total.night.minutes')
dev.off()

library(ggplot2)
ggplot(CV_data, aes(Total.night.minutes, color=Churn)) +
  geom_histogram(position="identity", binwidth=3, aes(y=..density.., fill=Churn),  alpha=0.5) +
  geom_density()


kruskal.test(Total.night.minutes ~ Churn, data = CV_data)

P_values=apply(CV_data[,-14], 2, function(x) kruskal.test(x,CV_data[,14])$p.value)

windows()
barplot(P_values[P_values<=.00001])
dev.off()

# plot scatter between two features in
# different colors per class
windows()
attach(CV_data)
plot(Total.day.minutes, Customer.service.calls,
     col=c("blue","red")[Churn])
dev.off()

# Using ggplot2
qplot(Total.day.calls, Customer.service.calls,
      colour = Churn, shape = Churn, 
      data = CV_data)

#dplyr: A Grammar of Data Manipulation
#A fast, consistent tool for working with data frame
# like objects, both in memory and out of memory.

library(dplyr)
group_by(CV_data, Churn) %>%
  summarise(
    count = n(),
    mean = mean(Customer.service.calls, na.rm = TRUE),
    sd = sd(Customer.service.calls, na.rm = TRUE),
    median = median(Customer.service.calls, na.rm = TRUE),
    IQR = IQR(Customer.service.calls, na.rm = TRUE)
  )


