library(readxl)

OSA=read_excel(
  "C:/MSTC_BD/R/WORKING/DATASETS/APNEA/OSA_DB_UPM.xlsx",na="-1")
fix(OSA)
dim(OSA)

OSA=na.omit(OSA)
dim(OSA)
names(OSA)

## Describe Database

# First define Gender as a factor!

OSA$Gender = factor(OSA$Gender)
summary(OSA)

# See relations between variables

pairs(OSA)

## PLOT Correlation Matrix

# FIRST
# install corrplot 
library(corrplot)


# back to as.numeric for including it..

OSA_C=OSA

OSA_C$Gender = as.numeric(OSA_C$Gender)
M <- cor(OSA_C)
corrplot(M, method="number")
corrplot(M, method="circle")


attach(OSA)


lm.fit=lm(IAH~Weight+Height+Cervical+Age)

summary(lm.fit)

OSA_male=subset(OSA, Gender==0)

# Another way
# OSA_male = OSA[OSA$Gender == 0, ]

names(OSA_male)
attach(OSA_male)

lm_male.fit=lm(IAH~Height+Weight+Cervical+Age)

summary(lm_male.fit)

## Predict two particular cases
newdata = data.frame(Height=c(185,180),
                     Weight=c(100,80),
                     Cervical=c(42,39),Age=c(58,46))

predict(lm_male.fit, newdata) 


## Discuss about the ASSUMPTIONS of Linear Regression
plot(predict(lm_male.fit), residuals(lm_male.fit))

hist(sqrt(IAH))

lm_male2.fit=lm(sqrt(IAH)~Height+Weight+Cervical+Age)

summary(lm_male2.fit)

plot(predict(lm_male2.fit), residuals(lm_male2.fit))
plot(predict(lm_male2.fit), rstudent(lm_male2.fit))

OSA_female=subset(OSA, Gender==1)

names(OSA_female)
attach(OSA_female)

lm_female.fit=lm(IAH~Weight+Height+Cervical+Age)

summary(lm_female.fit)





