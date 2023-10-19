rm(list=ls())
install.packages("ivreg")
library(ivreg)
data("SchoolingReturns")
attach(SchoolingReturns)
help("SchoolingReturns")

#instrumental variable
model1<-lm(log(wage)~education+experience+I(experience^2)+ethnicity+smsa+south)
summary(model1)

#IV: nearness to college on eduction on wage
#cause nearness not correlated with wage

#first stage
model2<-lm(education~nearcollege+experience+I(experience^2)+ethnicity+smsa+south)
summary(model2)

#reduced form - outcome on IV
model3<-lm(log(wage)~nearcollege+experience+I(experience^2)+ethnicity+smsa+south)
summary(model3)

#result of reduced/first stage coeffs

ivform<-model3$coefficients[2]/model2$coefficients[2]
ivform
#gives us education on wages - unbiased
#13.2% impact of education on wages, model1 showed 7%

#2SLS
model4<-ivreg(log(wage)~ethnicity+smsa+south+I(experience^2)|education|nearcollege)
#exogenous variable: constant variables, related ones
#Indogenous variable: treatment variable/education/main independent one
summary(model4)
