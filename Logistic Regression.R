#Download Affairs dataframe from AER package
#install.packages("AER")
library(AER)
data(Affairs, package="AER")
summary(Affairs)
str(Affairs)
head(Affairs)
table(Affairs$affairs)
#Create ynaffair dichotomous factor
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
str(Affairs)

Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No", "Yes"))
#Table on number of Yes and No
table(Affairs$ynaffair)

#Logistic Regression
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation + rating,
                data=Affairs, family=binomial())
summary(fit.full)

# According to the p-values: 
#gender, children, education, occupation are not significant contributions. since p > .05
#Second fitted equation without those variables.
fit.reduced <- glm(ynaffair ~ age+yearsmarried+religiousness+rating,
                   data=Affairs, family=binomial())
summary(fit.reduced)
#Each regression coeff. is significant (p < .05) 
#The two models are nested, fit.reduced is a subset of fit.full. Can use anova() function to compare them
anova(fit.reduced, fit.full, test="Chisq")

#Look at regression coefficients:
coef(fit.reduced)
#exponentiate log(odds) to better understand and interpret them
exp(coef(fit.reduced))
#Left off at pg. 321, sec. 13.2.1