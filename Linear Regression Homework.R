#Biostatistics
#Simple Linear Regression Homework
#Kim Daniels
#30-06-2020


library(tidyverse)
library(readr)

lungs <- read_delim("LungCapData.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE) #Reads in tab separated file)
#CHAPTER 8 EXERCISE ONE

mod<- lm(LungCap ~ Age, data= lungs) #Lungcapacity as a function of age ie to what extent does Age predict lung capacity
summary(mod)

#The estimate of the intercept is alpha in the equation. it is equal to 1.1468 with a standard error or 0.18353. The units are lung capacity in pints.
#The estimate for beta is 0.54485 with a standard error of 0.01416. The unit is lung capacity per age class.

#CHAPTER 8 EXERCISE TWO
#The slope of the linear model fitted to the residuals would equal zero

#The intercept of a linear model fitted to the residuals would equal zero

#The null hypothesis will state that in a plot of residuals, intercept equals zero
#The null hypothesis will state that in a plot of residuals gradient equals zero
#We accept the null hypotheses because the residuals are randomly distributed around the horizontal axis. 
#All the assumptions are also met:

library(ggfortify)
autoplot(mod, colour= "salmon", shape=1, 
         size= 0.2, ncol= 2, which= c(1:2))

autoplot(mod, colour= "salmon", shape=1, 
         size= 0.2, ncol= 2, which= c(3:5))

#The overall model fit is significant (p-value: < 2.2e-16)

#Why?: the points in the residual plot are randomly dispersed around the horizontal axis so when fitted with a linear regression model, the model would  fall on the horizontal axis (y=0), having a gradient of zero. The intercept would also be zero (since it would lying on the line y=0.)

#Plot of the fitted line added to a scatterplot of residuals
ggplot(data=lungs, aes(x= Age, y=mod$residuals)) + 
geom_point(aes(y=mod$residuals), shape= 1, colour= "red3") +
  labs(x="Age",y= "Residual")+
  stat_smooth(method=lm, se=TRUE, colour= "blue3", size= 0.2)
