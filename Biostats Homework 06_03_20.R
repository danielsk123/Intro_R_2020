#Biostats: Intro R workshop
#Kim Daniels
#24_02_2020

#Exercise 1

# Load some packages ------------------------------------------------------

library(fitdistrplus)
library(logspline)
library(dplyr)
library(tidyverse)

#Child heights (Normal distribution)
heights<-c(32.81,28.26,26.36,29.64,30.61,26.5,28.29,27.99,31.7,32.26,28.73,27.08,28.96,27.88,27.31,30.28,32.75,28.01,29.86,31.55)
child<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)


data_frame_1<-data.frame(child,heights)

  
ggplot(data = data_frame_1, aes(x = child, y = heights)) +
    geom_histogram()

#Birds (T-test)
set.seed(666) #Random number generation
r_dat <- data.frame(dat = c(rnorm(n = 20, mean = 10, sd = 2),
                            rnorm(n = 20, mean = 8, sd = 2)), #asks for heteroscadistisity
                    sample = c(rep("CBD", ), rep("Stellenbosch")))
    


  