#an assessment fo t-tests
#Kim Daniels
#06/03/2020
#Biostats

library(tidyverse)

#Looking at two samples

#Random normal data
set.seed(666) #Random number generation
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)), #asks for heteroscadistisity
                    sample = c(rep("A", 1000), rep("B", 1000))) #creates two sets of ssamples, a and b


nrow(r_dat)
length(r_dat$sample)


# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")

#Testing for Normal Distribution
shapiro.test(r_dat$dat)

a <- r_dat %>%
  filter(sample == "A") 

b<- r_dat %>%
  filter(sample=="B")

shapiro.test(a$dat)
shapiro.test(b$dat)

#OR

r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#Testing homoscedasticity
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

#two sample two sided t test
#test the null hypotheissi that there is no difference in the means of the samples belonging to groups a and B
#so reject HO when p<=0.05

t.test(dat ~ sample, data = r_dat, var.equal = TRUE) #does data vary by sample

# One sample two sided t-test
# create a single sample of random normal data

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5), #sets teh requirements for the data generated
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

# compare random data against a population mean of 20 (is the mean of our sample different from the pop mean)
t.test(r_one$dat, mu = 20) #when mu is 10, there is a significant difference and when mu is 30 there is a difference. Accepts H0 at 20. Closer to the mean of the sample means higher likelihood of acceptance. two tailed

#one sided.(we're only interested if its less than(alternative="less))
t.test(r_one$dat, mu = 30, alternative = "less") #Our sample is less than 30


