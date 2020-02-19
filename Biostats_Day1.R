#Biostats
#Day 1
#Kim Daniels
#19_02_2020

#Loading packages
library(tidyverse)

#Loading Dataset
chicks <- as_tibble(ChickWeight)  #loads data as table

head(chicks) #Shows the top 6 observations of the table
tail(chicks, n = 2) #Shows the bottom 2 observations of the table

colnames(chicks) #Shows the column names in the dataset

summary(chicks) #shows summary statistics for the dataset

chicks %>% 
  summarise(length = n()) #Tidy way of finding length
#the same as
length(chicks$weight) #Base R way of finding length


#creating a dataset
dat1 <- c(23, 45, 23, 66, 13)
mean(dat1)

chicks %>% 
  summarise(mean_wt = mean(weight)) #tidy way of finnding mean weight

chicks %>% 
  summarise(mean_wt = round(mean(weight), 1)) #mean weight rounded off to 1

chicks %>% 
  summarise(mean_wt = sum(weight) / n()) #divides mean into its base calculations

quantile(chicks$weight) #Generates the quantiles for the variable 'weight'

dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1, na.rm = TRUE) #tells r to remove NA values when computing the mean

#Everything you can find for summarise function in a pipe
grp_stat <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

plt3 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)") + 
  theme_pubr()
 