# Day_1.R
# Data about Laminaria collected along the Cape Peninsula
# Do various data manipulations, analyses and graphs
# Kim Daniels
# 28_01_2020

#Loading tidyverse package
library(tidyverse)

# Run this if 'laminaria.csv` has columns separated by ','
laminaria <- read_delim("data/laminaria.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

#Viewing data 
head(laminaria) #generates a tibble of first 6 rows
tail(laminaria) #generates a tibble of last 6 rows
glimpse(laminaria) #generates an overview of the dataset
view(laminaria) #opens the data for viewing
names(laminaria) #shows column names

#Tidyverse

lam_sub<-laminaria %>% # Tell R which dataframe we are using and saves result as a dataset
  select(site, total_length) # Select only specific columns

# %>%  shift+ctrl+m
# <-  alt+-

lam_slice <- laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78) #selects only data between these row numbers

lam_kom <- laminaria %>%
  filter(site == "Kommetjie") #"filter" removes only information that is needed

laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows






