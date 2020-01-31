#Day 4
#Last Homework Assignment
#Kim Daniels and Aamirah Botha
#Due: 5 February 2020


# Load libraries
library(tidyverse)
library(ggpubr)
library(readr)
library(scales)
library(ggsn)
library(maps)

bathywide_tidy <- bathy_wide %>%
  gather(long, key = "lat")
