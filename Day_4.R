#Day 4
#Tidy Data
#Kim Daniels
#31_01_2020


# Load libraries
library(tidyverse)
library(ggpubr)
library(readr)


load("data/SACTN_mangled.RData")

view(SACTN1)

#Plotting the tidy data SACTN1

ggplot(data = SACTN1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) + #groups together column contents of site and source. 0 means theres'll be no space between the two. separator can be set with "sep"
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()

#Tidying the data SACTN2 source split
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #adds a column called temp and groups three to create source column

#Tidying the data SACTN3 spreading column var to depth and type
SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val) 

#Tidying the data SACTN4 Separate index column (site and source)
SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") #actng on the column index and naming them. / and spaces are the separator
  # deletes initial index column

#separating date into its components
SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date), #What is the component of the code that deleted teh indexcolumn before but kept the date colum here?
         year = lubridate::year(date))


