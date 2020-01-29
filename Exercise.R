<<<<<<< HEAD
#Exercise 4.5
#Day_1
#Kim Daniels
#28_01_2020


#Importing laminaria.csv
library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#Create a new data frame from the laminaria dataset that meets the following criteria: contains only the site column and a new column called total_length_half containing values that are half of the total_length. In this total_length_half column, there are no NAs and all values are less than 100.

New_dataframe<-laminaria %>% 
  mutate(total_length_half=total_length/2) %>% 
  select(site,total_length_half(na.rm=TRUE))

#Find the mean, min, and max blade_length for each site. Also add the number of observations
        
laminaria %>%
  group_by(site,blade_length)%>%
  summarise(mean_blade_length=mean(blade_length),
            min_blade_length=min(blade_length),
            max_blade_length=max(blade_length),n=n())

#What was the heaviest stipe measured in each site?
laminaria %>%
  group_by(site) %>%
  summarise(max_stipe_mass=max(stipe_mass))

#Return the columns site, region, and stipe_length.
laminaria %>%
  select(site, region, stipe_length, stipe_mass)

###
    
            
=======
#Exercise 4.5
#Day_1
#Kim Daniels
#28_01_2020


#Importing laminaria.csv
library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#Create a new data frame from the laminaria dataset that meets the following criteria: contains only the site column and a new column called total_length_half containing values that are half of the total_length. In this total_length_half column, there are no NAs and all values are less than 100.

New_dataframe<-laminaria %>% 
  mutate(total_length_half=total_length/2) %>% 
  select(site,total_length_half(na.rm=TRUE))

#Find the mean, min, and max blade_length for each site. Also add the number of observations
        
laminaria %>%
  group_by(site,blade_length)%>%
  summarise(mean_blade_length=mean(blade_length),
            min_blade_length=min(blade_length),
            max_blade_length=max(blade_length),n=n())

#What was the heaviest stipe measured in each site?
laminaria %>%
  group_by(site) %>%
  summarise(max_stipe_mass=max(stipe_mass))

#Return the columns site, region, and stipe_length.
laminaria %>%
  select(site, region, stipe_length, stipe_mass)

##### Very neat script
# Adding comments after each line of code only helps you and the collaborator understand what you are doing.
# Overall Mark for DAY 1: 8/10
            
>>>>>>> 9d9a7059fb93a65c77117555a8bbb655af04b477
