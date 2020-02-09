#Tidier Data Assignment
#Kim Daniels
#Due: 10 February 2020

#SECTION 1
#Loading packages
library(tidyverse)
library(boot) #Install boot first to access today's dataset. #This line activates boot

#Plotting with boot
# Load data
BOD <- boot::BOD
data(BOD)

view(BOD)

head(BOD)

#C: BOD is tidy : each row is an observation with two values (time and demand)

data("BJsales")
head(BJsales)  #Not Tidy

data("EuStockMarkets")
head(EuStockMarkets) #Tidy


data("DNase")
head(DNase) #Not Tidy

data("Formaldehyde")
head(Formaldehyde) #Tidy


data("Orange")
head(Orange) #Tidy

data("UCBAdmissions")
head(UCBAdmissions) #Not Tidy

#####

#SECTION 2
#Installing package 'dslabs'
install.packages("dslabs")

#loading libraries
library(dplyr)
library(dslabs)
data(murders)

#Viewing Data
murders
view(murders)
glimpse(murders)
head(murders)
tail(murders) 
names(murders) # lists the variables in murders
str(murders) # lists the structure of murders
levels(murders$region) # lists the levels of factor 'region' in murders
dim(murders) # dimensions of murders
class(murders) # class of murders

#Write a paragraph describing the 'murders' dataset

#'Murders' is a dataframe with 51 obseravtions and 5 variables. 'The variables are 'state', 'abb', 'region','population' and 'total'. Variables are of classes character, character, factor, double-precision floating point number (hold numeric values with decimal points) and double-precision floating point number respectively. The variable 'region' has four levels namely Northeast, South, North Central and West. 

murders <- mutate(murders, population_in_millions = population / 10^6)

#Using the select function, write a code that only shows the states and population size.
select(murders,state,population)

#Using this, remove Florida from the dataset.
murders %>% 
  filter(state != "Florida")

#Create a new data frame called no_south that removes states from the South region. How many states are in this category?
no_south <- filter(murders, region != "South")

apply(no_south, 2, as.factor)

levels(no_south$state)

#Using this create a new data frame only showing the data for New York and Texas
selected_sites <- c("New York", "Texas")

new_data_frame<- murders %>% 
  filter(state %in% selected_sites)

#Calculate the population size of the South and West regionally
murders%>%
  group_by(region) %>%
  summarise(region_population_size= sum(population)) %>%
  filter(region!= "Northeast") %>%
  filter(region!= "North Central")

#Create a new data frame with only the population size of the Northeast region
population_size_northeast<- murders%>%
  group_by(region) %>%
  filter(region== "Northeast") %>%
  select(population)

#Create two plots of your choice and explain visible trends
library(ggplot2)

Fig_1 <- ggplot(data = murders, aes(x = population, y = total))+
  geom_point(aes(colour=region))+
  geom_line(aes(group=region, colour=region))+
  labs(x="Population Size", y= "Total Murders", title="Total Murders per Region")


south_region_population<- murders%>%
  group_by(region) %>%
  filter(region!= "Northeast") %>%
  filter(region!= "North Central")%>%
  filter(region!="West")
south_region_population

Fig_2 <- ggplot(data = south_region_population, aes(x = population, y = state)) + 
  geom_point(aes(colour=total))+
  labs(x="Population Size", y= "State", title="Total Murders per State in the South Region", key= "Total Murders")+
  scale_colour_distiller(palette= "Spectral")

#Figure 1: Total Murders per Region shows that as population size per region increases, total murders shows an increase. This is especially true for higher population sizes such as that of the West Region. At low population sizes, total murders show more variation. 
#Figure 2: Total Murders per State in the South Region, shows that as population size increases, the prevalence of murders generally shows an increase. Florida has the largest population size in the region and the most murders. This trend does not hold true for lower poulation sizes.


#Compare with population size of the South with the population size of the West
pop_size_s_w<- murders%>%
  group_by(region) %>%
  summarise(region_population_size= sum(population)) %>%
  filter(region!= "Northeast") %>%
  filter(region!= "North Central")

histogram <- ggplot(data = pop_size_s_w, aes(x= region_population_size))+
  geom_histogram (aes(fill= region), binwidth=100)+
  labs(x = "Region", y = "Population Size")
  
histogram

#Create a new data frame where the total>20 but <100 and to exclude the value 120
new_data_frame_2<- murders %>%
  filter(total>20) %>%
  filter(total<100) %>%
  filter(total!=120)

#Create an object, containing from 10th to 24th row and 26th row.
new_data_frame_3<-murders %>%
  slice(10:26)

new_data_frame_3[-c(25),]

#Use as_tibble to convert the murders data table into a tibble and save it in an object called murders_tibble

murders_tibble<- as_tibble(murders)

#Use the group_by function to convert murders into a tibble that is grouped by region.
a<- murders%>%
  group_by(region) %>%
  as.tibble()

#Write tidyverse code that is equivalent to this code:
murders%>%
  group_by(region) %>%
  as.dataframe()

#SECTION 3
library(dplyr)
library(dslabs)
data(heights)

#Write a paragraph describing the heights dataset
#The "heights" dataset contains the sexes and heights of 1050 students (1050 observations, 2 variables). The variable 'sex' has two levels namely 'male' and 'female'. It is a categorical variable. 'Height' is a continuous variable. 

#Explore the datasets using the various exploring functions. 
heights
view(heights)
glimpse(heights)
head(heights)
tail(heights) 
names(heights) 
str(heights) 
levels(heights$sex) 
dim(heights)
class(heights) 

#Determine the average and standard deviation for males and female. Then calculate the median, minimum and maximum values.
heights %>%
  group_by(sex)%>%
  summarise(mean_heights_per_sex=mean(height),
            sd_heights_per_sex=sd(height)) 

heights %>%
  summarise(median_heights=median(height),
            min_heights=min(height),
            max_heights=max(height))

#SECTION 4
#Create two vectors x and y

x<- c( 1, 6, 21, 19 , NA, 73, NA)
y<- c(NA, NA, 3, NA, 13, 24, NA)

#a)
sum(is.na(x))
sum(is.na(y))

#b)
sum(is.na(x))
sum(is.na(y))

#SECTION 5
Seasonal_data <-
  data.frame(year = c(2015, 2016, 2017, 2018),
             winter = c(41, 39, 47, 40),
             spring = c(41, 46, 57, 45),
             summer = c(75, 52, 85, 66),
             Autumn = c(57, 66, 52, 56))

#Using the data above, design an hypothesis.
#We believe that temperatures for summer will be higher than temperatures for winter across all four years. We will test this by creating graphs and looking for observable trends.

#...then create two plots and write a paragraph discussing your findings

library(ggplot2)

Annual_ave_summer <- ggplot(data = Seasonal_data, aes(x = year,y= summer))+
  geom_point(aes(colour="#EC8740"))+
  geom_smooth(se = FALSE, aes(colour="#EC8740"))+
  labs(x="Years", y= "Summer", title="Average Annual Temperature for Summer over 4 years")+
  theme(legend.position= "none")

Annual_ave_winter <- ggplot(data = Seasonal_data, aes(x = year,y= winter))+
  geom_point(aes(colour="blue"))+
  geom_smooth(se = FALSE, aes(colour="blue"))+
  labs(x="Years", y= "Winter", title="Average Annual Temperature for Winter over 4 years")+
  theme(legend.position= "none")

#...write a paragraph discussing your findings.
#The hypothesis holds true for summer and winter temperatures from 2015 to 2018. Both seasons display the same trend over the four year period: With 2016 being the coldest year for both seasons. The warmest year for both seasons is 2017. The years 2015 and 2018 display intermediate values for average seasonal temperature in summer and winter. 

cats_data <- tibble (cats = c("A", "B", "C"), 
                   position = c("1-2-3", "3-1-2", "2-3-1"), 
                   minutes = c(3, 3,3),
                   seconds = c(12, 44, 15))
cats_data

#Using the seperate() function split the position column into new three columns.
cats_data_tidy<- cats_data %>%
  separate(col = position, into = c("first_place", "second_place","third_place"), sep = "-")

#Unite the minutes and seconds column into its own column. The new column name will be total_time.
cats_data_tidier<- cats_data_tidy%>%
  unite(minutes, seconds, col = "total_time", sep = ":")

#SECTION 6
#Find, select and use a datasets of your choice.

data("USArrests")
USArrests

#Apply functions to the dataset and at each line of code describe what you are doing.
USA_crime<-USArrests %>%
  gather(Murder,Assault,Rape, key=Crime, value= Crime_Stats) #Gathering crime types (murder, rape, assault) into one column (Crime) and their stats (Crime_Stats) into another

USA_crime_spread<- USA_crime %>%
  spread(key =Crime, value= Crime_Stats) #Attempting to separate Murder, Assault and Rape into three separate columns along with their stats.

Rape_sep<- USArrests %>% 
  separate(col = Rape, into = c("Rape Number", "Decimal"), sep=".") #acting on the column rape and creating two columns with number and decimal of rape numbers. Separator is a full stop ".". It also deletes initial column

USA_crime_and_USA_Arrests <- left_join(USA_crime, USArrests) #Joins two dtaframes USA_crime and USArrests. second dataset attaches to the left of the first.

USArrests%>%   #Arranges the data frame with urban population size in descending order
  arrange(desc(UrbanPop))

USArrests%>%
  select(Rape, Murder, Assault) #Creates a dataframe containing only the columns Murder, Rape and Assault

Grouped_Orange<- Orange %>%
  group_by(Tree) #Creates a dataframe where the data are grouped by Tree

USArrests %>%
mutate(quarter_urban_pop=UrbanPop/4) #Creates a new column named 'quarter_urban_pop' containing values that are quarter of the UrbanPop value
