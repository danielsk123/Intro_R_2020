#Biostats Homework
#Kim Daniels
#24_02_2020

##Create Script of Functions
library() #Loads packages
read_delim()#Reads in comma delimited files
head() #generates a tibble of first 6 rows
tail() #generates a tibble of last 6 rows
tail() #shows the bottom 12 rows
glimpse() #generates an overview of the dataset
view() #opens the data for viewing
names() #shows column names
select() %>% # Select specific columns first
slice() #selects only data between these row numbers
filter() # Filter out only what information is needed
nrow() # Count the number of rows
read_csv()#Reads in csv files
mutate()
group_by() #Groups variables of the same kind
summarise() #Generates summary statistics

ggplot() + #make a graph 
  geom_point() + #make graph a scatter plot
  geom_line() + #links scatter plot with lines
  geom_smooth(method = "lm") + #adds line of best fit
  facet_wrap(ncol = 2)+ # Creates faceted figure (2 columns)
  labs() #adds axis labels

A <- datasets::B #Loads built in dataset B and names it A
load() #Loads data stored in Rdata format

gather(DEA, KZNSB, SAWS, key = "src", value = "temp") #adds a column called temp and groups DEA, KZNSB, SAWS to create source column
spread(key = var, value = val) #spreading column var to depth and type
separate(col = index, into = c("site", "src"), sep = "/ ") #acting on the column index and naming them site and source. / and spaces are the separator # deletes initial index column


##Explore the dataset (ChickWeight) with different functions not used in class
 
  #Loading packages
library(tidyverse)

  #Loading Dataset
chicks <- as_tibble(ChickWeight)  #loads data as table

  #Viewing Dataset
chicks
view(chicks)
glimpse(chicks)
head(chicks)
tail(chicks) 
names(chicks) # lists the variables in chicks
str(chicks) # lists the structure of chicks (type of data, number of levels, first few values)
levels(chicks$Diet) # lists the levels of factor 'diet' in chicks
dim(chicks) # dimensions of chicks (variable number and row number)
class(chicks) #returns the class of the dataset (dataframe, table, etc)


#Skewness task (3.2.3)
library(e1071)
skewness(faithful$eruptions)
  #Mean of eruptions is less than its median and the data is therefore left-skewed.

#Exercise 1 (3.6.1)
summary(chicks$weight)

grp_stat_weight <- chicks %>%
 summarise(min = min(weight),
            qrt1 = quantile(weight, p = 0.25),
            med = median(weight),
            mean = mean(weight, na.rm = TRUE),
            qrt3 = median(weight, p = 0.75),
            max = max(weight))
           

