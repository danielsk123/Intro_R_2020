#Day 1
#Purpose: demonstrate principle of data analysis
#Kim Daniels

#28_01_2020

library(lubridate)

#read in the data
temps <- read.csv("data/SACTN_data.csv")
temps #to view the dataset in console

#look at the data
head(temps)
tail(temps,12) #shows the bottom 12 rows
glimpse(temps)

#read in the data
temps <- read_csv("data/SACTN_data.csv")  #recognises the date column as date

#Summarise the data (generates table of site means and standard deviation per month)
mean_temps<-temps %>% 
  mutate(mon= month(date, label= TRUE, abbr=TRUE)) %>%
  group_by(site,mon) %>%
  summarise(mean_temp=mean(temp,na.rm=TRUE),
            sd_temp=sd(temp,na.rm=TRUE))
mean_temps

#make a graph 
ggplot(data= mean_temps,aes(x=mon,y=mean_temp)) +
  geom_point(aes(col=site))



