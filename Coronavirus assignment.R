#Corona virus Assignment
#Kim Daniels and Aamirah Botha
#Due: 11 March 2020

#Installing packages
install.packages("prettymapr")
install.packages("plotly")


#Loading libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(readr)
library(scales)
library(ggsn)
library(maps)
library(ggplot2)
library(maptools)
library(lubridate)
library(plotly)

#Loading data
deaths_and_new_cases_of_hiv <- read_csv("data/deaths-and-new-cases-of-hiv.csv")

Corona_virus_data_deaths <- read_csv("data/time_series_19-covid-Deaths.csv")
Corona_virus_data_recovered <- read_csv("data/time_series_19-covid-Recovered.csv")
Corona_virus_data_confirmed <- read_csv("data/time_series_19-covid-Confirmed.csv")

#Data source: https://github.com/CSSEGISandData/COVID-19
#Data source: https://ourworldindata.org/hiv-aids

#Tidying HIV Data

HIV_2017<- deaths_and_new_cases_of_hiv %>%
  rename(Country= Entity, New_Infections=c(5), Deaths=c(4), Living_with_HIV= c(6)) %>%
  select(-c(2)) %>%
  group_by(Year) %>% 
  summarise(mean_deaths= mean(Deaths), 
            mean_new_infections= mean(New_Infections), 
            mean_Living_with_HIV = mean(Living_with_HIV))

#Tidying Corona virus Data
Corona_virus_recovered<-Corona_virus_data_recovered %>%
  gather(c(5:51), key = "Date", value = "Recovered") %>%
  separate(col = Date, into = c("date", "time"), sep = " ") %>%
  select(-time)
  
Corona_virus_confirmed<-Corona_virus_data_confirmed %>%
  gather(c(5:51), key = "Date", value = "Confirmed")%>%
  separate(col = Date, into = c("date", "time"), sep = " ")%>%
  select(-time)

Corona_virus_deaths<-Corona_virus_data_deaths %>%
  gather(c(5:51), key = "Date", value = "Deaths")%>%
  separate(col = Date, into = c("date", "time"), sep = " ")%>%
  select(-time)

Intermediate_dataset<-left_join(Corona_virus_recovered, Corona_virus_confirmed)
Corona_virus_tidy<-left_join(Intermediate_dataset,Corona_virus_deaths)

# Mapping
Map_1<- ggplot() +
  borders() + # The global shape file
  coord_equal()+# Equal sizing for lon/lat
  labs(title="Spread of COVID-19")+ 
  theme_bw()+
  borders(fill = "white", colour = "black")+
  geom_point(data=Corona_virus_confirmed, aes(x=Long, y=Lat), color="red")+
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           transform = TRUE, model = "WGS84") + # Set appearance
           north(x.min = -150, x.max = -100, y.min = -70, y.max = -60, # Set location of symbol
                 scale = 30, symbol = 16)
Map_1



ggplotly(Map_1)

#Bar graphs HIV

HIV_infections <- ggplot(data = HIV_2017, aes(x = Year, y = mean_new_infections))+
  geom_bar(stat =  "identity", aes(fill=Year))+
  labs(x="Year", y="New Infections")

HIV_infections

HIV_deaths <- ggplot(data = HIV_2017, aes(x = Year, y = mean_deaths))+
  geom_bar(stat =  "identity", aes(fill=Year))+
  theme_bw()+
  labs(x="Year", y="Mean Deaths")

HIV_deaths

HIV_living_with <- ggplot(data = HIV_2017, aes(x = Year, y = mean_Living_with_HIV))+
                       geom_bar(stat =  "identity", aes(fill=Year))+
                        labs(x="Year", y="Mean Living With HIV")+
                        theme_bw()

HIV_living_with

#Facetting
Graphs<- ggarrange(HIV_infections, HIV_deaths, HIV_living_with,
ncol= 2 , nrow= 2,
labels= c("A","B","C"),
common.legend= FALSE)


ggplotly(HIV_living_with)
ggplotly(HIV_deaths)
ggplotly(HIV_infections)

#Bar graphs COVID-19

      Coronavirus_tidy_2<-Corona_virus_tidy %>%
        group_by(`Country/Region`) %>% 
        summarise(mean_recovered= mean(Recovered), 
                  mean_confirmed= mean(Confirmed), 
                  mean_deaths= mean(Deaths))

      ##COVID_graph <- ggplot(data = Coronavirus_tidy_2, aes(x = Country/Region, y = mean_recovered))+
        geom_bar(stat =  "identity", aes(fill=Country))+
        labs(x="Country", y="Recovered")
      
      #Corona_1 <- Corona_virus_confirmed %>% 
        group_by(`Country/Region`) %>% 
        summarise(mean_confirmed = mean(Confirmed))
      
      #Corona_2 <- Corona_virus_recovered %>% 
        group_by(`Country/Region`) %>% 
        summarise(mean_recovered = mean(Recovered))
      
      #Corona_3 <- Corona_virus_deaths %>% 
        group_by(`Country/Region`) %>% 
        summarise(mean_deaths = mean(Deaths))
      
      #Corona_tidy <- left_join(Corona_1, Corona_2)
      Corona_tidy2 <- left_join(Corona_tidy, Corona_3)
      
      Corona_tidy2<- Corona_tidy2 %>% 
        rename( "Country"= "Country/Region")

#Bar graphs
Corona_Confirmed <- ggplot(data = Corona_virus_confirmed, aes(x = Country, y = mean_confirmed))+
  geom_bar(stat =  "identity", aes(fill=mean_confirmed))+
  labs(x="Country", y="Mean confirmed")
Corona_Confirmed

Corona_Deaths <- ggplot(data = Corona_virus_deaths, aes(x = Country, y = mean_deaths))+
  geom_bar(stat =  "identity", aes(fill=mean_deaths))+
  labs(x="Country", y="Mean Deaths")
Corona_Deaths

Corona_Recovered <- ggplot(data = Corona_virus_recovered, aes(x = Country, y = mean_recovered))+
  geom_bar(stat =  "identity", aes(fill=mean_recovered))+
  labs(x="Country", y="Mean recovered")
Corona_Recovered 

ggarrange(Corona_Confirmed, Corona_Deaths, Corona_Recovered, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C"), # Label each figure
          common.legend = FALSE) # Create common legend


