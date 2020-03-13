#Biostats
#Exercise 2
#Kim Daniels
#13_03_2020


#Loading pacakages
library(tidyverse)
library(ggpubr)
library(readr)
library(scales)
library(ggsn)
library(maps)
library(ggplot2)
library(lubridate)
library(readxl)
library(RColorBrewer)

#1.) The SACTNmonthly_v4.0.RData

SACTNmonthly_tidy <- SACTNmonthly_v4.0 %>%
  mutate(year= year(date)) %>%
  group_by(site, year) %>%
  filter(src == "KZNSB") %>% 
  summarise(mean_temp=mean(temp))

ggplot(data= SACTNmonthly_tidy,aes(x=year,y=mean_temp)) +
  geom_line()+
  labs(x="Year", y= "Temperature(°C)",title="KZNSB: series of annual means")+
  facet_wrap(~site, ncol=5)

#2.) The data.laminaria.csv data

laminaria <- read_excel("laminaria.xlsx")

lam_1<- laminaria %>% 
  filter(site %in% c("A-Frame", "Baboon Rock", "Batsata Rock", "Betty's Bay",
                     "Bordjiestif North", "Buffels", "Buffels South", 
                     "Miller's Point","Roman Rock"))

ggplot(data= lam_1, aes(x=blade_length,y=blade_weight)) +
  geom_point(aes(colour=site), size=1)+
  geom_line(aes(colour=site), size=3)+
  labs(x="Blade length (cm)", y= "Blade mass (kg)",title="A crazy graph of some data for False Bay sites")+
  facet_wrap(~site, ncol=3)+
  scale_colour_brewer(palette="Accent")


ggplot(data= lam_1, aes(x=blade_length,y=blade_weight)) +
  geom_point(aes(colour=site), size=1)+
  geom_line(aes(colour=site), size=3)+
  labs(x="Blade length (cm)", y= "Blade mass (kg)",title="A crazy graph of some data for False Bay sites")+
  facet_wrap(~site, ncol=3)+
  scale_colour_brewer(palette="Set1")

ggarrange(Graph, Graph_Fixed,
                  ncol= 2,
                  labels= c("A","B"),
                  common.legend= FALSE,
                  legend = "none")

#3.) The ToothGrowth data

teeth<- datasets::ToothGrowth

teeth_2 <- teeth%>% 
  group_by(supp, dose) %>%
  summarise(mean_length = mean(len),
            sd_length = sd(len))

ggplot(teeth_2, aes(x = dose, y = mean_length, fill = supp)) +
  geom_col(aes(fill = supp), position = "dodge", colour = "black") + 
  geom_errorbar(aes(ymin = mean_length - sd_length,
                    ymax = mean_length + sd_length), 
                position = "dodge", size=0.5) +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)", 
       title= "Dosage of supplements in relation to tooth growth")
