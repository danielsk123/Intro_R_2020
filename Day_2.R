#Day 2
#Summary Statistics
#Kim Daniels
#29_01_2020


#Loading Tidyverse Package
library(tidyverse)

#Importing data (data is semicolon-delimited)
library(readr)
laminaria <- read_delim("data/laminaria.csv", #tells R the directory for the data
                        ";", escape_double = FALSE, trim_ws = TRUE)

head(laminaria) #To make sure data is loaded correctly (shows first six rows)

laminaria %>% # Choose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length and adds it as a new column

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% # adding this causes R to find the mean and sd for each site
summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
          sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths

laminaria %>%
  group_by(site) %>% # adding this causes R to calculate the following for each site
  summarise(mean_tot_length= mean(total_length), # Create a summary of the mean of the total lengths
            median_tot_length= median(total_length), # Create a summary of the median of the total lengths
            sd_tot_length= sd(total_length),# Create a summary of the standard deviation of the total lengths
            var_tot_length=var(total_length))# Create a summary of the variance of the total lengths


#Plotting with ggplot

ggplot(data=laminaria, aes(x = stipe_mass, y = stipe_length)) + #aes is a subfunctionto specify axes
  geom_point(shape = 19, colour = "purple", fill = "pink") + #geo_point (point graph) geom_line(line graph)...when writing script witin a plot, add plus before moving to the next line
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") #labs=labels (need not have underscores)


#Plotting (chickweight dataset)

ChickWeight <- datasets::ChickWeight #this is the only way to read in a built in dataset. It has to be assigned a name

view(ChickWeight) #lets us view the data

#Generate a plot (point and line)
ggplot(ChickWeight, aes(x = Time, y = weight, colour=Diet)) + #aes is a subfunction to specify axes.Each diet option is in a different colour
  geom_point(shape=13, size= 2,colour="grey") + #creates a scatter plot shape 13, colour grey, size 2
  geom_line(aes(group = Chick)) #links scatter plot with line per chick

#Generate a plot (point and linear model)
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + #Linear Model shows the line of best fit. Diet 3 has the greatest weight gain
  theme_bw() 

#Generate a plot (point and linear model) (3 important variables included)
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + #add 'aes' because size then becomes a function
  geom_smooth(method = "lm", size = 1.2)+
  theme(legend.position = "bottom") # Change the legend position


#Facetting Figures
library(tidyverse)
library(ggpubr)
library(readr)

# Create faceted figure (Each diet will have its own graph)
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets, TWO COLUMNS ncol=2
  labs(x = "Days", y = "Mass (g)")

ChickWeight <- datasets::ChickWeight

ChickLast <- ChickWeight %>% 
  filter(Time == 21) #This will not run, ChickLast not created Error in filter(., Time == 21) : object 'Time' not found


line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1

lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")
lm_1

# Could not create Chicklast, Used Chickweight instead
#Generating a histogram
histogram_1 <- ggplot(data = ChickWeight, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
histogram_1

#generating a boxplot
box_1 <- ggplot(data = ChickWeight, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1

#Gridding Figure
ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend
