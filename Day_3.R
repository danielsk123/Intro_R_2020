#Day 3
#ggplot: Mapping on Day 3
#Kim Daniels
#30_01_2020

#Loading packages
library(tidyverse)
library(boot) #Install boot first to access today's dataset. #This line activates boot

#Plotting with boot
# Load data
urine <- boot::urine #reading in a built in dataset. assign a name so it appears in environment
view(urine)

# Create a quick scatterplot
ggplot(data = urine, aes(x = osmo, y = ph)) + #x/y= needs to match the column variables
  geom_point(aes(colour = cond))+ #cond is a variable. r will create a scale using cond
  labs(x="Osmoregulation", y= "pH")

scale_colour_gradientn(colours = c("#A5A94D", "#6FB16F", "#45B19B",
                                   "#59A9BE", "#9699C4", "#CA86AD"))  #Type in own colour series from http://tristen.ca/hcl-picker/#/hlc/6/0.95/D07463/8FA551



### Mapping in R

#Loading packages
library(tidyverse)
library(ggpubr)

#load data (data is stored in Rdata format)
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
#load("data/MUR.RData")
load("data/MUR_low_res.RData")

# Custom colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

#Outline of the SA coast (same as chickweight scatter plot code)
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()

#Land Mask
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + #Groups the dots to form a land mask
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) # Colours can be changed

#Province borders
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders creates a path of code corresponding to province borders

#Force lat.long extent: fit image to the plot size 
#(removed the top section of mask not needed)
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #expand zooms in on the map

#Rename MUR data
sst<-MUR_low_res

#Adding SST to map
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + #The ocean temperatures, data is being out into SST data column called bins. each bin is a box
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #Adds the map mask
  geom_path(data = sa_provinces, aes(group = group)) + #adds provinces
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #establishes limit for map

  
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#Creating the final map
final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + #name assignment
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #group function is the same as group_by function. group is also a column name
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) + #adds the tiles around the coast
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map #ahve to run the name to view the map



##Chapter 10
# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

# Load Africa map
load("data/africa_map.RData")

#access the default maps
ggplot() +
  borders() + # The global shape file
  coord_equal()+# Equal sizing for lon/lat 
  coord_equal(xlim = c(-75, -25), ylim = c(-60, 15), expand = 0) #Cutting out part of South America

sa_1 <- ggplot() + #Cutting out South Africa
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1 


sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", #Adding text to map. /n means new line
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30, 
           colour = "pink") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 45, 
           colour = "springgreen")
sa_2

#to add a north arrow
sa_3 <- sa_2 +
 # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           #ERROR dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           # transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3

#insetting maps
# Load Africa map
load("data/africa_map.RData")

africa_map

sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4

