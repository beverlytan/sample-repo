# R Script for Github tutorial
# Investigating difference in soil pH between habitat types
# Beverly Tan (s1550222@sms.ed.ac.uk)

# Loading packages and data ---- 

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating boxplot

# If you do not have these packages installed, install them with the following code: 
# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(readr)

# Load raw data from "data" folder  
soils_raw <- read_csv("data/soils-raw.csv")

# Preparing data for analysis ---- 

# Cleaning data and obtaining necessary information
soils <- soils_raw %>%
  select(site_num, sample_num, habitat, soil_pH) %>%
  group_by(site_num, habitat) %>%
  summarise(average = mean(soil_pH))

# Saving cleaned dataframe as a csv file 
write.csv(soils, file = "int-pdt/soils-cleaned.csv")

# Creating a boxplot ---- 

# Using ggplot to create our boxplot 
(boxplot <- ggplot(soils, aes(habitat, average)) + 
   geom_boxplot(aes(fill = habitat)) + 
   scale_fill_manual(values = c("#b3e0ff", "#ffe6cc")) + 
   scale_x_discrete(name = "\nHabitat type", 
                    labels = c('Forest','Grassland')) + 
   scale_y_continuous(limits = c(2, 5), 
                      name = "Soil pH\n") + 
   theme_bw() +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),             
         panel.grid = element_blank(),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),            
         legend.position = "none"))    

# Using ggsave to save our boxplot as a png file
ggsave(boxplot, file = "final-pdt/boxplot.png")