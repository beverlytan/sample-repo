# R Script for Github tutorial
# Investigating difference in soil pH between habitat types
# Beverly Tan (s1550222@sms.ed.ac.uk)

# Loading packages and data ---- 

library(tidyverse)

soils_raw <- read_csv("data/soils-raw.csv")

# Preparing data for analysis ---- 

soils <- soils_raw %>%
  select(site_num, sample_num, habitat, soil_pH) %>%
  group_by(site_num, habitat) %>%
  summarise(average = mean(soil_pH))

write.csv(soils, file = "int-pdt/soils-cleaned.csv")

# Creating a boxplot ---- 

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

ggsave(boxplot, file = "final-pdt/boxplot.png")

# Conducting anova ---- 

soils_lm <- lm(average ~ habitat, data = soils)

anova(soils_lm)        


