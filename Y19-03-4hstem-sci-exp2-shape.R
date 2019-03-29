#============================================================================ 
#  Y19-03-4hstem-sci-exp2-shape.R
#
# Process and anlyze data from experiments examining the effect of the
# shape of the bottle on the probability of success in the water bottle flip
#
# PARTS
# 1. List libaries used and import data (line 12)
# 2. Vertical bar chart success vs volume for Arrowhead 8 oz bottle (line 27)
# 3. Scatter plot of success by trial order and bottle size (line 50)
#
#============================================================================ 

#-- 1. List libaries used and import data -----------------------------------

library(tibble)
library(DescTools)
library(tidyr)
library(dplyr)
library(ggplot2)

### Import success vs voume data for Arrowhead
VolEffcts <- as_tibble(read.csv("./Y19-03-4h-stem-expts/stem-water-bottle-flips-2019-03-23.csv"))

### Import the data comparing success rate of flips between the two bottles
Comparison <- as_tibble(read.csv("./Y19-03-4h-stem-expts/stem-water-bottle-flips-2019-03-25.csv"))


#-- 2. Vertical bar chart success vs volume for Arrowhead 8 oz bottle -------

pProp <- ggplot(VolEffcts, aes(x = Volume, y = Percent)) +
  geom_col() +
  theme_bw() +
  scale_x_continuous(breaks=c(35,70,105,140,175,210,245)) +
  scale_y_continuous(breaks=c(0,25,50,75,100), limits = c(0,100)) +

  xlab("Volume of water in the bottle") +
  ylab("Successful flips as a \nproportion of trials") +
  
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18),
        legend.position = "none")

pProp

ggsave(filename = "./Y19-03-4h-stem-expts/shape_fig1_vol_arrowhead.jpg", 
       plot = pProp, device = "jpg",  dpi = 300, width = 8, height = 5.83, units = "in")

#-- 3. Scatter plot of success by trial order and bottle size ---------------

### Look at top of the flip data set
head(Comparison)
# A tibble: 6 x 4
#   Member    Time   Bottle Success
#   <fct>     <fct>  <fct>    <int>
# 1 Charlotte First  8 oz         8
# 2 Charlotte First  16 oz       11
# 3 Charlotte Second 8 oz        17
# 4 Charlotte Second 16 oz       12
# 5 Athena    First  8 oz         3
# 6 Athena    First  16 oz        2

### Order of Time and Bottle will be important for plotting, so examine
unique(Comparison$Time) # Gives what we want, see below
# [1] First  Second
# Levels: First Second

unique(Comparison$Bottle) # Order is inconvenient, see below
# [1] 8 oz  16 oz
# Levels: 16 oz 8 oz

### Change order of factors in bottle
 #  from https://rpubs.com/sediaz/reorder_levels
 # iris$Species <- factor(iris$Species,levels(iris$Species)[c(2,3,1)])
Comparison$Bottle <- factor(Comparison$Bottle,levels(Comparison$Bottle)[c(2,1)])

unique(Comparison$Bottle)  # That fixed it, see below
# [1] 8 oz  16 oz
# Levels: 8 oz 16 oz

pScatter <- ggplot(Comparison, aes(x = Time, y = Success)) +
  geom_jitter(height = 0, width = 0.2, shape = 21, size = 3) +
  facet_wrap("Bottle", ncol = 2) +
  scale_y_continuous(limits = c(0,20)) +
  theme_bw() +
  
  
  xlab("First and second round of flips for each bottle size") +
  ylab("Successful flips out \nof 20 trials") +
  
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18),
        legend.position = "none")

pScatter

ggsave(filename = "./Y19-03-4h-stem-expts/shape_fig2_scatter.jpg", 
       plot = pScatter, device = "jpg", dpi = 300, width = 8, height = 5.83, 
       units = "in")

