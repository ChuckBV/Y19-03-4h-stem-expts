#============================================================================ 
#  Y19-03-4hstem-sci-exp1-vol.R
#
# Process and anlyze data from experiments examining the effect of the
# volume of water on the probability of success in the water bottle flip 
#
# PARTS
# 1. List libaries used and import data (Line 14)
# 2. Plot success vs. volume for each individual member (Line 26)
# 3. Box plot of the distribution of successful flips by volume (Line 54)
# 4. Plot success vs. vol for all members compbined
#
#============================================================================

#-- 1. List libaries used and import data -----------------------------------
library(tibble)
library(dplyr)
library(ggplot2)
library(DescTools)

### Import data
flips <- as_tibble(read.csv("./Y19-03-4h-stem-expts/stem-water-bottle-flips-2019-03-11.csv"))

### Reduce data to essentials
flips$Prop <- flips$Successes/flips$Trials

#-- 2. Plot success vs. volume for each individual member -------------------

### Plot proportion sucessful vs. volume in ml, Note that volume of a full 
### bottle is 480 ml. A quadratic line (parabola) is fitted separately for
### the data of each member
pPlot_succes <- ggplot(flips, aes(x = Ml, y = Prop, colour = Flipper)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = "red") +
  theme_bw() +
  facet_wrap(~ Flipper,  ncol = 3) +
  lims(x = c(50,450), y = c(0,1)) +

  xlab("Volume of water in the bottle") +
  ylab("Successful flips as a \nproportion of trials") +

  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18),
        legend.position = "none")

pPlot_succes

ggsave(filename = "./Y19-03-4h-stem-expts/Vol_fig1_individuals.jpg", plot = pPlot_succes, device = "jpg",  
       dpi = 300, width = 8, height = 5.83, units = "in")


#-- 3. Plot a Box plot of the distribution of successful flips by volume ----

### Create a variable with a sequential row number for each row in the current
### data set (6 persons x 10 volumes/person = 60 records)
counts <- flips %>%
  select(1,2,4) %>%
  mutate(RowID = as.integer(rownames(flips)))
counts
# A tibble: 60 x 4
#   Flipper     Ml Successes RowID
#   <fct>    <int>     <int> <int>
# 1 Isabella    80         2     1
# 2 Isabella   120         4     2
# 3 Isabella   160         7     3
# 4 Isabella   200         9     4
# 5 Isabella   240         5     5
# 6 Isabella   280        10     6
# 7 Isabella   320         4     7
# 8 Isabella   360         3     8
# 9 Isabella   400         3     9
# 10 Isabella   440         0    10
# ... with 50 more rows

sum(flips$Successes)
# [1] 244

### To Create a Box Plot, we need one row for each successful flip. The 
### following code creates n copies of RowID above, were n = Successes
### in the Counts data set above

RowID <- rep(counts$RowID, counts$Successes)  # Creates a vector
RowID
# [1]  1  1  2  2  2  2  3  3  3  3  3  3  3 

mydf <- data.frame(RowID) 
# Converts from vector (line of numbers) to a data frame (column of numbers) 
head(mydf)
# 1     1
# 2     1
# 3     2
# 4     2
# 5     2
# 6     2

counts2 <- full_join(counts, mydf) # merge Counts and mydf
head(counts2)
# A tibble: 6 x 4
#   Flipper     Ml Successes RowID
#   <fct>    <int>     <int> <int>
# 1 Isabella    80         2     1
# 2 Isabella    80         2     1
# 3 Isabella   120         4     2
# 4 Isabella   120         4     2
# 5 Isabella   120         4     2
# 6 Isabella   120         4     2

pBoxplot <- ggplot(counts2, aes(x = Flipper, y = Ml)) +
  geom_boxplot()  +
  theme_bw() +
  
  xlab("Person flipping the bottle") +
  ylab("Volume in bottle (ml)") +
  
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18))

pBoxplot

ggsave(filename = "./Y19-03-4h-stem-expts/Vol_fig2_median_success.jpg", plot = pBoxplot, device = "jpg",  
       dpi = 300, width = 8, height = 5.83, units = "in")

Desc(Ml ~ Flipper, data = counts2, plotit = FALSE)
# Summary: 
#   n pairs: 3e+02, valid: 3e+02 (100.0%), missings: 0 (0.0%), groups: 6
# 
# 
#               ArlesArias  AthenaPennebaker    CharlotteBurks             David     IsabellaArias             Logan
# mean           2.462e+02         2.655e+02         2.491e+02         2.475e+02         2.408e+02         2.156e+02
# median         2.200e+02         2.800e+02         2.400e+02         2.400e+02         2.400e+02         2.000e+02
# sd             9.646e+01         1.163e+02         9.001e+01         8.877e+01         8.867e+01         9.764e+01
# IQR            1.100e+02         1.600e+02         1.600e+02         1.600e+02         1.200e+02         9.000e+01
# n                  3e+01             1e+01             4e+01             1e+02             5e+01             4e+01
# np               10.078%            4.264%           13.566%           39.535%           18.605%           13.953%
# NAs                    0                 0                 0                 0                 0                 0
# 0s                     0                 0                 0                 0                 0                 0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 5.4906, df = 5, p-value = 0.359


#-- 4. Plot success vs. vol for all members compbined ----


pPlot_succes2 <- ggplot(flips, aes(x = Ml, y = Prop, colour = Flipper)) +
  geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = "red") +
  theme_bw() +
  #lims(x = c(0,500), y = c(0,1)) +
  
  
  xlab("Volume of water in the bottle") +
  ylab("Successful flips as a \nproportion of trials") +
  
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12))

pPlot_succes2

ggsave(filename = "./Y19-03-4h-stem-expts/Vol_fig3_scatter_all.jpg", 
       plot = pPlot_succes2, device = "jpg", dpi = 300, width = 8, 
       height = 5.83, units = "in")

### Quadratic model

flips$Ml <- as.numeric(flips$Ml)

flips$Ml2 <- flips$Ml**2 

quad <- lm(Successes ~ Ml + Ml2 + Flipper, data = flips)
summary(quad)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -4.604e+00  2.237e+00  -2.058   0.0446 *  
#   Ml                7.651e-02  1.774e-02   4.313 7.20e-05 ***
#   Ml2              -1.570e-04  3.348e-05  -4.690 2.01e-05 ***
#   FlipperAthena    -2.200e+00  1.348e+00  -1.631   0.1088    
#   FlipperCharlotte  6.000e-01  1.348e+00   0.445   0.6582    
#   FlipperDavid      7.500e+00  1.348e+00   5.562 9.38e-07 ***
#   FlipperIsabella   2.100e+00  1.348e+00   1.557   0.1255    
#   FlipperLogan      8.000e-01  1.348e+00   0.593   0.5556    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

