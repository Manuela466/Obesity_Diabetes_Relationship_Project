library(nFactors)
library(lavaan)
library(dplyr)
library(tidyr)
library(psych)
library(corrplot)
library(ggplot2)
library(car)
library(caret)
library(Hmisc)
library(tidyverse)
library(BBmisc)
library(sf)
library(corrplot)
library(gridExtra)


#---------Read in data----------
acs_place_data<-readRDS("acs_places_merged.RDS") %>%
  st_drop_geometry()


#-------Put the variables into a new data set----------
obesity_data <- acs_place_data[, c("OBESITY", 
                                   "DIABETES",
                                   "pct_minority", 
                                   "pct_latin",
                                   "pct_black")]



#--------Quantile the data-----------
quantile(acs_place_data$pct_black, probs = seq(0,1,0.25))
quantile(acs_place_data$pct_latin, probs = seq(0,1,0.25))



#----------------------OBESITY--------------------------------------------
#---------Filter data for high and low black percent-------------
low_black_per_ob <- filter(obesity_data, pct_black <= 0.7)
high_black_per_ob <- filter(obesity_data, pct_black >= 10)


#Graph high and low black percent obesity V.S diabetes
black_low_ob <- ggplot(low_black_per_ob, aes(x = OBESITY, y = DIABETES, color = pct_black)) +
  geom_point() +
  labs(title = "Scatter plot: Obesity vs. Diabetes",
       x = "Obesity",
       y = "Diabetes") +
  scale_color_gradientn(colors = c("hot pink", "blue"))


black_high_ob <- ggplot(high_black_per_ob, aes(x = OBESITY, y = DIABETES, color = pct_black)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot: Obesity vs. Diabetes in Black community",
       x = "Obesity",
       y = "Diabetes") +
  scale_color_gradientn(colors = c("hot pink", "blue"))


#-----------Put graph the same page---------------
grid.arrange(black_low_ob, black_high_ob,
             ncol = 1, nrow = 2)


#---------Filter data for high and low latin percent-------------
low_latin_per_ob <- filter(obesity_data, pct_latin <= 2)
high_latin_per_ob <- filter(obesity_data, pct_latin >= 11)

#Graph high and low latin percent obesity V.S diabetes
latin_low_ob <- ggplot(low_latin_per_ob, aes(x = OBESITY, y = DIABETES, color = pct_latin)) +
  geom_point() +
  labs(title = "Scatter plot: Obesity vs. Diabetes",
       x = "Obesity",
       y = "Diabetes") +
  scale_color_gradientn(colors = c("hot pink", "blue"))


latin_high_ob <- ggplot(high_latin_per_ob, aes(x = OBESITY, y = DIABETES, color = pct_latin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot: Obesity vs. Diabetes in Latin community",
       x = "Obesity",
       y = "Diabetes") +
  scale_color_gradientn(colors = c("hot pink", "blue"))


#-----------Put graph the same page---------------
grid.arrange(latin_low_ob, latin_high_ob,
             ncol = 1, nrow = 2)

#-----------PUT HIGH PERCENTAGE OF BLACK AND LATIN ON SAME PAGE---------------------
grid.arrange(black_high_ob, latin_high_ob,
             ncol = 1, nrow = 2)

#-----------PUT LOW PERCENTAGE OF BLACK AND LATIN ON SAME PAGE---------------------
grid.arrange(latin_low_ob, black_low_ob,
             ncol = 1, nrow = 2)
