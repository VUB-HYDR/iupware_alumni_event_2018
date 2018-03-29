
### DATA HANDLING ###
# IMPORTING #

wells = read.csv('./data/wells.csv')
str(wells)
head(wells)
summary(wells)


### CLEANING
library(magrittr)
library(tidyr)
library(dplyr)

wells$q = na_if(wells$q, 999) %>% multiply_by(-1)
wells = wells %>% mutate(dh = replace(dh, dh < 0, NA))
head(wells)

### TRANSFORMING
# average specific capacity & corresponding T values in Limburg?

 wells  %>%
  mutate(spc = q/dh) %>%  mutate(tran = 0.76*spc^1.08)  %>% 
  filter(province != 'Limburg') %>% 
  summarise(aspc = mean(spc, na.rm=T), atran = mean(tran, na.rm=T), count=n())

## PLOTTING
# plot drawdown in function of discharge
plot(wells$q, wells$dh)

library(ggplot2)
  ggplot(wells, aes(x = q, y = dh, color = stratigraphy)) + geom_point() +
    theme_classic()

# plot the number of pumps with a rate higher than 1000 cbm/s
  wells %>% filter(q > 1000) %>% 
    ggplot(aes(x=province, fill=stratigraphy)) + geom_bar()
  

