
### TIME SERIES ANALYSIS ###
library(magrittr)

# Import
nete_ds = read.csv("./data/nete_discharge.csv")
head(nete_ds)

# Clean
library(lubridate)
# nete_ds$Date = as.Date(nete_ds$Date, format = '%d/%m/%Y')
nete_ds$Date = dmy(nete_ds$Date)

library(tidyr)
library(dplyr)
nete_df = gather(nete_ds, key = 'location', value = 'discharge', -Date)
nete_df$discharge = na_if(nete_df$discharge, -999)
head(nete_df)

# Plot
library(ggplot2)
ggplot(nete_df, aes(Date, discharge, color=location)) + geom_line() +
  facet_wrap(~location)

huls = subset(nete_df, location == 'Hulshout')

### Handling dates & times ###
# Time difference
huls$Date[nrow(huls)] - huls$Date[1]

# extract 
wday(huls$Date[5565], label = T)

# Data manipulation
library(tibbletime)
huls = huls %>% as_tbl_time(index = Date)

huls  %>%  filter_time("1991-12-06"~"1998") %>%
  ggplot(aes(Date, discharge)) + geom_area(fill='lightgreen', alpha=0.7)

# summarize statistics
huls %>% collapse_by(period = 'yearly') %>%
  group_by(Date) %>% summarize(max = max(discharge, na.rm = T)) 

# moving average
mov_avg = rollify(mean(na.rm=T), window = 365)

huls %>% mutate(avg = mov_avg(discharge)) %>%
  ggplot(aes(x = Date)) + geom_line(aes(y = discharge), color = 'red') +
  geom_line(aes(y = avg), color = 'blue')








### TIME SERIES ANALYSIS ###

ggplot(huls, aes(Date, discharge)) + geom_line() +
  geom_smooth()

## Extreme value analysis ##
# Maxima in water year (Oct 1-Sep 30) from 1986 to 2010
huls$wy = year(huls$Date + as.numeric(dmy('31-12-1990')-dmy('1-10-1990')))

annmax = huls  %>% filter(wy %in% c(1986:2010)) %>%
 group_by(wy) %>% summarize(max = max(discharge, na.rm=T))

ggplot(annmax, aes(wy, max)) + geom_line() + geom_point()

# fit distribution
library(extremeStat)
anndis = distLfit(annmax$max)
plotLfit(anndis)

# plot return periods
annext = distLextreme(dlf = anndis, RPs = c(2:30))
plotLextreme(annext, log=T)


## Forecasting ##
# set as monthly data
huls_ts = huls %>% select(-location) %>%
  filter_time("1989"~"1997")  %>% collapse_by('monthly') %>%
  group_by(Date) %>% summarize(discharge = mean(discharge))


# needs ts --> less flexible
# use timetk
library(timetk)
library(zoo)

huls_ts = huls_ts %>%
  tk_ts(start = c(1989, 1), end = c(1997, 12), frequency = 12) %>%
  na.approx()

huls_ts
plot(huls_ts)

# decomposition
huls_dcm = stl(huls_ts[,1], s.window = 12)
plot(huls_dcm)

# check for stationarity
library(tseries)
adf.test(huls_ts, alternative='stationary', k=0)

# tidy forecasting with sweep
# ETS model
library(forecast)
huls_ets = ets(huls_ts)
plot(huls_ets)

str(huls_ets)

library(sweep)
sw_tidy(huls_ets)

# cast
huls_fcst = forecast(huls_ets, h = 36)
autoplot(huls_fcst)

str(huls_fcst)
sw_sweep(huls_fcst) %>% tail(10)






# # auto-correlation plots
# acf(huls_ts) # p & q
# pacf(huls_ts) # p & q
# nsdiffs(huls_ts) # i

# fit model using ARIMA
# huls_arm = Arima(huls_ts, order = c(1,0,0), 
#                  seasonal = list(order = c(1,0,0), frequency = 12))
# huls_pred1 = forecast(huls_arm, h=36)
# autoplot(huls_pred1)
