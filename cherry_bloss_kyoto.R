library("tidyverse")
library("rnoaa")
library("zoo")
library("lubridate")
set.seed(23)

temp2 <- ghcnd_search(stationid = "JA000047759",
                     #refresh = TRUE,
                     var = c("tavg"),
                     date_min = "1990-01-01",
                     date_max = "2023-02-27")
temp_jap <- data.frame(temp2[["tavg"]][["tavg"]], temp2[["tavg"]][["date"]])
temp_jap$tavg = temp_jap$temp2...tavg......tavg...
temp_jap$date = temp_jap$temp2...tavg......date...
temp_jap = subset(temp_jap, select = -c(temp2...tavg......tavg..., temp2...tavg......date...))
temp_jap$tavg = (temp_jap$tavg / 10) * (9/5) + 32


bloom_data2 <-read.csv("kyoto.csv")
temp23_jap = read.csv("https://www.ncei.noaa.gov/erddap/griddap/nmme_ccsm4_ts_day_r01_by_time_LAT_LON.csv?TS%5B(2023-01-01T12:00:00Z):1:(2023-02-27T12:00:00Z)%5D%5B(35):1:(35.017)%5D%5B(136):1:(135.733)%5D", skip = 1)
temp23_jap = temp23_jap %>% 
  transmute(date = as.Date(UTC), 
            temp_in_f = (Kelvin - 273.15) * (9/5) + 32)
temp23_jap$date = ymd(temp23_jap$date)

temp23_jap = do.call("rbind", replicate(33, temp23_jap, simplify = F))
temp_jap = temp_jap %>% 
  filter(month(date) == 1 | (month(date) == 2 & day(date) < 28))

temp_jap = temp_jap %>%
  mutate(tavg = ifelse(is.na(tavg), na.approx(tavg), tavg))

diff_jap = data.frame(year = temp_jap[,2], diff_sq = (temp_jap[,1] - temp23_jap[,2])^2)
sum_jap = rowsum(diff_jap$diff_sq,rep(1:33,each=58))
sumdiff_jap = data.frame(year = c(1990:2022), sum_jap, doy = bloom_data2$bloom_doy[c(802:834)])

which.min(sumdiff_jap$sum_jap)

#The weather in 2023 so far has been the closest to 2011 as it has the lowest sum of differences
#squared. Thus I will predict that the cherry blossoms will bloom on the same day, day 99, or
# April 9th, 2023


