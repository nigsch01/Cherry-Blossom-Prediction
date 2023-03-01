library("tidyverse")
library("rnoaa")
library("zoo")
library("lubridate")
set.seed(23)

temp3 <- ghcnd_search(stationid = "SZ000001940",
                      #refresh = TRUE,
                      var = c("tavg"),
                      date_min = "2002-01-01",
                      date_max = "2023-02-27")
temp_lie <- data.frame(tavg = temp3[["tavg"]][["tavg"]], date = temp3[["tavg"]][["date"]])
temp_lie$tavg = (temp_lie$tavg / 10) * (9/5) + 32


bloom_data3 <-read.csv("liestal.csv")
temp23_lie = read.csv("https://www.ncei.noaa.gov/erddap/griddap/nmme_ccsm4_ts_day_r01_by_time_LAT_LON.csv?TS%5B(2023-01-01T12:00:00Z):1:(2023-02-27T12:00:00Z)%5D%5B(47.5331):1:(47.5331)%5D%5B(7.583):1:(7.5831)%5D", skip = 1)
temp23_lie = temp23_lie %>% 
  transmute(date = as.Date(UTC), 
            temp_in_f = (Kelvin - 273.15) * (9/5) + 32)
temp23_lie$date = ymd(temp23_lie$date)

temp_lie = temp_lie %>% 
  filter(year(date) < 2023 & (month(date) == 1 | (month(date) == 2 & day(date) < 28)))
temp23_lie = do.call("rbind", replicate(21, temp23_lie, simplify = F))


temp_lie = temp_lie %>%
  mutate(tavg = ifelse(is.na(tavg), na.approx(tavg), tavg))

diff_lie = data.frame(year = temp_lie[,2], diff_sq = (temp_lie[,1] - temp23_lie[,2])^2)
sum_lie = rowsum(diff_lie$diff_sq,rep(1:21,each=58))
sumdiff_lie = data.frame(year = c(2002:2022), sum_lie, doy = bloom_data3$bloom_doy[c(109:129)])

which.min(sumdiff_lie$sum_lie)
#The 13th row (the year 2014) has had the closest weather to this year (2023) and thus my guess for
#the bloom date in Liestal is the 84th date of the year or March 25th, 2023
