library("tidyverse")
library("rnoaa")
library("zoo")
set.seed(23)

temp <- ghcnd_search(stationid = "USC00186350",
                     #refresh = TRUE,
                     var = c("tmax", "tmin"),
                     date_min = "2000-01-01",
                     date_max = "2023-02-25")

temp_dc <-
  left_join(temp$tmax, temp$tmin, by = c("id", "date")) %>%
  mutate(temp = ((((tmax + tmin) / 2)/10)* (9/5) + 32)) %>%
  select(date, temp)

bloom_data <-read.csv("washingtondc.csv")
temp23 = read.csv("https://www.ncei.noaa.gov/erddap/griddap/nmme_ccsm4_ts_day_r01_by_time_LAT_LON.csv?TS%5B(2023-01-01T12:00:00Z):1:(2023-02-23T12:00:00Z)%5D%5B(39):1:(39)%5D%5B(77):1:(77)%5D", skip = 1)
temp23 = temp23[1:18,] %>% 
  transmute(date = as.Date(UTC), 
            temp_in_f = (Kelvin - 273.15) * (9/5) + 32)

temp23 = do.call("rbind", replicate(22, temp23, simplify = F))
  

temp_dc = temp_dc[c(1:18, 367:384, 732:749, 1097:1114, 1463:1480, 1828:1845, 2193:2210, 2558:2575,
                    2924:2941, 3289:3306, 3654:3671, 4019:4036, 4385:4402, 4750:4767, 5115:5132,
                    5480:5497, 5846:5863, 6211:6228, 6576:6593, 6941:6958, 7307:7324, 7672:7689),]


temp_dc = temp_dc %>%
  mutate(temp = ifelse(is.na(temp), na.approx(temp), temp))

diff = data.frame(temp_dc[,1], (temp_dc[,2] - temp23[,2])^2)

sum = c(sum(diff$temp[1:18]), sum(diff$temp[19:36]),sum(diff$temp[37:54]),sum(diff$temp[55:72]),
        sum(diff$temp[73:90]),sum(diff$temp[91:108]),sum(diff$temp[109:126]),sum(diff$temp[127:144]),
        sum(diff$temp[145:162]),sum(diff$temp[163:180]),sum(diff$temp[181:198]),sum(diff$temp[199:216]),
        sum(diff$temp[217:234]),sum(diff$temp[235:252]),sum(diff$temp[253:270]),sum(diff$temp[271:288]),
        sum(diff$temp[289:306]),sum(diff$temp[307:324]),sum(diff$temp[325:342]),sum(diff$temp[343:360]),
        sum(diff$temp[361:378]),sum(diff$temp[379:396]))

sumdiff = data.frame(year = c(2000, 2001, 2003:2022), sum, doy = bloom_data$bloom_doy[c(80, 81, 83:102)])

which.min(sumdiff$sum)

#Row 11 in sumdiff has the minimum deviation in the 2023 weather, and so the 2023 bloom date
#will be day 88 or 03-29-2023








