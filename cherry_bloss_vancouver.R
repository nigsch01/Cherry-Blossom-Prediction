library("tidyverse")
library("rnoaa")
library("zoo")
library("lubridate")
set.seed(23)

temp4 <- ghcnd_search(stationid = "CA001106200",
                     #refresh = TRUE,
                     var = c("tmin", "tmax"),
                     date_min = "1950-01-01",
                     date_max = "2023-02-27")

temp_van <-
  left_join(temp4$tmax, temp4$tmin, by = c("id", "date")) %>%
  mutate(temp = ((tmax + tmin) / 20) * (9/5) + 32) %>%
  select(date, temp) %>% 
  mutate(temp = ifelse(is.na(temp), na.approx(temp), temp))
temp_van = temp_van[670:nrow(temp_van),]
temp23_van = temp_van %>% 
  filter(year(date) == 2023)
temp_van = temp_van %>% 
  filter(year(date) < 2022 & (month(date) == 1 | (month(date) == 2 & day(date) < 28)))

bloom_data4 <-
  data.frame(year = c(2000:2005, 2008:2021),
         doy  = rpois(20, 95))

temp23_van = do.call("rbind", replicate(20, temp23_van, simplify = F))

diff_van = data.frame(year = temp_van[,1], diff_sq = (temp_van[,2] - temp23_van[,2])^2)
sum_van = rowsum(diff_van$temp,rep(1:20,each=58))
sumdiff_van = data.frame(year = c(2000:2005, 2008:2021), sum_van, doy = bloom_data4$doy)

x = as.numeric(which.min(sumdiff_van$sum_van))
doy_pred_van_10 = data.frame(year = 2023:2032, 
                             doy = sumdiff_van$doy[x]) #data frame of 10 year prediction

# The lowest sum of differences squared is located in the third row, which corresponds with the year
#2003. The day of year for the bloom, created from a random poisson distribution with lambda = 95,
#is 93. This day of year is April 2, 2023. 

?na.approx


