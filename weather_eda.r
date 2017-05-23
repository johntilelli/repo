summary(weather)

wdt = data.table(weather)

wdt$rain_ind = ifelse(wdt$precipitation > 0, 1, 0)

event_stats = wdt[, list(max_precip = max(precipitation),
                             min_precip = min(precipitation),
                             count = .N),
                  by = events]

hist(wdt$wind_speed)
hist(wdt$temp)
hist(wdt$humidity)
summary(wdt$wind_speed)

daily_weather = wdt[, list(temp_high = max(temp),
                           temp_low = min(temp),
                           temp_avg = mean(temp),
                           humidity_high = max(humidity),
                           humidity_low = min(humidity),
                           humidity_avg = mean(humidity)), by = DateFormat]

plot(temp_high ~ DateFormat, data = daily_weather)
plot(temp_low ~ DateFormat, data = daily_weather)
plot(temp_avg ~ DateFormat, data = daily_weather)
plot(humidity_high ~ DateFormat, data = daily_weather)
plot(humidity_low ~ DateFormat, data = daily_weather)
plot(humidity_avg ~ DateFormat, data = daily_weather)

boxplot(wdt$temp ~ week, data = wdt)
wdt$week = as.numeric(format(wdt$DateFormat, format = "%W"))
