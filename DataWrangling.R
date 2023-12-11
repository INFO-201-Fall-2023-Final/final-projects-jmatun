library(dplyr)
library(readr)
library(stringr)

NY_Collisions <- read.csv("MotorVehicleCollisionsCrashes.csv")
NY_Weather <- read.csv("NYWeather.csv")


NY_Collisions_Date <- NY_Collisions %>% mutate(CRASH.DATE = as.Date(CRASH.DATE, format = "%m/%d/%Y")) %>% mutate(season = ifelse((format(CRASH.DATE, "%m") == "03"), "Spring", ifelse((format(CRASH.DATE, "%m") == "06"), "Summer", ifelse((format(CRASH.DATE, "%m") == "09"), "Fall", "Winter"))),) %>% 
  filter(CRASH.DATE >= as.Date("2023-01-01")) %>% filter(CRASH.DATE <= as.Date("2023-09-19")) %>% filter((format(CRASH.DATE, "%m") == "01") |  (format(CRASH.DATE, "%m") == "03") |  (format(CRASH.DATE, "%m") == "06") |  (format(CRASH.DATE, "%m") == "09")    
)
NY_Weather_Date <- NY_Weather %>% mutate(datetime = as.Date(datetime))


#Merging the columns for both data frames which is Date
combined <- left_join(NY_Collisions_Date, NY_Weather_Date, by = c("CRASH.DATE" = "datetime"))

# Adding the categorical and continuous variables
combined <- combined %>% mutate(roadConditions = ifelse(windspeed >= 56 | temp <= 0 | precip > 0 | snow > 0, "dangerous", "safe")) %>% mutate(dangerRating = (windspeed / 56 * .25) + (.01 / temp + .01 * .25) + (precip / 1 * .25) + (snow / 1 * .25))
#View(combined)

# Creating summary data frame 
summary_df <- combined %>%
  mutate(season = ifelse((format(CRASH.DATE, "%m") == "03"), "Spring", ifelse((format(CRASH.DATE, "%m") == "06"), "Summer ", ifelse((format(CRASH.DATE, "%m") == "09"), "Fall", "Winter"))),) %>%
  group_by(season) %>%
  summarise(
    total_crashes = n(), 
    total_injuries = sum(NUMBER.OF.PERSONS.INJURED),
    total_fatalities = sum(NUMBER.OF.PERSONS.KILLED),
    dangerous_conditions_pct = round(sum(roadConditions == "dangerous") / total_crashes, digits = 2),
    average_danger_rating = round(mean(dangerRating), digits = 2)
  )

#View(summary_df)
#print("done")

library(dplyr)

new_summary <- combined %>%
  group_by(season) %>%
  summarise(
    total_rain = sum(str_detect(preciptype, "rain")),
    total_snow = sum(str_detect(preciptype, "snow")),
    total_other = sum(!str_detect(preciptype, "rain") & !str_detect(preciptype, "snow"))
  ) %>% select(season, total_rain, total_snow, total_other)

weather_summary <- NY_Weather_Date %>%
  mutate(CRASH.DATE = datetime) %>%
  filter((format(CRASH.DATE, "%m") == "01" | format(CRASH.DATE, "%m") == "03" |format(CRASH.DATE, "%m") == "06" | format(CRASH.DATE, "%m") == "09") & (CRASH.DATE >= as.Date("2023-01-01"))) %>%
  mutate(season = ifelse((format(CRASH.DATE, "%m") == "03"), "Spring", ifelse((format(CRASH.DATE, "%m") == "06"), "Summer", ifelse((format(CRASH.DATE, "%m") == "09"), "Fall", "Winter")))) %>%
  mutate(weather = ifelse(str_detect(preciptype, "rain"), "Rain", ifelse(str_detect(preciptype, "snow"), "Snow", "Clear"))) %>%
  group_by(season, weather) %>% 
  summarise(count = n()) %>%
  group_by(season) %>%
  mutate(percentage = count / sum(count) * 100)
 
cause_summary <- combined %>%
  mutate(weather = ifelse(str_detect(preciptype, "rain"), "Rain", ifelse(str_detect(preciptype, "snow"), "Snow", "Clear"))) %>%
  group_by(season, CONTRIBUTING.FACTOR.VEHICLE.1, weather) %>%
  summarise(
    count = n()
  ) 

#View(cause_summary)
#View(summary_df) 
#View(weather_summary)
#write.csv(cause_summary, file = "causeSummary.csv")
#write.csv(new_summary, file = "seasonSummary.csv")
#write.csv(summary_df, file = "summary.csv")
#write.csv(combined, file = "combinedFrame.csv")
