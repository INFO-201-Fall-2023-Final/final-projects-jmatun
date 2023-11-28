library(dplyr)
library(readr)

NY_Collisions <- read.csv("MotorVehicleCollisionsCrashes.csv")
NY_Weather <- read.csv("NYWeather.csv")


NY_Collisions_Date <- NY_Collisions %>% mutate(CRASH.DATE = as.Date(CRASH.DATE, format = "%m/%d/%Y")) %>% 
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
  mutate(season = ifelse((format(CRASH.DATE, "%m") == "03"), "Spring", ifelse((format(CRASH.DATE, "%m") == "06"), "Summer", ifelse((format(CRASH.DATE, "%m") == "09"), "Fall", "Winter"))),) %>%
  group_by(BOROUGH, season) %>%
  summarise(
    total_crashes = n(), 
    total_injuries = sum(NUMBER.OF.PERSONS.INJURED),
    total_fatalities = sum(NUMBER.OF.PERSONS.KILLED),
    dangerous_conditions_pct = round(sum(roadConditions == "dangerous") / total_crashes, digits = 2),
    average_danger_rating = round(mean(dangerRating), digits = 2)
  )

#View(summary_df)

write.csv(combined, file = "combinedFrame.csv")
