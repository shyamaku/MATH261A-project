library(dplyr)
library(ggplot2)
police_incidents <- readr::read_csv('C:/Users/shyam/OneDrive/Documents/MATH261A-project/data/Police_Department_Incident_Reports__2018_to_Present_20250922.csv')
vehicle_data <- subset(police_incidents, police_incidents$`Incident Category` == "Motor Vehicle Theft")
#head(vehicle_data)

incident_week <- as.Date(vehicle_data$`Incident Date`, format="%Y-%m-%d")
vehicle_data$week <- as.numeric(strftime(incident_week, format="%V"))
theft_per_week_2024 <- subset(vehicle_data, vehicle_data$`Incident Year`== 2024) %>% count(week, name = 'num_theft')
#head(theft_per_week_2024)
plot(theft_per_week_2024$week, theft_per_week_2024$num_theft, col = "red",  xlab = "Week", ylab = "Number of Thefts", main = "Vehicle Thefts per Week in 2024")

lm_fit <- lm(num_theft ~ week, data = theft_per_week_2024)
summary(lm_fit)

anova(lm_fit)

df_new <- data.frame(fitted = lm_fit$fitted.values,residuals = lm_fit$residuals)

ggplot(df_new, aes(fitted, residuals))+
  geom_point() +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values plot For Number of Vehicle Theft vs Week"
  )