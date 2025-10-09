library(dplyr)
library(ggplot2)
police_incidents <- readr::read_csv("~/MATH261A-project/data/Police_Department_Incident_Reports__2018_to_Present_20250922.csv")

vehicle_data <- subset(police_incidents, police_incidents$`Incident Category` == "Motor Vehicle Theft" & police_incidents$`Incident Year`!=2025)
#head(vehicle_data)

incident_week <- as.Date(vehicle_data$`Incident Date`, format="%Y-%m-%d")
vehicle_data$week <- as.numeric(strftime(incident_week, format="%V"))
theft_per_week_2024 <- subset(vehicle_data, vehicle_data$`Incident Year`== 2024) %>% count(week, name = 'num_theft')
theft_per_year <- vehicle_data %>% count(vehicle_data$`Incident Year`, name = 'yearly_theft')
plot(theft_per_year$`vehicle_data$\`Incident Year\``, theft_per_year$yearly_theft, col = "blue", xlab = "Years", ylab = "Motor Vehicle Theft Reports", main = "Vehicle Thefts Reported Yearly (2018 - 2024)")
plot(theft_per_week_2024$week, theft_per_week_2024$num_theft, col = "red",  xlab = "Week", ylab = "Motor Vehicle Thefts Reports", main = "Vehicle Thefts Reported per Week in 2024")

lm_fit <- lm(num_theft ~ week, data = theft_per_week_2024)
summary <- summary(lm_fit)

anova <- anova(lm_fit)

df_new <- data.frame(fitted = lm_fit$fitted.values,residuals = lm_fit$residuals)

ggplot(df_new, aes(fitted, residuals))+
  geom_point() +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values plot For Number of Vehicle Theft vs Week")


