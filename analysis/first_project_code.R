library(dplyr)
library(ggplot2)

#Read the Data file using read_csv
police_incidents <- readr::read_csv("~/MATH261A-project/data/Police_Department_Incident_Reports__2018_to_Present_20250922.csv")

#Filter the Motor Vehicle Theft Reports from the data which is not of the year 2025
vehicle_data <- subset(police_incidents, police_incidents$`Incident Category` == "Motor Vehicle Theft" & police_incidents$`Incident Year`!=2025)

#Aggregate the motor vehicle theft reports by years from 2018 to 2024 and plot a scatterplot with year as x-axis and count as y-axis
theft_per_year <- vehicle_data %>% count(vehicle_data$`Incident Year`, name = 'yearly_theft')
plot(theft_per_year$`vehicle_data$\`Incident Year\``, theft_per_year$yearly_theft, col = "blue", xlab = "Years", ylab = "Motor Vehicle Theft Reports", main = "Vehicle Thefts Reported Yearly (2018 - 2024)")

#Extract the incident week from the incident date field and Aggregate the number of motor vehicle theft reports by week
incident_week <- as.Date(vehicle_data$`Incident Date`, format="%Y-%m-%d")
vehicle_data$week <- as.numeric(strftime(incident_week, format="%V"))
theft_per_week_2024 <- subset(vehicle_data, vehicle_data$`Incident Year`== 2024) %>% count(week, name = 'num_theft')
#Plot a scatterplot with week as x-axis and number of vehicle theft reports as y-axis
plot(theft_per_week_2024$week, theft_per_week_2024$num_theft, col = "red",  xlab = "Week", ylab = "Motor Vehicle Thefts Reports", main = "Vehicle Thefts Reported per Week in 2024")

#Fit a Simple Linear Regressino Model on number of motor vehicle theft using week as predictor
lm_fit <- lm(num_theft ~ week, data = theft_per_week_2024)
summary <- summary(lm_fit) #summary of fitted model

#Implement ANOVA on the fitted model
anova <- anova(lm_fit)

#Create a residual vs fitted scatterplot
df_new <- data.frame(fitted = lm_fit$fitted.values,residuals = lm_fit$residuals)
ggplot(df_new, aes(fitted, residuals))+
  geom_point() +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values plot For Number of Vehicle Theft vs Week")


