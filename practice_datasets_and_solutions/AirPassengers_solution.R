library(ggplot2)
library(dplyr)

# 1. Load Data
ap = read.csv("AirPassengers.csv")
ap$Date = as.Date(ap$Date)
ap$Month = factor(ap$Month, levels = month.abb)
ap$YearFactor = factor(ap$Year)

# 2. Analytical Plotting
# a. Histogram of Passenger Volume
ggplot(ap, aes(x = Passengers)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Passenger Volume", x = "Passengers (Thousands)", y = "Count") +
  theme_minimal()
ggsave("../practice_plots/airpassengers_plot1_hist.png", width = 6.5, height = 4)

# b. Boxplot of Passenger Volume across Years
ggplot(ap, aes(x = YearFactor, y = Passengers, fill = YearFactor)) +
  geom_boxplot() +
  labs(title = "Boxplot of Passenger Volume Across Years", x = "Year", y = "Passengers (Thousands)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/airpassengers_plot2_box_year.png", width = 6.5, height = 4)

# c. Scatter Plot of Timeline with Linear Trendline
ggplot(ap, aes(x = Date, y = Passengers)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Passenger Volume Over Time with Linear Trendline", x = "Date", y = "Passengers (Thousands)") +
  theme_minimal()
ggsave("../practice_plots/airpassengers_plot3_scatter_trend.png", width = 6.5, height = 4)

# d. Barplot of Average Monthly Passengers across Months
ap %>%
  group_by(Month) %>%
  summarise(Avg_Passengers = mean(Passengers)) %>%
  ggplot(aes(x = Month, y = Avg_Passengers, fill = Month)) +
  geom_col() +
  labs(title = "Barplot of Average Monthly Passenger Traffic", x = "Month", y = "Average Passengers") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/airpassengers_plot4_bar_month.png", width = 6.5, height = 4)

# 3. Model Building: Regression Model for Passenger Volume
set.seed(42)
train_idx = sample(1:nrow(ap), 0.75 * nrow(ap))
train = ap[train_idx, ]
test = ap[-train_idx, ]

model = lm(Passengers ~ Year + Month, data = train)
summary(model)

# Prediction & Metrics
preds = predict(model, newdata = test)
actuals = test$Passengers
rmse = sqrt(mean((actuals - preds)^2))
r2 = 1 - sum((actuals - preds)^2) / sum((actuals - mean(actuals))^2)
print(c(RMSE = rmse, R2 = r2))

# Plot Actual vs Predicted
eval_df = data.frame(Actual = actuals, Predicted = preds)
ggplot(eval_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 2.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Airline Passengers", x = "Actual", y = "Predicted") +
  theme_minimal()
ggsave("../practice_plots/airpassengers_plot5_actual_vs_pred.png", width = 6.5, height = 4)
