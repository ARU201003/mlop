library(ggplot2)
library(dplyr)

# 1. Load Data
swiss_data = read.csv("swiss.csv")
swiss_data$Catholic_Majority = factor(ifelse(swiss_data$Catholic > 50, "Catholic", "Protestant"))

# 2. Analytical Plotting
# a. Histogram of Fertility Rates
ggplot(swiss_data, aes(x = Fertility)) +
  geom_histogram(bins = 10, fill = "lightsalmon", color = "black") +
  labs(title = "Histogram of Swiss Fertility Rates", x = "Fertility Index", y = "Count") +
  theme_minimal()
ggsave("../practice_plots/swiss_plot1_hist_fertility.png", width = 6.5, height = 4)

# b. Boxplot of Examination Scores Across Religious Majority
ggplot(swiss_data, aes(x = Catholic_Majority, y = Examination, fill = Catholic_Majority)) +
  geom_boxplot() +
  labs(title = "Boxplot of Examination Scores Across Religion", x = "Majority Religion", y = "Examination Score (%)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/swiss_plot2_box_exam.png", width = 6.5, height = 4)

# c. Scatter Plot of Agriculture vs Fertility with Regression Line
ggplot(swiss_data, aes(x = Agriculture, y = Fertility)) +
  geom_point(color = "darkgreen", size = 2.5) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed") +
  labs(title = "Agriculture vs Fertility with Regression Line", x = "Agriculture (%)", y = "Fertility Index") +
  theme_minimal()
ggsave("../practice_plots/swiss_plot3_scatter_agri.png", width = 6.5, height = 4)

# d. Scatter Plot of Education vs Fertility with Trendline
ggplot(swiss_data, aes(x = Education, y = Fertility)) +
  geom_point(color = "darkblue", size = 2.5) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Education vs Fertility with Trendline", x = "Education (%)", y = "Fertility Index") +
  theme_minimal()
ggsave("../practice_plots/swiss_plot4_scatter_edu.png", width = 6.5, height = 4)

# 3. Model Building: Multiple Linear Regression
set.seed(42)
train_idx = sample(1:nrow(swiss_data), 0.75 * nrow(swiss_data))
train = swiss_data[train_idx, ]
test = swiss_data[-train_idx, ]

model = lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = train)
summary(model)

# Prediction & Metrics
preds = predict(model, newdata = test)
actuals = test$Fertility
rmse = sqrt(mean((actuals - preds)^2))
mae = mean(abs(actuals - preds))
r2 = 1 - sum((actuals - preds)^2) / sum((actuals - mean(actuals))^2)
print(c(RMSE = rmse, MAE = mae, R2 = r2))

# Plot Actual vs Predicted
eval_df = data.frame(Actual = actuals, Predicted = preds)
ggplot(eval_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Swiss Fertility Rates", x = "Actual Fertility", y = "Predicted Fertility") +
  theme_minimal()
ggsave("../practice_plots/swiss_plot5_actual_vs_pred.png", width = 6.5, height = 4)
