library(ggplot2)
library(dplyr)

# 1. Load Data
diamonds_data = read.csv("diamonds.csv")

set.seed(42)
dia_sample = diamonds_data[sample(nrow(diamonds_data), 3000), ]

# 2. Analytical Plotting
# a. Histogram of Diamond Price
ggplot(dia_sample, aes(x = price)) +
  geom_histogram(bins = 25, fill = "seagreen", color = "black") +
  labs(title = "Histogram of Diamond Price", x = "Price ($)", y = "Count") +
  theme_minimal()
ggsave("../practice_plots/diamonds_plot1_hist_price.png", width = 6.5, height = 4)

# b. Boxplot of Diamond Price Across Color Grades
ggplot(dia_sample, aes(x = color, y = price, fill = color)) +
  geom_boxplot() +
  labs(title = "Boxplot of Diamond Price Across Color Grades", x = "Color Grade (D to J)", y = "Price ($)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/diamonds_plot2_box_color.png", width = 6.5, height = 4)

# c. Scatter Plot of Carat vs Price with Linear Regression Line
ggplot(dia_sample, aes(x = carat, y = price)) +
  geom_point(alpha = 0.4, color = "dodgerblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Carat vs Price with Fitted Trendline", x = "Carat", y = "Price ($)") +
  theme_minimal()
ggsave("../practice_plots/diamonds_plot3_scatter_carat_price.png", width = 6.5, height = 4)

# d. Barplot of Cut Quality Distribution
ggplot(dia_sample, aes(x = cut, fill = cut)) +
  geom_bar() +
  labs(title = "Barplot of Cut Quality Distribution", x = "Cut Grade", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/diamonds_plot4_bar_cut.png", width = 6.5, height = 4)

# End of Diamonds Analytical Plotting Solution
