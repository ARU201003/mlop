library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)

# 1. Load Data
penguins = read.csv("penguins.csv")
penguins$species = factor(penguins$species)
penguins$island = factor(penguins$island)
penguins$sex = factor(penguins$sex)

# 2. Analytical Plotting
# a. Histogram of Penguin Body Mass
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 20, fill = "coral", color = "black") +
  labs(title = "Histogram of Penguin Body Mass", x = "Body Mass (g)", y = "Count") +
  theme_minimal()
ggsave("../practice_plots/penguins_plot1_hist_mass.png", width = 6.5, height = 4)

# b. Boxplot of Flipper Length Across Species
ggplot(penguins, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Flipper Length Across Species", x = "Species", y = "Flipper Length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("../practice_plots/penguins_plot2_box_flipper.png", width = 6.5, height = 4)

# c. Scatter Plot of Bill Length vs Body Mass with Trendline
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bill Length vs Body Mass by Species", x = "Bill Length (mm)", y = "Body Mass (g)", color = "Species") +
  theme_minimal()
ggsave("../practice_plots/penguins_plot3_scatter_bill_mass.png", width = 6.5, height = 4)

# d. Barplot of Sex Distribution Across Species
ggplot(penguins, aes(x = species, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Sex Distribution Across Species", x = "Species", y = "Count", fill = "Sex") +
  theme_minimal()
ggsave("../practice_plots/penguins_plot4_bar_sex.png", width = 6.5, height = 4)

# 3. Model Building: Decision Tree Classification
set.seed(42)
train_idx = sample(1:nrow(penguins), 0.75 * nrow(penguins))
train = penguins[train_idx, ]
test = penguins[-train_idx, ]

model = rpart(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = train, method = "class")

# Plot Decision Tree
png("../practice_plots/penguins_plot5_tree.png", width = 750, height = 500, res = 110)
rpart.plot(model, type = 2, extra = 104, main = "Decision Tree for Penguin Species Classification")
dev.off()

# Prediction & Metrics
preds = predict(model, test, type = "class")
conf_matrix = table(Actual = test$species, Predicted = preds)
accuracy = mean(test$species == preds)

print(conf_matrix)
print(c(Accuracy = accuracy))
