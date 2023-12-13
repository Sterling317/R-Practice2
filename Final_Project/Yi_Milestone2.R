#Junchen Yi  Date: 02/12/2023 Class:ALY6010 70330

install.packages("dplyr")
library(dplyr)
library(ggplot2)


# 1.What is the relationship between potential score and age?
# Basic descriptive statistics
summary(players$potential)
summary(players$age)
# Scatter plot of age vs potential
# Set graphics parameters
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
plot(players$age, players$potential, main = "Age vs Potential", 
     xlab = "Age",
     ylab = "Potential",
     pch = 19,
     col = "blue")
# Calculate the regression model
model <- lm(potential ~ age, data=players)
# Add a regression line
abline(model, col="red")

# One-sample t-test
# Calculate the average potential score for the entire dataset
dataset_mean <- mean(players$potential)
# 1000 players were randomly selected
set.seed(123)  # Set up random seeds for repeatability
sample_players <- sample(players$potential, size = 1000)
# t test
t.test(sample_players, mu = dataset_mean)


write.csv(players, "players.csv")

# 2.What's the relationship between age and salary? Do players salary get higher
#   while they get older?
summary(players$age)
summary(players$value_eur)
#Scatter plot of salary and age
ggplot(players, aes(x = age, y = value_eur)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +  # polynomial regression line
  labs(title = "Relationship Between Age and Salary with Polynomial Regression Line",
       x = "Age", 
       y = "Salary") +
  theme_minimal()

# Linear regression analysis
salary_age_model <- lm(value_eur ~ age, data = players)
summary(salary_age_model)


# 3.Do height (height_cm) affect a player's speed (pace)?
# Visualization
ggplot(players, aes(x = height_cm, y = pace, color = pace)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", linewidth = 0.5) +
  labs(title = "Relationship Between Height and Pace", x = "Height (cm)", y = "Pace")
# Calculating average height
average_height <- mean(players$height_cm)
#Dividing players into two groups base on their height
short_players <- players$pace[players$height_cm < average_height]
tall_players <- players$pace[players$height_cm >= average_height]
# Two sample t-test
t.test(short_players, tall_players, alternative = "two.sided", var.equal = FALSE)
