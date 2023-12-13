#Junchen Yi  Date: 12/11/2023 Class:ALY6010 70330
library(ggplot2)
library(dplyr)
library(janitor)

#Exploring
players <- read.csv("players_22.csv")

#clean names
players <- players %>%
  clean_names()

#Removing some useless columns
players <- players[, -(1:2)]
players <- players[, -(2:3)]
players <- players[, -(7)]
players <- players[, -(9)]
players <- players[, -(10:11)]
players <- players[, -(11:15)]
players <- players[, -(12:14)]
players <- players[, -(14)]
players <- players[, -(16:20)]
players <- players[, -(39:88)]
players <- players[, -(22:38)]

#Change column and make it understandable
names(players)[names(players) == "short_name"] <- "name"
names(players)[names(players) == "wage_eur"] <- "weekly_wage"
names(players)[names(players) == "club_name"] <- "club"

# Remove rows with null value
# Remove Gold Keeper and save it(Because Goalkeepers only have an overall rating, no basic stats)
gk_data <- subset(players, club_position == "GK")
# Removes the row with club_position "GK" from the original data
players <- subset(players, club_position != "GK")
players <- na.omit(players)

#Descriptive Statistics
# Obtain basic descriptive statistics on basic player attributes
sub_players <- players %>%
  select(c(overall, age, value_eur, weekly_wage, height_cm, weight_kg))
summary(sub_players)

# Visualization
#1.Preferred foot: left foot vs. right foot
foot_prefer <- table(players$preferred_foot)
foot_prefer_df <- data.frame(Foot = names(foot_prefer), F_Count = as.vector(foot_prefer))
ggplot(foot_prefer_df, aes(x = Foot, y = F_Count)) +
  geom_bar(aes(x = Foot), stat = "identity", fill = "blue", width = 0.75) +
  geom_text(aes(label = F_Count), vjust = -0.5, color = "black", size = 3)+
  xlab("Player preferred foot") +
  ylab("Number of players of each type") 

# Weak foot analysis
no_week_foot <- players[players$weak_foot == 5, ]
#Select those players overall value >= 85
no_week_foot <- no_week_foot[no_week_foot$overall >= 85, ]
row_count <- nrow(no_week_foot)
row_count #answer = 6

# top 10 players with the highest ability values
top_ten_players <- players %>%
  arrange(desc(overall)) %>%
  slice(1:10)
typeof(top_ten_players$overall)
ggplot(top_ten_players, aes(x = name, y = overall, color = name)) +
  scale_y_continuous(limits = c(0, 95), breaks = seq(0, 95, by = 10))+
  geom_bar(stat = "identity", fill = "blue", width = 0.75) +
  geom_text(aes(label = overall), vjust = -0.5, color = "black", size = 4)+
  xlab("Player Names") +
  ylab("Overall stat")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot
# Relationship between a player's age and overall score
# Set graphics parameters
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
# Create a scatter plot
plot(players$age, players$overall, main="Age VS. Overall Score", 
     xlab="Age", 
     ylab="Overall", 
     pch=19, 
     col="blue")
# Calculate the regression model
model <- lm(overall ~ age, data=players)
# Add a regression line
abline(model, col="red")
#Generate regression analysis table
if (!require(stargazer)) {
  install.packages("stargazer")
  library(stargazer)
}
stargazer(model, type = "text")

#Percentage of clubs to which the fifty highest ranked players belong.
top_50_players <- players %>%
  arrange(desc(overall)) %>%
  slice(1:50)

top_50_category <- top_50_players %>%
  count(club)

sum_of_other <- sum(top_50_category$n[top_50_category$n <= 2])
top_50_category <- top_50_category[top_50_category$n > 2, ]
new_row <- data.frame(
  club = "Others",
  n = sum_of_other
)
top_50_category <- rbind(top_50_category, new_row)
top_50_category <- top_50_category

# Pie chart
pie(top_50_category$n, labels = paste(top_50_category$club, " (", round(top_50_category$n/sum(top_50_category$n)*100, 1), "%)", ")"),
    main = "Proportion for each club(top 50 players)", cex = 0.8)

#Further analysis
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
model_age_po <- lm(potential ~ age, data=players)
# Add a regression line
abline(model, col="red")
#Generate regression analysis table
stargazer(model_age_po, type = "text")

# One-sample t-test
# Calculate the average potential score for the entire dataset
dataset_mean <- mean(players$potential)
# 1000 players were randomly selected
set.seed(123)  # Set up random seeds for repeatability
sample_players <- sample(players$potential, size = 1000)
# t test
t.test(sample_players, mu = dataset_mean)


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


# Clean the global environment
# rm(list = ls())
