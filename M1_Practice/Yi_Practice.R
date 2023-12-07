#Junchen Yi  Date: 05/11/2023 Class:ALY6010 70330
install.packages("dplyr")
install.packages("janitor")
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyverse)

olympics_data <- read.csv("dataset_olympics.csv")
#Filter those athletes who didn't get Medal
olympics_data <- filter(olympics_data, Medal != "")

#Filter out rows with no NA value in all columns
olympics_data <- olympics_data %>%
  filter(if_all(everything(), ~ !is.na(.)))

#Clean column names and converts those to lower case
olympics_data <- clean_names(olympics_data)

#Remove columns named id and noc
olympics_data <- select(olympics_data, -id, -noc)

#Converts the name of column "team" to "country"
names(olympics_data)[names(olympics_data) == "team"] <- "country"

#Count the number of athletes in each age group
age_groups <- table(olympics_data$age)
View(age_groups)

#Classification by gender and type of medal
medal_table <- table(olympics_data$sex, olympics_data$medal)
flat_medal_table <- ftable(medal_table)
View(flat_medal_table)

#Show the relationship between sport, gender and medal type
medals_sport_gender <- table(olympics_data$sport, olympics_data$sex, olympics_data$medal)
flat_MSG <- ftable(medals_sport_gender)
View(flat_MSG)

# Select 2016 Summer Olympics data
olympic_data_2016_summer <- subset(olympics_data, year == 2016 & season == "Summer")
# Create a simplified cross-tabulation for gender and medal types
cross_tab_sex_medal <- xtabs(~ sex + medal, data = olympic_data_2016_summer)
View(cross_tab_sex_medal)

# Create a histogram showing the number of medals won by each gender
medal_df <- as.data.frame(flat_medal_table)
ggplot(medal_df, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Freq), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) + 
  labs(title = "Medal Distribution by Gender",
       x = "Medal Type",
       y = "Number of Medals",
       fill = "Gender") +
  scale_fill_manual(values = c("F" = "pink", "M" = "blue")) + 
  theme_minimal()

#Age distribution of gold medalists
gold_medalists <- olympics_data %>%
  filter(medal == "Gold")
# Calculate median and mean
median_age <- median(gold_medalists$age) #25
mean_age <- mean(gold_medalists$age) #25.33
ggplot(gold_medalists, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "gold", color = "black") +
  geom_vline(xintercept = median_age, color = "blue", linetype = "dashed", size = 1, 
             show.legend = TRUE, label = "Median") +
  geom_vline(xintercept = mean_age, color = "red", linetype = "dotted", size = 1, 
             show.legend = TRUE, label = "Mean") +
  labs(title = "Age Distribution of Gold Medalists",
       x = "Age",
       y = "Count",
       caption = paste("Median age:", median_age, "\nMean age:", mean_age)) +
  theme_minimal()

# Histogram of Participation by Year
hist(olympics_data$year, 
     breaks=seq(min(olympics_data$year), max(olympics_data$year), by=4), 
     main="Participation Over the Years", 
     xlab="Year", 
     ylab="Number of Athletes", 
     col="orange")


# rm(list = ls())
