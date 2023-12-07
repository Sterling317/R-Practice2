install.packages("psych")
library(psych)
library(janitor)
library(dplyr)
library(ggplot2)

salary_data <- read.csv("Salary_Data.csv")
# Clean column name
salary_data <- salary_data %>% 
  clean_names()
#Check whether the data frame contains null values and clean 
any(is.na(salary_data))
salary_data <- na.omit(salary_data)

# Descriptive Statistics
describe(salary_data)

# Grouped Descriptive Statistics
sub_salary_data <- salary_data %>%
  select(-c(gender, job_title, education_level)) #Remove some columns containing strings
grouped_describe <- describe(sub_salary_data)
selected_describe <- grouped_describe[c("n", "mean", "median", "sd", "se", "min", "max")]

# Visualization
# Scatter plot for experience and salary
# Set graphics parameters
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
# Create a scatter plot
plot(salary_data$years_of_experience, salary_data$salary, main="Experience vs Salary", 
     xlab="Years of Experience", 
     ylab="Salary", 
     pch=19, 
     col="blue")
# Calculate the regression model
model <- lm(salary ~ years_of_experience, data=salary_data)
# Add a regression line
abline(model, col="red")

#Create a box plot showing salary distribution of different job roles
#Select the top three positions with the largest sample size
top_roles <- names(sort(table(salary_data$job_title), decreasing = TRUE))[1:3]
# filter data
filtered_data <- subset(salary_data, job_title %in% top_roles)
# Create box plot
ggplot(filtered_data, aes(x=job_title, y=salary, fill = job_title)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Salary by Job Role(Top 3 Job Role)", x="Job Titles", y="Salary")

# Use ggplot2 to draw a jitter chart
salary_data <- subset(salary_data, education_level != "phD")
#Unify row names
salary_data$education_level <- gsub("\\bMaster's\\b", "Master's Degree", salary_data$education_level)
salary_data$education_level <- gsub("\\bMaster's Degree Degree\\b", "Master's Degree", salary_data$education_level)
salary_data$education_level <- gsub("\\bBachelor's\\b", "Bachelor's Degree", salary_data$education_level)
salary_data$education_level <- gsub("\\bBachelor's Degree Degree\\b", "Bachelor's Degree", salary_data$education_level)
#Draw jitter chart
ggplot(salary_data, aes(x=education_level, y=salary)) +
  geom_jitter(width=0.3, alpha=0.1, color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Salary Distribution by Education Level", x="Education Level", y="Salary")
