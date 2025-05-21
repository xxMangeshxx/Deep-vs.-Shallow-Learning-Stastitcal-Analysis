
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("GGally")) install.packages("GGally")

library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)


dataset <- read_csv("D:/R project/learning_comparison_dataset_adjusted.csv")


head(dataset)

summary(dataset)


gpa_comparison <- dataset %>%
  group_by(Learning_Type) %>%
  summarise(Mean_GPA = mean(GPA, na.rm = TRUE),
            SD_GPA = sd(GPA, na.rm = TRUE),
            Count = n())

print(gpa_comparison)


ggplot(dataset, aes(x = GPA, fill = Learning_Type)) +
  geom_density(alpha = 0.5) +
  labs(title = "GPA Distribution: Deep Learning vs Shallow Learning",
       x = "GPA",
       y = "Density") +
  theme_minimal()

ggplot(dataset, aes(x = Learning_Type, y = Student_Satisfaction_Score, fill = Learning_Type)) +
  geom_boxplot() +
  labs(title = "Student Satisfaction Score by Learning Type",
       x = "Learning Type",
       y = "Student Satisfaction Score") +
  theme_minimal()

ggplot(dataset, aes(x = Study_Hours_Per_Week, y = GPA, color = Learning_Type)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Study Hours per Week vs GPA",
       x = "Study Hours per Week",
       y = "GPA") +
  theme_minimal()



ggplot(dataset, aes(x = Course_Difficulty, y = GPA, fill = Learning_Type)) +
  geom_boxplot() +
  labs(title = "GPA by Course Difficulty and Learning Type",
       x = "Course Difficulty",
       y = "GPA") +
  theme_minimal()

ggplot(dataset, aes(x = Learning_Type, y = Instructor_Experience_Years, fill = Learning_Type)) +
  geom_boxplot() +
  labs(title = "Instructor Experience Years by Learning Type",
       x = "Learning Type",
       y = "Instructor Experience Years") +
  theme_minimal()

ggplot(dataset, aes(x = Class_Size, fill = Learning_Type)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Class Size Distribution by Learning Type",
       x = "Class Size",
       y = "Frequency") +
  theme_minimal()

ggplot(dataset, aes(x = Technology_Used, fill = Learning_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Technology Used in Courses by Learning Type",
       x = "Technology Used",
       y = "Count") +
  theme_minimal()





