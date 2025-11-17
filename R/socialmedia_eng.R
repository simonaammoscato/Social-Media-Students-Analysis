# Dataset Analysis: "Students' Social Media Addiction.csv"

# Objective:
# To examine students' behavior regarding social media usage,
# comparing the average daily time spent on social media
# between Single and In Relationship students.
# Additionally, potential relationships between social media usage,
# addiction level, sleep quality, and mental health are analyzed.

# Variables in the dataset:
# Student_ID
# Age
# Academic_Level
# Country 
# Avg_Daily_Usage_Hours
# Most_Used_Platform
# Affects_Academic_Performance
# Sleep_Hours_Per_Night
# Mental_Health_Score
# Relationship_Status
# Conflicts_Over_Social_Media
# Addicted_Score
#####################################################################
# Basic import
setwd("~/Desktop/Dataset_R")  
getwd()

library(tidyverse)

social <- read_csv("Students Social Media Addiction.csv")   
social                             

# Check dimensions and structure
dim(social)    
names(social)  
head(social)
str(social)  
glimpse(social)

#####################################################################
# Descriptive statistics of key variables

social |> 
  filter(!is.na(Avg_Daily_Usage_Hours)) |> 
  summarise(
    mean = mean(Avg_Daily_Usage_Hours),
    median = median(Avg_Daily_Usage_Hours),
    sd = sd(Avg_Daily_Usage_Hours),
    min = min(Avg_Daily_Usage_Hours),
    max = max(Avg_Daily_Usage_Hours))

social |> 
  filter(!is.na(Mental_Health_Score)) |> 
  summarise(
    mean = mean(Mental_Health_Score),
    median = median(Mental_Health_Score),
    sd = sd(Mental_Health_Score),
    min = min(Mental_Health_Score),
    max = max(Mental_Health_Score))

social |> 
  filter(!is.na(Sleep_Hours_Per_Night)) |> 
  summarise(
    mean = mean(Sleep_Hours_Per_Night),
    median = median(Sleep_Hours_Per_Night),
    sd = sd(Sleep_Hours_Per_Night),
    min = min(Sleep_Hours_Per_Night),
    max = max(Sleep_Hours_Per_Night))

social |> 
  filter(!is.na(Addicted_Score)) |> 
  summarise(
    mean = mean(Addicted_Score),
    median = median(Addicted_Score),
    sd = sd(Addicted_Score),
    min = min(Addicted_Score),
    max = max(Addicted_Score))

social |> 
  filter(!is.na(Conflicts_Over_Social_Media)) |> 
  summarise(
    mean = mean(Conflicts_Over_Social_Media),
    median = median(Conflicts_Over_Social_Media),
    sd = sd(Conflicts_Over_Social_Media),
    min = min(Conflicts_Over_Social_Media),
    max = max(Conflicts_Over_Social_Media))

social |> 
  filter(!is.na(Age)) |> 
  summarise(
    mean = mean(Age),
    median = median(Age),
    sd = sd(Age),
    min = min(Age),
    max = max(Age))

#####################################################################
# Comparison of Single, Complicated and In Relationship in Avg_Daily_Usage_Hours

social |> 
  group_by(Relationship_Status) |> 
  filter(!is.na(Relationship_Status)) |>
  summarise(
    mean = mean(Avg_Daily_Usage_Hours),
    sd = sd(Avg_Daily_Usage_Hours),
    N = n())

# Boxplot
social |> 
  ggplot(aes(x = Relationship_Status, y = Avg_Daily_Usage_Hours, fill = Relationship_Status)) +
  geom_boxplot() +
  xlab("Relationship Status") +
  ylab("Average Daily Usage Hours") +
  theme_minimal()

# The boxplot shows the distribution of usage hours for each group.

#####################################################################
# Analysis 1: Relationship between Avg_Daily_Usage_Hours and Mental_Health_Score

social |> 
  ggplot(aes(x = Avg_Daily_Usage_Hours, y = Mental_Health_Score, color = Affects_Academic_Performance)) +
  geom_point() +
  xlab("Average Daily Usage Hours") +
  ylab("Mental Health Score") +
  theme_bw()

out.lm1 <- lm(Mental_Health_Score ~ Avg_Daily_Usage_Hours, data = social)

out.lm1 |>
  summary()

social |>
  select(Avg_Daily_Usage_Hours, Mental_Health_Score) |>
  mutate(Mental_Health_Score.hat = predict(out.lm1)) |>
  arrange(Avg_Daily_Usage_Hours)

social |>
  summarise(
    Mx  = mean(Avg_Daily_Usage_Hours),
    Mx2 = mean(Avg_Daily_Usage_Hours^2),
    My  = mean(Mental_Health_Score),
    My2 = mean(Mental_Health_Score^2),
    Mxy = mean(Avg_Daily_Usage_Hours * Mental_Health_Score),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y))

# Comment:
# There is a negative relationship between average daily social media usage
# and mental health score. Each additional hour spent on social media
# is associated with a decrease of about 0.704 points in mental health score.
# The model explains approximately 64.2% of the observed variability, showing
# a strong and statistically significant relationship.

#####################################################################
# Analysis 2: Relationship between Avg_Daily_Usage_Hours and Sleep_Hours_Per_Night

social |> 
  ggplot(aes(x = Avg_Daily_Usage_Hours, y = Sleep_Hours_Per_Night, color = Academic_Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Average Daily Usage Hours") +
  ylab("Sleep Hours Per Night") +
  theme_bw()

out.lm2 <- lm(Sleep_Hours_Per_Night ~ Avg_Daily_Usage_Hours, data = social)

out.lm2 |> 
  summary()

social |>
  select(Avg_Daily_Usage_Hours, Sleep_Hours_Per_Night) |>
  mutate(Sleep_Hours_Pred = predict(out.lm2)) |>
  arrange(Avg_Daily_Usage_Hours)

social |>
  summarise(
    Mx  = mean(Avg_Daily_Usage_Hours),
    Mx2 = mean(Avg_Daily_Usage_Hours^2),
    My  = mean(Sleep_Hours_Per_Night),
    My2 = mean(Sleep_Hours_Per_Night^2),
    Mxy = mean(Avg_Daily_Usage_Hours * Sleep_Hours_Per_Night),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,          
    R2  = Sxy^2 / (S2x * S2y),
    y.hat = a + b * 5.5)        # Predicted sleep hours for 5.5h usage

# Comment: 
# There is a negative relationship between daily social media usage
# and sleep hours. Each additional hour spent on social media
# is associated with a reduction of about 0.71 hours of sleep.
# The model explains about 62.5% of the observed variability.
# A student using social media 5.5 hours/day is predicted to sleep around 6.46 hours/night.

#####################################################################
# Analysis 3: Relationship between Addicted_Score and Conflicts_Over_Social_Media

social |> 
  ggplot(aes(x = Addicted_Score, y = Conflicts_Over_Social_Media, color = Relationship_Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Addiction Score") +
  ylab("Conflicts Over Social Media") +
  theme_bw()

out.lm3 <- lm(Conflicts_Over_Social_Media ~ Addicted_Score, data = social)

out.lm3 |>
  summary()

social |>
  select(Addicted_Score, Conflicts_Over_Social_Media) |>
  mutate(Conflicts_Pred = predict(out.lm3)) |>
  arrange(Addicted_Score)

social |>
  summarise(
    Mx  = mean(Addicted_Score),
    Mx2 = mean(Addicted_Score^2),
    My  = mean(Conflicts_Over_Social_Media),
    My2 = mean(Conflicts_Over_Social_Media^2),
    Mxy = mean(Addicted_Score * Conflicts_Over_Social_Media),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y))

# Comment:
# There is a strong positive relationship between social media addiction
# and frequency of social media-related conflicts.
# Each additional point in addiction score is associated with about
# 0.563 more conflict points. The model explains ~87% of the variability,
# indicating addiction score is a strong predictor of relational conflicts.

#####################################################################
# Analysis 4: Relationship between Mental_Health_Score and Sleep_Hours_Per_Night

social |>
  ggplot(aes(x = Sleep_Hours_Per_Night, y = Mental_Health_Score, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Sleep Hours Per Night") +
  ylab("Mental Health Score") +
  theme_bw()

out.lm4 <- lm(Mental_Health_Score ~ Sleep_Hours_Per_Night, data = social)

out.lm4 |> 
  summary()

social |>
  select(Sleep_Hours_Per_Night, Mental_Health_Score) |>
  mutate(Mental_Health_Pred = predict(out.lm4)) |>
  arrange(Sleep_Hours_Per_Night)

social |>
  summarise(
    Mx  = mean(Sleep_Hours_Per_Night),
    Mx2 = mean(Sleep_Hours_Per_Night^2),
    My  = mean(Mental_Health_Score),
    My2 = mean(Mental_Health_Score^2),
    Mxy = mean(Sleep_Hours_Per_Night * Mental_Health_Score),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y),
    y.hat = a + b * 7)

# Comment:
# There is a positive relationship between sleep hours and mental health score.
# Each additional hour of sleep is associated with ~0.69 higher mental health score.
# The model explains ~50% of observed variability, suggesting a moderately solid relationship.

#####################################################################
# Analysis 5: Relationship between Age and Conflicts_Over_Social_Media

social |> 
  ggplot(aes(x = Conflicts_Over_Social_Media, y = Age, color = Relationship_Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Conflicts Over Social Media") +
  ylab("Age")

out.lm5 <- lm(Conflicts_Over_Social_Media ~ Age, data = social)

out.lm5 |> 
  summary()

social |>
  select(Age, Conflicts_Over_Social_Media) |>
  mutate(Conflicts_Pred = predict(out.lm5)) |>
  arrange(Age)

social |>
  summarise(
    Mx  = mean(Age),
    Mx2 = mean(Age^2),
    My  = mean(Conflicts_Over_Social_Media),
    My2 = mean(Conflicts_Over_Social_Media^2),
    Mxy = mean(Age * Conflicts_Over_Social_Media),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y),
    y.hat = a + b * 22)        

# Comment:
# There is a relationship between students' age and the number of conflicts
# related to social media usage. Each additional year of age is associated
# with a reduction of about 0.126 points in conflicts.
# The model explains ~3.4% of the observed variability, suggesting age has
# a weak but statistically significant effect on social media conflicts.
# Predicted conflicts for a 22-year-old student is about 2.68.
# Older students tend to experience slightly fewer social media conflicts than younger ones, 
# although the overall effect is limited.




#####################################################################
# Credits
# This analysis was inspired by the work and teaching materials of Luigi Augugliaro, Full Professor at the University of Palermo.
# The dataset and concepts were used for educational purposes only.