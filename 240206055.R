# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load datasets
results <- read.csv("C:/Users/64371/Documents/r/results.csv")
goalscorers <- read.csv("C:/Users/64371/Documents/r/goalscorers.csv")
shootouts <- read.csv("C:/Users/64371/Documents/r/shootouts.csv")

# Inspect data structure and summary
str(results)
summary(results)

# Check for missing values
sum(is.na(results))
# Return: [1] 0 ---- Missing value is 0

# Subsection 1: Impact of home advantage on match outcomes

# Data preparation: Calculate the difference between home and away scores
results$score_difference <- results$home_score - results$away_score

# Visualization: Distribution of score differences grouped by neutral venue
ggplot(results, aes(x = neutral, y = score_difference, fill = neutral)) +
  geom_boxplot() +
  labs(
    title = "Effect of Neutral Venue on Home Advantage",
    x = "Neutral Venue (TRUE = Neutral, FALSE = Non-Neutral)",
    y = "Home Score - Away Score"
  )

# Correlation analysis: Relationship between neutral venue and score difference
correlation <- cor.test(as.numeric(results$neutral), results$score_difference)

# Print correlation results
print(correlation)

# Build a linear regression model
model <- lm(score_difference ~ neutral, data = results)

# Print regression analysis results
summary(model)

# Visualization: Effect of neutral venue on total goals scored
results$total_score <- results$home_score + results$away_score

ggplot(results, aes(x = neutral, y = total_score, fill = neutral)) +
  geom_boxplot() +
  labs(
    title = "Effect of Neutral Venue on Total Goals",
    x = "Neutral Venue",
    y = "Total Goals"
  )

# Subsection 2: Effect of tournament type on match outcomes

# Count matches by tournament type
tournament_counts <- results %>%
  count(tournament, sort = TRUE)

# Visualization: Bar chart of match counts by tournament type
ggplot(tournament_counts, aes(x = reorder(tournament, n), y = n, fill = tournament)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Number of Matches for Different Event Types",
    x = "Tournament",
    y = "Match Count"
  ) +
  theme(legend.position = "none")

# Analyze average total goals by tournament type
tournament_avg_scores <- results %>%
  group_by(tournament) %>%
  summarise(avg_score = mean(home_score + away_score, na.rm = TRUE)) %>%
  arrange(desc(avg_score))

# Visualization: Average total goals by tournament type
ggplot(tournament_avg_scores, aes(x = reorder(tournament, avg_score), y = avg_score, fill = tournament)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Total Goals in Different Tournament Types",
    x = "Tournament",
    y = "Average Score"
  ) +
  theme(legend.position = "none")

# Test the effect of tournament type on total goals
anova_model <- aov(total_score ~ tournament, data = results)
summary(anova_model)

# Subsection 3: Geographic factors and match distribution

# Count matches by country
country_counts <- results %>%
  count(country, sort = TRUE)

# Visualization: Top 10 countries with the most matches
ggplot(country_counts %>% top_n(10), aes(x = reorder(country, n), y = n, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with the Most Matches",
    x = "Country",
    y = "Match Count"
  ) +
  theme(legend.position = "none")

# Visualization: Average total goals by country
country_avg_scores <- results %>%
  group_by(country) %>%
  summarise(avg_score = mean(home_score + away_score, na.rm = TRUE)) %>%
  arrange(desc(avg_score))

ggplot(country_avg_scores %>% top_n(10), aes(x = reorder(country, avg_score), y = avg_score, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with the Highest Average Total Goals",
    x = "Country",
    y = "Average Total Goals"
  ) +
  theme(legend.position = "none")

# Subsection 4: Match trends over time

# Convert date column to Date type
results$date <- as.Date(results$date)

# Visualization: Total goals over time
ggplot(results, aes(x = date, y = total_score)) +
  geom_line(alpha = 0.6, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Total Goals Over Time",
    x = "Date",
    y = "Total Goals"
  )

# Filter matches for World Cup years
world_cup_years <- c(1930, 1934, 1938, 1950, 1954, 1958, 1962, 1966, 1970, 1974, 1978, 1982, 1986, 1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022)
results$year <- as.numeric(format(results$date, "%Y"))
wc_results <- results %>% filter(year %in% world_cup_years)

# Visualization: Total goals in World Cup years
ggplot(wc_results, aes(x = factor(year), y = total_score, fill = factor(year))) +
  geom_boxplot() +
  labs(
    title = "Total Goals in World Cup Years",
    x = "Year",
    y = "Total Goals"
  ) +
  theme(legend.position = "none")

# Subsection 5: Factors influencing high-scoring matches

# Define high-scoring matches
results$high_score <- ifelse(results$total_score > 5, 1, 0)

# Visualization: Distribution of high-scoring matches
ggplot(results, aes(x = factor(high_score), fill = factor(high_score))) +
  geom_bar() +
  labs(
    title = "Distribution of High-Scoring Matches",
    x = "High-Scoring Match (1 = Yes, 0 = No)",
    y = "Match Count"
  ) +
  theme(legend.position = "none")

# Logistic regression model
logit_model <- glm(high_score ~ neutral + tournament + home_score + away_score, 
                   data = results, 
                   family = "binomial")
summary(logit_model)

# Predicted probabilities
results$predicted_prob <- predict(logit_model, type = "response")

# Visualization: Distribution of predicted probabilities
ggplot(results, aes(x = predicted_prob, fill = factor(high_score))) +
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  labs(
    title = "Predicted Probability of High-Scoring Matches",
    x = "Predicted Probability",
    y = "Match Count",
    fill = "High-Scoring Match"
  )













