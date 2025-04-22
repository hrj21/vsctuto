# Import Required Libraries
library(dplyr)
library(tidyr)

# Load and Explore the Dataset
data <- read.csv("data/data.csv")
print(head(data))

# Calculate Cure Rates
data_agg <- data %>%
    group_by(Treatment, Outcome) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = Outcome, values_from = count, values_fill = 0) %>%
    mutate(Cure_Rate = Cured / (Cured + Sick))

print(data_agg)

# Compare Cure Rates Against Placebo
placebo_rate <- data_agg %>%
    filter(Treatment == "Placebo") %>%
    pull(Cure_Rate)

data_agg <- data_agg %>%
    mutate(Better_Than_Placebo = Cure_Rate > placebo_rate)

print(data_agg)

# Determine the Drug with the Highest Cure Rate
better_drugs <- data_agg %>%
    filter(Better_Than_Placebo)

if (nrow(better_drugs) > 0) {
    best_drug <- better_drugs %>%
        filter(Cure_Rate == max(Cure_Rate)) %>%
        pull(Treatment)
} else {
    best_drug <- "None"
}

cat(sprintf("The drug with the highest cure rate is: %s\n", best_drug))

# Write the Result to solution.txt
write(best_drug, file = "solution.txt")
