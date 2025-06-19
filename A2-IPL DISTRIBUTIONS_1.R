# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(fitdistrplus)

# Set working directory
setwd("C:/Users/ribin/OneDrive/Desktop/SCMA-632/Data")

# Load data
ipl_bbb <- read.csv("IPL_ball_by_ball_updated till 2024.csv", stringsAsFactors = FALSE)
ipl_salary <- readxl::read_excel("IPL SALARIES 2024.xlsx")

# Grouping data: Season, Innings No, Striker, Bowler
grouped_data <- ipl_bbb %>%
  group_by(Season, Innings.No, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), wicket_confirmation = sum(wicket_confirmation), .groups = 'drop')

# Total runs and wickets per season
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored), .groups = 'drop')

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = 'drop')

# Top players for 2023
player_runs %>% filter(Season == '2023') %>% arrange(desc(runs_scored))
player_wickets %>% filter(Season == '2023') %>% arrange(desc(wicket_confirmation))

# Top 3 players by season
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  slice_max(order_by = runs_scored, n = 3)

top_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  slice_max(order_by = wicket_confirmation, n = 3)

print(top_run_getters)
print(top_wicket_takers)

# Add year column
ipl_bbb$year <- format(as.Date(ipl_bbb$Date, format = "%d-%m-%Y"), "%Y")

# Find total runs per year
total_run_each_year <- ipl_bbb %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored), .groups = 'drop') %>%
  arrange(year, desc(runs_scored))

# Top batsmen for the last three years
list_top_batsman_last_three_year <- list()
for (y in unique(total_run_each_year$year)[1:3]) {
  list_top_batsman_last_three_year[[y]] <- total_run_each_year %>%
    filter(year == y) %>%
    arrange(desc(runs_scored)) %>%
    head(3) %>%
    pull(Striker)
}

# Function to find best distribution
get_best_distribution <- function(data) {
  fit <- fitdist(data, "norm")
  print(summary(fit))
}

# Run distribution fitting for top batsmen
runs <- ipl_bbb %>%
  group_by(Striker, Match.id) %>%
  summarise(runs_scored = sum(runs_scored), .groups = 'drop')

for (year in names(list_top_batsman_last_three_year)) {
  for (batsman in list_top_batsman_last_three_year[[year]]) {
    cat("************************\n")
    cat("Year:", year, "Batsman:", batsman, "\n")
    player_runs <- runs %>% filter(Striker == batsman) %>% pull(runs_scored)
    get_best_distribution(player_runs)
    cat("\n\n")
  }
}

# Find total wickets per year
total_wicket_each_year <- ipl_bbb %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = 'drop') %>%
  arrange(year, desc(wicket_confirmation))

# Top bowlers for last three years
list_top_bowler_last_three_year <- list()
for (y in unique(total_wicket_each_year$year)[1:3]) {
  list_top_bowler_last_three_year[[y]] <- total_wicket_each_year %>%
    filter(year == y) %>%
    arrange(desc(wicket_confirmation)) %>%
    head(3) %>%
    pull(Bowler)
}

# Run distribution fitting for top bowlers
wickets <- ipl_bbb %>%
  group_by(Bowler, Match.id) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = 'drop')

for (year in names(list_top_bowler_last_three_year)) {
  for (bowler in list_top_bowler_last_three_year[[year]]) {
    cat("************************\n")
    cat("Year:", year, "Bowler:", bowler, "\n")
    player_wickets <- wickets %>% filter(Bowler == bowler) %>% pull(wicket_confirmation)
    get_best_distribution(player_wickets)
    cat("\n\n")
  }
}

# Best fit distribution for Tilak Varma
tilak_varma_runs <- ipl_bbb %>%
  filter(Striker == 'Tilak Varma') %>%
  group_by(Match.id) %>%
  summarise(runs_scored = sum(runs_scored), .groups = 'drop')

cat("Finding best fit distribution for Tilak Varma\n")
get_best_distribution(tilak_varma_runs$runs_scored)

# Correlation between salary and runs
R2024 <- total_run_each_year %>% filter(year == '2024')

# Fuzzy matching alternative in R
# Clean names to increase matching probability
library(stringdist)
R2024$Striker_clean <- tolower(R2024$Striker)
ipl_salary$Player_clean <- tolower(ipl_salary$Player)

# Find closest matches
ipl_salary$Matched_Player <- sapply(ipl_salary$Player_clean, function(x) {
  distances <- stringdist::stringdist(x, R2024$Striker_clean)
  matched_index <- which.min(distances)
  if (min(distances) <= 3) {
    return(R2024$Striker[matched_index])
  } else {
    return(NA)
  }
})

# Merge datasets
merged_data <- merge(ipl_salary, R2024, by.x = 'Matched_Player', by.y = 'Striker', all = FALSE)

# Correlation
correlation <- cor(merged_data$Rs, merged_data$runs_scored, use = "complete.obs")
print(paste("Correlation between Salary and Runs:", correlation))
