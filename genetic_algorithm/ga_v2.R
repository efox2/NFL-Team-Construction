#install.packages('genalg')
library(genalg)
library(ggplot2)

# Set working directory and read in the csv / excel file
setwd("C:/Users/Evan/Documents/Github/NFL-Team-Construction/Datasets")
df <- read.csv("simplified_dataset_v2.csv")
player_size <- length(df[,1])

# positions
unique(df[,5])
length(unique(df[,5]))

# Set Contextual Parameters
sal_limit <- 200 * 1000000
total_players = 53
target_players = 3
max_team_score = total_players * 100

# Set Genetic Algorithm Parameters
iter = 100
popSize = 300
mutationChance = 0.01
nextGeneration = 25

# Pre-select some players
fullTeamIxs = which(df$Team == 'New England Patriots')
fullTeamDF = df[fullTeamIxs,]
preSelectedIxs = sample(fullTeamIxs, total_players - target_players, replace = FALSE)
modified_df <- df
modified_df$oldIX <- row.names(df)
if (length(preSelectedIxs) > 0) modified_df <- modified_df[-preSelectedIxs,]
row.names(modified_df) <- NULL

# Setup fitness function
evalFunc <- function(x) {
  
  score = 0
  
  # Get indexes of selected players (one-hot encoded)
  team_ind <- which(x==1)
  current_team_points <- sum(modified_df[team_ind,6]) + sum(df[preSelectedIxs,6])
  current_team_salary <- sum(modified_df[team_ind,8]) + sum(df[preSelectedIxs,8])
  
  # Set constraints and return value
  # if over salary, set to worst score (players all 0) + how over the salary cap the team is / 1000
  if(current_team_salary > sal_limit) {
    new_score = max_team_score + ((current_team_salary - sal_limit) / 100000)
    if(new_score > score) score = new_score
  }
  
  # if incorrect number of players, set to worst score + 1000 * player diff
  if(sum(x) + total_players - target_players != total_players ) {
    new_score = max_team_score + (abs(sum(x) - target_players) * 10)
    if(new_score > score) score = new_score
  }
  
  # if no constraint issues, return max score - current score
  new_score = max_team_score - current_team_points
  if(new_score > score) score = new_score
  
  return(score)
}

# Setup monitoring function
monitorFunc <- function(obj) {
  # components of obj:
  # $names
  # [1] "type"           "size"           "popSize"        "iter"           "iters"          "population"     "elitism"        "mutationChance"
  # [9] "evaluations"    "best"           "mean"          
  minEval = min(obj$evaluations);
  plot(obj, type="hist")
  print(obj$iter)
  minEval = min(obj$evaluations)
  print(minEval)
}

# Run model
rbga.results <- rbga.bin(size = player_size - total_players + target_players, 
                         popSize = popSize, 
                         iters = iter, 
                         mutationChance = mutationChance,  
                         elitism = nextGeneration,
                         evalFunc = evalFunc, 
                         monitorFunc = monitorFunc,
                         verbose=FALSE)

# Print results
summary(rbga.results, echo = T)
plot(rbga.results) # type can be "hist", "vars", or "default"

# best evaluation score
minEval = min(rbga.results$evaluations)
bestIx = rbga.results$evaluations == minEval
bestTeam = rbga.results$population[bestIx,]
bestTeamPlayers = modified_df[which(bestTeam == 1),]
newFullTeamPlayers = df[c(strtoi(bestTeamPlayers$oldIX), preSelectedIxs),]

# save(rbga.results, "test2.Rdata")