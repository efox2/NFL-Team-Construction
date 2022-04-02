#install.packages('genalg')
library(genalg)
library(ggplot2)

# Set working directory and read in the csv / excel file
setwd("C:/Users/Evan/Documents/Github/NFL-Team-Construction")
df <- read.csv("dataset_v1.csv")
df <- df[c(1,2,3,4,68)]
player_size <- length(df[,1])

# positions
unique(df[,4])
length(unique(df[,4]))

# Setup fitness function
evalFunc <- function(x) {
  
  score = 0
  
  # Get indexes of selected players (one-hot encoded)
  team_ind <- which(x==1)
  current_team_points <- sum(df[team_ind,3])
  current_team_salary <- sum(df[team_ind,5])
  
  # Set constraints and return value
  # if over salary, set to worst score (players all 0) + how over the salary cap the team is / 1000
  if(current_team_salary > sal_limit) {
    new_score = max_team_score + ((current_team_salary - sal_limit) / 100000)
    if(new_score > score) score = new_score
  }
  
  # if incorrect number of players, set to worst score + 1000 * player diff
  if(sum(x) != total_players ) {
    new_score = max_team_score + (abs(sum(x) - total_players) * 10)
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

# Set Contextual Parameters
sal_limit <- 200 * 1000000
total_players = 53
target_players = 53
max_team_score = total_players * 100

# Set Genetic Algorithm Parameters
iter = 1000
popSize = 300
mutationChance = 0.025
nextGeneration = 25

# Run model
rbga.results <- rbga.bin(size = player_size, 
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
bestTeamPlayers = df[which(bestTeam == 1),]
bestTeamPlayers

# save(rbga.results, "test2.Rdata")