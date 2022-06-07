#install.packages('genalg')
library(genalg)
library(ggplot2)
library(xgboost)
library(data.table)
library(lattice)
geneticalg3 <- function(target_players, selectedTeam){
  # Set working directory and read in the csv / excel file
  # make sure to change the wd
  df = read.csv("./simplified_dataset_v2_old.csv")
  player_size <- length(df[,1])
  
  # positions
  unique(df[,5])
  length(unique(df[,5]))
  # Fix positions
  positions = c("QB", "RB", "OL", "TE", "WR", "DL", "LB", "DB", "K", "P")
  position_counts = c(2, 3, 6, 2, 4, 6, 5, 6, 1, 1)
  
  ml_position_counts = c(3, 4, 9, 3, 6, 9, 7, 10, 1, 1)
  
  df$Position[df$Position == "LG"] <- "OL"
  df$Position[df$Position == "RG"] <- "OL"
  df$Position[df$Position == "LT"] <- "OL"
  df$Position[df$Position == "RT"] <- "OL"
  df$Position[df$Position == "C"] <- "OL"
  df$Position[df$Position == "LE"] <- "DL"
  df$Position[df$Position == "RE"] <- "DL"
  df$Position[df$Position == "DT"] <- "DL"
  df$Position[df$Position == "MLB"] <- "LB"
  df$Position[df$Position == "LOLB"] <- "LB"
  df$Position[df$Position == "ROLB"] <- "LB"
  df$Position[df$Position == "CB"] <- "DB"
  df$Position[df$Position == "SS"] <- "DB"
  df$Position[df$Position == "FS"] <- "DB"
  df$Position[df$Position == "HB"] <- "RB"
  df$Position[df$Position == "FB"] <- "RB"
  df$Position[df$Position == "RE"] <- "OL"
  
  # Set Contextual Parameters
  sal_limit <- 200 * 1000000
  total_players = 53
  target_players = 3
  max_team_score = total_players * 100
  
  # read xgboost model
  #ml_xgboost = readRDS("./xgboost_regression.rds")
  
  ml_rpart = readRDS("../src/genetic_algorithm/rpart_model.rds")
  
  # Set Genetic Algorithm Parameters
  iter = 100
  popSize = 300
  mutationChance = 0.01
  nextGeneration = 25
  
  # Set Penalty Parameters
  salPenalty = 10000      # penalty is how over the cap you are / salPenalty
  playerCountPenalty = 100  # penalty is abs(how many players you have - target) * playerCountPenalty
  positionalPenalty = 100 # penalty is how many missing required position players * positionalPenalty
  
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
    current_team_pos <- c(modified_df[team_ind,5], df[preSelectedIxs,5])
    
    overall = c(modified_df[team_ind,6]) + sum(df[preSelectedIxs,6])
    
    team_input = data.frame(rbind(modified_df[team_ind,c(5,6)], df[preSelectedIxs, c(5,6)]))
    
    # ml_position_counts, positions
    ml_input = c()
    for(position in positions) {
      inPosition = team_input[which(team_input$Position == position),]
      if(nrow(inPosition) < ml_position_counts[which(positions==position)]) {
        chosen = inPosition[order(inPosition$Overall.Rating, decreasing=TRUE),]
        ml_input = append(ml_input, chosen$Overall.Rating)
        ml_input = append(ml_input, c(integer(ml_position_counts[which(positions==position)] - nrow(inPosition))))
      } else {
        chosen = inPosition[order(inPosition$Overall.Rating, decreasing=TRUE),][0:ml_position_counts[which(positions==position)],]
        ml_input = append(ml_input, chosen$Overall.Rating)
      }
    }
    rpartdf <- data.frame(matrix(ncol = 53, nrow = 0))
    xy <- c('Quarter.Back_1','Quarter.Back_2','Quarter.Back_3','Running.Back_1','Running.Back_2','Running.Back_3','Running.Back_4','Offensive.Lineman_1','Offensive.Lineman_2',
            'Offensive.Lineman_3','Offensive.Lineman_4','Offensive.Lineman_5','Offensive.Lineman_6','Offensive.Lineman_7','Offensive.Lineman_8','Offensive.Lineman_9',
            'Tight.End_1','Tight.End_2','Tight.End_3','Wide.Receiver_1','Wide.Receiver_2','Wide.Receiver_3','Wide.Receiver_4','Wide.Receiver_5','Wide.Receiver_6',
            'Defensive.Lineman_1','Defensive.Lineman_2','Defensive.Lineman_3','Defensive.Lineman_4','Defensive.Lineman_5','Defensive.Lineman_6','Defensive.Lineman_7','Defensive.Lineman_8','Defensive.Lineman_9',
            'Line.Backer_1','Line.Backer_2','Line.Backer_3','Line.Backer_4','Line.Backer_5','Line.Backer_6','Line.Backer_7',
            'Defensive.Back_1','Defensive.Back_2','Defensive.Back_3','Defensive.Back_4','Defensive.Back_5','Defensive.Back_6','Defensive.Back_7','Defensive.Back_8','Defensive.Back_9','Defensive.Back_10'
            ,'Kicker_1','Punter_1')
    colnames(rpartdf) <- xy
    ml_inp = c(ml_input)
    rpartdf[nrow(rpartdf) + 1,] <- rbind(rpartdf,ml_inp)
    # ml_input is the input to the machine learning function
    # ml_output is the output of the machine learning function
    #####
    
    ml_input_rpart=rpartdf
    #ml_input = matrix(unlist(ml_input), ncol =53, nrow =1)
    #team_prediction = predict(ml_xgboost, ml_input_rpart)
    #ml_output = team_prediction
    
    team_win_percentage = predict(ml_rpart, ml_input_rpart)
    ml_output = team_win_percentage
    
    ######
    
    # Determine score for genalg
    # ml_output is output from ml model
    score = max_team_score + 1 - ((ml_output * max_team_score)/100)
    
    # Set constraints and return value
    # if over salary, set to worst score (players all 0) + how over the salary cap the team is / 1000
    if(current_team_salary > sal_limit) {
      new_score = max_team_score + ((current_team_salary - sal_limit) / salPenalty)
      if(new_score > score) score = new_score
    }
    
    # if incorrect number of players, set to worst score + 1000 * player diff
    if(sum(x) != target_players ) {
      new_score = max_team_score + (abs(sum(x) - target_players) * playerCountPenalty)
      if(new_score > score) score = new_score
    }
    
    # if no constraint issues, return max score - current score
    new_score = max_team_score - current_team_points
    if(new_score > score) score = new_score
    
    
    # modify score for positional considerations
    # penalize 2 points for every missing required position player
    for (position in positions) {
      inPosition = length(which(current_team_pos == position))
      neededPosition = position_counts[which(positions==position)]
      if (inPosition < neededPosition) score = score + positionalPenalty * (neededPosition - inPosition)
    }
    return(score)
  }
  
  # Setup monitoring function
  monitorFunc <- function(obj) {
    # components of obj:
    # $names
    # [1] "type"           "size"           "popSize"        "iter"           "iters"          "population"     "elitism"        "mutationChance"
    # [9] "evaluations"    "best"           "mean"          
    minEval = min(obj$evaluations);
    plot(obj, type="hist", sub=sprintf("iteration %d", obj$iter))
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
  if (sum(bestIx) > 1) bestTeam = bestTeam[1,]
  bestTeamPlayers = modified_df[which(bestTeam == 1),]
  newFullTeamPlayers = df[c(strtoi(bestTeamPlayers$oldIX), preSelectedIxs),]
  
  # Player Count Breakdown
  print(sprintf("Number of Players on Team: %d", length(newFullTeamPlayers$Team)))
  
  # Salary Cap Breakdown
  print(sprintf("Cap Room Remaining: %d million", floor((sal_limit - sum(newFullTeamPlayers$Annual.Salary..in...)) / 1000000)))
  
  # Position breakdown
  for (position in positions) {
    neededPosition = position_counts[which(positions==position)]
    print(sprintf("Extra %s: %d", position,length(which(newFullTeamPlayers$Position == position)) - neededPosition))
  }
  
  #make sure to change the wd
  #setwd("/Users/sanjayaravind/Downloads/Datasets")
  save.image(file="test4.RData") 
  return(newFullTeamPlayers)
}
