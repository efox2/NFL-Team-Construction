#install.packages('genalg')
library(genalg)
library(ggplot2)

# Set working directory and read in the csv / excel file
setwd("C:/Users/Evan/Documents/UB_Schoolwork/EAS509/Project")
df <- read.csv("week11-full.csv")

# Get indexes of player positions
TE_ind <- which(df[4]=='TE') 
QB_ind <- which(df[4]=='QB') 
RB_ind <- which(df[4]=='RB')
WR_ind <- which(df[4]=='WR')
DEF_ind <- which(df[4]=='DST')#DEF_ind <- which(df[4]=='Defense')

# Setup fitness function
evalFunc <- function(x) {
   
   # Get indexes of selected players (one-hot encoded)
   team_ind <- which(x==1)
   current_solution_points <- sum(df[team_ind,2])
   current_solution_weight <- sum(df[team_ind,3])
   # print(current_solution_weight)
   # print(current_solution_points)

   # Set constraints and return value
   if(current_solution_weight > sal.limit) return(abs(50000 - current_solution_weight))
   if(sum(x) != 9 )  return(200*(abs(sum(x)-9)))
   if(sum(x[QB_ind]) >1|| sum(x[DEF_ind]) >1 )   return(500)
   if(sum(x[TE_ind]) == 2 && (sum(x[RB_ind]) >2 || sum(x[WR_ind]) >3)) {return(sum(x[TE_ind])*50)}
   if(sum(x[WR_ind]) == 4 && (sum(x[RB_ind]) >2 || sum(x[TE_ind]) >1)) {return(sum(x[WR_ind])*50)}
   if(sum(x[WR_ind]) > 4  || (sum(x[RB_ind]) >3 || sum(x[TE_ind]) >2)) {return(sum(x[WR_ind])*50)}
   return(-current_solution_points)
}

monitorFunc <- function(obj) {
   minEval = min(obj$evaluations);
   plot(obj, type="hist")
}

# Model building
iter = 100
player_size <- length(df[,1])
sal.limit <- 50000

# Run model
rbga.results <- rbga.bin(size = player_size, 
                    popSize = 500, 
                    iters = iter, 
                    mutationChance = 0.01,  
                    evalFunc = evalFunc, 
                    monitorFunc = monitorFunc,
                    verbose=TRUE)

# Print results
summary(rbga.results, echo = T)

plot(rbga.results) # type can be "hist", "vars", or "default"

# # EXAMPLE for floating point chromosome too
# # optimize two values to match pi and sqrt(50)
# evaluate <- function(string=c()) {
#    returnVal = NA;
#    if (length(string) == 2) {
#       returnVal = abs(string[1]-pi) + abs(string[2]-sqrt(50));
#    } else {
#       stop("Expecting a chromosome of length 2!");
#    }
#    returnVal
# }
# 
# monitor <- function(obj) {
#    # plot the population
#    xlim = c(obj$stringMin[1], obj$stringMax[1]);
#    ylim = c(obj$stringMin[2], obj$stringMax[2]);
#    plot(obj$population, xlim=xlim, ylim=ylim, 
#         xlab="pi", ylab="sqrt(50)");
# }
# 
# rbga.results = rbga(c(1, 1), c(5, 10), monitorFunc=monitor, 
#                     evalFunc=evaluate, verbose=TRUE, mutationChance=0.01)
# 
# plot(rbga.results)
# plot(rbga.results, type="hist")
# plot(rbga.results, type="vars")
