

# global settings ---------------------------------------------------------

K <- seq(5,100,5)
lambda <- seq(100,1000,50)
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
home_field_advantage_adjust <- TRUE
scoring_method <- 'classic'
point_spread_regression <- NULL
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- seq(0,100,15)


# Tune parameters   ----------------------------------------


sn <- 2016
team_list <- union(past_results[season>=sn]$team1,past_results[season>=season]$team2)


results <- past_results[season>=sn]

param_grid <- CJ(K = K, lambda = lambda, home_field_advantage_coeff = home_field_advantage_coeff)
param_grid[ , head_to_head_accuracy := numeric(0) ]

nrow_param_grid <- nrow(param_grid)

for (param_test in 1:nrow_param_grid) {
  cat('iteration ', param_test, ' out of ',nrow_param_grid,'\n')
  initial_elo_ratings <- data.table(team = team_list, elo = 1500)
  updated_elo_ratings <- return_elo_scores(results = results, 
                                           initial_ratings = initial_elo_ratings, 
                                           home_field_advantage_stats = hfa_helper_dataset,
                                           K = param_grid[param_test,K], 
                                           lambda = param_grid[param_test,lambda],
                                           autocorrelation_adjust = autocorrelation_adjust,
                                           margin_of_victory_adjust = margin_of_victory_adjust,
                                           home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                           home_field_advantage_coeff = param_grid[param_test,home_field_advantage_coeff],
                                           scoring_method = scoring_method)
  param_grid[param_test,head_to_head_accuracy := updated_elo_ratings$head_to_head_accuracy]
}


# use R Bayesian optimization package
library(rBayesianOptimization)

init_param_grid <- copy(param_grid)
setnames(init_param_grid, 'head_to_head_accuracy','Value')

objective_function <- function(K, lambda, home_field_advantage_coeff){
  initial_elo_ratings <- data.table(team = team_list, elo = 1500)
  updated_elo_ratings <- return_elo_scores(results = results, 
                                           initial_ratings = initial_elo_baratings, 
                                           home_field_advantage_stats = hfa_helper_dataset,
                                           K = K, 
                                           lambda = lambda,
                                           autocorrelation_adjust = autocorrelation_adjust,
                                           margin_of_victory_adjust = margin_of_victory_adjust,
                                           home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                           home_field_advantage_coeff = home_field_advantage_coeff,
                                           scoring_method = scoring_method)
  return(list(Score = updated_elo_ratings$head_to_head_accuracy, Pred = 0))
}

bounds <- list(K = c(param_grid[, min(K)], param_grid[, max(K)]),
               lambda = c(param_grid[, min(lambda)], param_grid[, max(lambda)]),
               home_field_advantage_coeff = c(param_grid[, min(home_field_advantage_coeff)], 
                                              param_grid[, max(home_field_advantage_coeff)]))

ba_search <- BayesianOptimization(objective_function,
                                  bounds = bounds,
                                  init_grid_dt = init_param_grid[sample(1:nrow(init_param_grid),50,replace = F)], 
                                  init_points = 50, 
                                  n_iter = 20,
                                  acq = "ucb", 
                                  kappa = 2, 
                                  eps = 0.0,
                                  verbose = TRUE)

# Best Parameters Found: 
#  Round = 106	K = 12.9889	lambda = 851.8346	home_field_advantage_coeff = 5.5941	Value = 0.6803
