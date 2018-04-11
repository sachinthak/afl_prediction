

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


# Tune parameters using  ----------------------------------------


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

