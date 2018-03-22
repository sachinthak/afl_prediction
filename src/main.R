library(ProjectTemplate)
load.project()


# just playing with elo functions -----------------------------------------


sn <- 2017
team_list <- union(past_results[season==sn]$team1,past_results[season==season]$team2)
schedule = schedules[season == sn]

# play with  the following to choose matches_played_so_far for simulation purposes
past_results[season==sn][grep('Round',round), cbind(.SD,.I)][round == 'Round 4']

matches_played_so_far <- 0
results_so_far <- past_results[season==sn][grep('Round',round)][1:matches_played_so_far]

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(results = results_so_far, 
                                         initial_elo_ratings, 
                                         K = 25, 
                                         lambda = 400,
                                         autocorrelation_adjust = TRUE,
                                         margin_of_victory_adjust = TRUE,
                                         scoring_method = 'classic')


# run point spread regressions --------------------------------------------

results <- past_results[season > 2013]
team_list <- union(results$team1,results$team2)
initial_elo_ratings <- data.table(team = team_list, elo = 1500)
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
scoring_method <-  'point_based'
reg_to_mean_factor <- 0.75

K <- 25
lambda <- 400

elo_ratings <- return_elo_scores(results = results, 
                                 initial_elo_ratings, 
                                 K = 25, 
                                 lambda = 400,
                                 autocorrelation_adjust = autocorrelation_adjust,
                                 margin_of_victory_adjust = margin_of_victory_adjust,
                                 scoring_method = scoring_method,
                                 reg_to_mean_factor = reg_to_mean_factor)

simple_point_spread_fit <- simple_point_spread_regression (results, initial_elo_ratings,
                                K, lambda, autocorrelation_adjust = autocorrelation_adjust,
                                margin_of_victory_adjust = margin_of_victory_adjust,
                                scoring_method = scoring_method,
                                reg_to_mean_factor = reg_to_mean_factor)

elo_diff <- elo_ratings[team=='Adelaide',elo] - elo_ratings[team=='Essendon',elo]
predict.lm(simple_point_spread_fit, newdata = data.table(elo_diff = elo_diff)) # 25.58966
