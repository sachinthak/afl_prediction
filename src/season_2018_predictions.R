
library(ProjectTemplate)
load.project()



# global settings ---------------------------------------------------------

K <- 25
lambda <- 400
autocorrelation_adjust <- FALSE
margin_of_victory_adjust <- FALSE
scoring_method <- 'classic'
point_spread_regression <- NULL
n_sim_tournaments <- 300
reg_to_mean_factor <- 0.75

# calculate elo by using the data after 2016 ----------------------------------------


sn <- 2016
team_list <- union(past_results[season>=sn]$team1,past_results[season>=season]$team2)


results <- past_results[season>=sn]

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(results = results, 
                                         initial_elo_ratings, 
                                         K = K, 
                                         lambda = lambda,
                                         autocorrelation_adjust = autocorrelation_adjust,
                                         margin_of_victory_adjust = margin_of_victory_adjust,
                                         scoring_method = scoring_method)


# simulate 2018 season ----------------------------------------------------

sn <- 2018
schedule = schedules[season == sn]

matches_so_far <- nrow(past_results[season == sn])
results_so_far <- past_results[season==sn][1:matches_so_far]



sim_results <- simulate_tournament(schedule = schedule, 
                                   K = K, 
                                   lambda = lambda, 
                                   elo_ratings = updated_elo_ratings, 
                                   results_so_far = results_so_far,
                                   scoring_method = scoring_method,
                                   autocorrelation_adjust = autocorrelation_adjust,
                                   margin_of_victory_adjust = margin_of_victory_adjust,
                                   point_spread_regression = NULL,
                                   n_sim_tournaments = n_sim_tournaments)

# plot probabilities
ggplot(sim_results$championship_frequency_table) + 
  geom_bar(aes(x=reorder(team,-prob), y = prob), stat = 'identity') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + scale_x_discrete(name = 'team') +
  scale_y_continuous(name = paste0('Probability of winning the premiership after ',matches_so_far,' matches'))

ggsave(paste0('2018_premiership_prob_after_',matches_so_far,'_matches.pdf'))



# round 2 match predictions -----------------------------------------------

simple_logit_point_spread_fit <- simple_logit_point_spread_regression(results, initial_elo_ratings,
                                                                      K, lambda, autocorrelation_adjust = autocorrelation_adjust,
                                                                      margin_of_victory_adjust = margin_of_victory_adjust,
                                                                      scoring_method = scoring_method,
                                                                      reg_to_mean_factor = reg_to_mean_factor)


elo_diff <- elo_ratings[team=='St Kilda',elo] - elo_ratings[team=='Brisbane Lions',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # 19.0488
predict.lm(simple_point_spread_fit, newdata = data.table(elo_diff = elo_diff)) # 19.0488 
