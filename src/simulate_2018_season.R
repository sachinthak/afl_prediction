
library(ProjectTemplate)
load.project()



# global settings ---------------------------------------------------------

K <-  25#10 #25
lambda <- 400 # 700 # 400
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
home_field_advantage_adjust <- TRUE
scoring_method <- 'classic'
point_spread_regression <- NULL
n_sim_tournaments <- 2000
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- 15


# calculate elo by using the data after 2016 ----------------------------------------


sn <- 2016
team_list <- union(past_results[season>=sn]$team1,past_results[season>=sn]$team2)


results <- past_results[season>=sn]

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(results = results, 
                                         initial_ratings = initial_elo_ratings, 
                                         home_field_advantage_stats = hfa_helper_dataset,
                                         K = K, 
                                         lambda = lambda,
                                         autocorrelation_adjust = autocorrelation_adjust,
                                         margin_of_victory_adjust = margin_of_victory_adjust,
                                         home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                         home_field_advantage_coeff = home_field_advantage_coeff,
                                         scoring_method = scoring_method)

updated_elo_ratings <- updated_elo_ratings$ratings

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
                                   home_field_advantage_stats = hfa_helper_dataset,
                                   home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                   home_field_advantage_coeff = home_field_advantage_coeff,
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

