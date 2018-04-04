
library(ProjectTemplate)
load.project()



# global settings ---------------------------------------------------------

K <- 25
lambda <- 400
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
home_field_advantage_adjust <- TRUE
scoring_method <- 'classic'
point_spread_regression <- NULL
n_sim_tournaments <- 300
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- 15


# calculate elo by using the data after 2016 ----------------------------------------


sn <- 2016
team_list <- union(past_results[season>=sn]$team1,past_results[season>=season]$team2)


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



# round 3 match predictions -----------------------------------------------

simple_logit_point_spread_fit <- simple_logit_point_spread_regression(results = results, initial_elo_ratings = initial_elo_ratings,
                                                                      home_field_advantage_stats = hfa_helper_dataset,
                                                                      K = K, lambda = lambda, 
                                                                      autocorrelation_adjust = autocorrelation_adjust,
                                                                      margin_of_victory_adjust = margin_of_victory_adjust,
                                                                      scoring_method = scoring_method,
                                                                      home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                                                      home_field_advantage_coeff = home_field_advantage_coeff,
                                                                      reg_to_mean_factor = reg_to_mean_factor)
sn <- 2018
rnd <- 'Round 3'

schedule_for_the_round <- schedules[season == sn & round == rnd]
round_prediction <- data.table(team1 = character(0), team2 = character(0), 
                               venue = character(0), margin = numeric(0))
for (match in 1:nrow(schedule_for_the_round)) {
  
  team_1 <- schedule_for_the_round[match,team1]
  team_2 <-  schedule_for_the_round[match,team2]
  ground <- schedules[season == sn & round == rnd & team1 == team_1 & team2 == team_2, venue]
  num_past_matches_team_1 <- hfa_helper_dataset[season == sn & venue == ground & team == team_1, num_past_matches]
  num_past_matches_team_2 <- hfa_helper_dataset[season == sn & venue == ground & team == team_2, num_past_matches]
  home_field_advantage <- home_field_advantage_coeff*(log(1+num_past_matches_team_1)-log(1+num_past_matches_team_2))
  elo_diff <- updated_elo_ratings[team==team_1,elo] - updated_elo_ratings[team==team_2,elo] 
  if (home_field_advantage_adjust) {
    elo_diff <- elo_diff + home_field_advantage
  }
  p <- 1/(1+10^(-elo_diff/lambda))
  logit_p <- log(p/(1-p))
  margin <- predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) 
  
  round_prediction <- rbind(round_prediction, data.table(team1 = team_1, team2 = team_2,
                                                         venue = ground, margin = margin))
}

round_prediction
