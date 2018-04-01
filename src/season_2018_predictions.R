
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
n_sim_tournaments <- 200
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- 6


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

simple_logit_point_spread_fit <- simple_logit_point_spread_regression(results = results, initial_elo_ratings = initial_elo_ratings,
                                                                      home_field_advantage_stats = hfa_helper_dataset,
                                                                      K = K, lambda = lambda, 
                                                                      autocorrelation_adjust = autocorrelation_adjust,
                                                                      margin_of_victory_adjust = margin_of_victory_adjust,
                                                                      scoring_method = scoring_method,
                                                                      home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                                                      home_field_advantage_coeff = home_field_advantage_coeff,
                                                                      reg_to_mean_factor = reg_to_mean_factor)


elo_diff <- updated_elo_ratings[team=='Adelaide',elo] - updated_elo_ratings[team=='Richmond',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -14.18807 


elo_diff <- updated_elo_ratings[team=='North Melbourne',elo] - updated_elo_ratings[team=='St Kilda',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -20.31522


elo_diff <- updated_elo_ratings[team=='Carlton',elo] - updated_elo_ratings[team=='Gold Coast',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # 0.4718978



elo_diff <- updated_elo_ratings[team=='Collingwood',elo] - updated_elo_ratings[team=='Greater Western Sydney',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -20.1379 



elo_diff <- updated_elo_ratings[team=='Brisbane Lions',elo] - updated_elo_ratings[team=='Melbourne',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -23.78697


elo_diff <- updated_elo_ratings[team=='Fremantle',elo] - updated_elo_ratings[team=='Essendon',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -27.55141



elo_diff <- updated_elo_ratings[team=='Western Bulldogs',elo] - updated_elo_ratings[team=='West Coast',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # -6.040386



elo_diff <- updated_elo_ratings[team=='Sydney',elo] - updated_elo_ratings[team=='Port Adelaide',elo]
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # 10.76364


sn <- 2018
t1 <- 'Geelong'
t2 <-  'Hawthorn'
ground <- schedules[season == sn & round == 'Round 2'  & team1 == t1 & team2 == t2, venue]
num_matches_team1 <- hfa_helper_dataset[season == sn & team == t1 & venue == ground,
                                                num_past_matches]
num_matches_team2 <- hfa_helper_dataset[season == sn & team == t2 & venue == ground,
                                                num_past_matches]
home_field_advantage <- home_field_advantage_coeff*(log(1+num_matches_team1)-log(1+num_matches_team2))
elo_diff <- updated_elo_ratings[team==t1,elo] - updated_elo_ratings[team==t2,elo] + home_field_advantage
p <- 1/(1+10^(-elo_diff/lambda))
logit_p <- log(p/(1-p))
predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) # 8.986679

