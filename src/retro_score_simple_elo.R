
library(ProjectTemplate)
load.project()



# global settings ---------------------------------------------------------

K <-  25#10 #25
lambda <- 400 # 700 # 400
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
home_field_advantage_adjust <- TRUE
scoring_method <- 'classic'
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- 15

# current seasn
sn_current <- 2018
# how many rounds to retro score in the current seasn
rounds_so_far <- 6


# calculate elo by using the data after 2016 ----------------------------------------

sn_begin <- 2016
team_list <- union(past_results[season>=sn_begin]$team1,past_results[season>=sn_begin]$team2)

# get the elo ratings using previous seasons
results <- copy(past_results[season >= sn_begin & season < sn_current])
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

elo_ratings <- copy(updated_elo_ratings$ratings)
# regress to the mean at the beginning when the current season starts
elo_ratings[, elo := elo*reg_to_mean_factor + 1500*(1-reg_to_mean_factor)]
round_prediction <- data.table(season = numeric(0), round = character(0),
                               team1 = character(0), team2 = character(0), 
                               team1_win_prob = numeric(0))
for (rnd in 1:rounds_so_far){
  # score the round using the most recent elo ratings
  schedule_for_the_round <- past_results[season == sn_current & round == paste0('Round ',rnd)]
  
  for (match in 1:nrow(schedule_for_the_round)) {
    team_1 <- schedule_for_the_round[match,team1]
    team_2 <-  schedule_for_the_round[match,team2]
    ground <- past_results[season == sn_current & round == paste0('Round ',rnd) & team1 == team_1 & team2 == team_2, venue]
    num_past_matches_team_1 <- hfa_helper_dataset[season == sn_current-1 & venue == ground & team == team_1, num_past_matches]
    num_past_matches_team_2 <- hfa_helper_dataset[season == sn_current-1 & venue == ground & team == team_2, num_past_matches]
    home_field_advantage <- home_field_advantage_coeff*(log(1+num_past_matches_team_1)-log(1+num_past_matches_team_2))
    elo_diff <- elo_ratings[team==team_1,elo] - elo_ratings[team==team_2,elo] 
    if (home_field_advantage_adjust) {
      elo_diff <- elo_diff + home_field_advantage
    }
    p <- 1/(1+10^(-elo_diff/lambda))
    
    round_prediction <- rbind(round_prediction, data.table(season = sn_current,
                                                           round = paste0('Round ',rnd),
                                                           team1 = team_1, team2 = team_2,
                                                           team1_win_prob = p ))
  }
  # update elo
  results <- copy(past_results[season == sn_current & round == paste0('Round ',rnd)])
  updated_elo_ratings <- return_elo_scores(results = results, 
                                           initial_ratings = elo_ratings, 
                                           home_field_advantage_stats = hfa_helper_dataset,
                                           K = K, 
                                           lambda = lambda,
                                           autocorrelation_adjust = autocorrelation_adjust,
                                           margin_of_victory_adjust = margin_of_victory_adjust,
                                           home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                           home_field_advantage_coeff = home_field_advantage_coeff,
                                           scoring_method = scoring_method,
                                           reg_to_mean_factor = 1) # within season no need to regress
  # to mean
  elo_ratings <- updated_elo_ratings$ratings
     
}

# save retro scores 
round_prediction[, model := 'simple_elo']
saveRDS(round_prediction, 'data/retro_score_simple_elo.RDS')
