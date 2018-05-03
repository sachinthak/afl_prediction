
generate_modelling_data <- function(results, initial_elo_ratings,
                                    home_field_advantage_stats = NULL,
                                    K, lambda,autocorrelation_adjust = TRUE,
                                    margin_of_victory_adjust = TRUE,
                                    home_field_advantage_adjust = FALSE,                                         
                                    home_field_advantage_coeff = 6,
                                    scoring_method = 'classic',reg_to_mean_factor = 0.75)
{
  seasons <- sort(results[,unique(season)])
  elo_ratings <- copy(initial_elo_ratings)
  
  summary_data <- list() #data.table(season = character(0), round = character(0), team_1 = character(0), team_2 = character(0),
             #score_1 = character(0), score_2 = character(0), 
             #pre_game_elo_team_1 = numeric(0), pre_game_elo_team_2 = numeric(0) )
  
  for (sn in seasons){
    
    # regression to the mean
    elo_ratings[,elo := elo*reg_to_mean_factor + 1500*(1-reg_to_mean_factor)]  
    
    season_results <-  results[season == sn]
    
    n_games <-  nrow(season_results)
    
    # loop over each game and update ratings
    for (game in 1:n_games){
      
      ground <- season_results[game,venue]
      round <- season_results[game,round]
      
      team_1 <- season_results[game,team1]
      team_2 <- season_results[game,team2]
      score_1 <- season_results[game,score_team1]
      score_2 <- season_results[game,score_team2]
      
      elo_team_1 <- elo_ratings[team == team_1,elo]
      elo_team_2 <- elo_ratings[team == team_2,elo]
      
      home_field_advantage <- 0
      if (!is.null(home_field_advantage_stats) & home_field_advantage_adjust == T){
        num_matches_team1 <- home_field_advantage_stats[season == sn-1 & team == team_1 & venue == ground,
                                                        num_past_matches]
        num_matches_team2 <- home_field_advantage_stats[season == sn-1 & team == team_2 & venue == ground,
                                                        num_past_matches]
        home_field_advantage <- home_field_advantage_coeff*(log(1+num_matches_team1)-log(1+num_matches_team2))
      }
      
      updated_elo <- update_elo(elo_rating_1 = elo_team_1, 
                                elo_rating_2 = elo_team_2, 
                                score_1 = score_1, score_2 = score_2,
                                K = K,lambda = lambda,
                                autocorrelation_adjust = autocorrelation_adjust,
                                margin_of_victory_adjust = margin_of_victory_adjust,
                                home_field_advantage = home_field_advantage,
                                scoring_method = scoring_method)
      
      elo_ratings[team == team_1, elo := updated_elo[1]]
      elo_ratings[team == team_2, elo := updated_elo[2]]
      
      summary_data[[paste0(sn,game)]] <-  data.table(season = sn, round = round, team_1 = team_1, team_2 = team_2,
                                       score_1 = score_1, score_2 = score_2, 
                                       pre_game_elo_team_1 = elo_team_1, pre_game_elo_team_2 = elo_team_2 )
    }  
  }
  summary_data <- rbindlist(summary_data)
  summary_data[, spread := ifelse(pre_game_elo_team_1 >= pre_game_elo_team_2, score_1 - score_2, score_2 - score_1)]
  summary_data[, elo_diff := abs(pre_game_elo_team_1-pre_game_elo_team_2)]
  
  return(summary_data)
}

simple_point_spread_regression <- function(results, initial_elo_ratings,
                                           home_field_advantage_stats = NULL,
                                           K, lambda, autocorrelation_adjust = TRUE,
                                           margin_of_victory_adjust = TRUE,
                                           home_field_advantage_adjust = FALSE,                                         
                                           home_field_advantage_coeff = 6,
                                           scoring_method = 'classic',reg_to_mean_factor = 0.75)
{
  modelling_data <- generate_modelling_data(results = results, initial_elo_ratings = initial_elo_ratings,
                                            home_field_advantage_stats = home_field_advantage_stats,
                                            K, lambda,autocorrelation_adjust = autocorrelation_adjust,
                                            margin_of_victory_adjust = margin_of_victory_adjust, 
                                            home_field_advantage_adjust = home_field_advantage_adjust,
                                            home_field_advantage_coeff = home_field_advantage_coeff,
                                            scoring_method = scoring_method, reg_to_mean_factor = reg_to_mean_factor)
  
  # it doesn't make sense to include a constant term 
  fit <- lm(formula = 'spread ~ 0 + elo_diff', data = modelling_data)
  
  return(fit)
}

simple_logit_point_spread_regression <- function(results, initial_elo_ratings,
                                                 home_field_advantage_stats = NULL,
                                                 K, lambda, autocorrelation_adjust = TRUE,
                                                 margin_of_victory_adjust = TRUE,
                                                 home_field_advantage_adjust = FALSE,                                         
                                                 home_field_advantage_coeff = 6,
                                                 scoring_method = 'classic',reg_to_mean_factor = 0.75)
{
  modelling_data <- generate_modelling_data(results = results, initial_elo_ratings = initial_elo_ratings,
                                            home_field_advantage_stats = home_field_advantage_stats,
                                            K, lambda,autocorrelation_adjust = autocorrelation_adjust,
                                            margin_of_victory_adjust = margin_of_victory_adjust, 
                                            home_field_advantage_adjust = home_field_advantage_adjust,
                                            home_field_advantage_coeff = home_field_advantage_coeff,
                                            scoring_method = scoring_method, reg_to_mean_factor = reg_to_mean_factor)
  
  modelling_data[, p := 1/(1+ 10^(-(elo_diff/lambda)))]
  modelling_data[, logit_p := log(p/(1-p))]
  # it doesn't make sense to include a constant term 
  fit <- lm(formula = 'spread ~ 0 + logit_p', data = modelling_data)
  
  return(fit)
}