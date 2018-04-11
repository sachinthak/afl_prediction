# This file contains ELO rating related functions

# evaluating the overall result based only on win/loose/draw without considering the margins
scoring_evaluation_classical <- function(score_diff)
{
  if (score_diff > 0) {
    res <- 1
  } else if (score_diff < 0){
    res <- 0
  } else {
    res <- 0.5
  }
  return(res)
}


# evaluating the overall result after considering the individual points
scoring_evaluation_point_based <- function(score_1, score_2)
{
  if(is.null(score_1) | is.null(score_2)){
    stop('Point based scoring evaluation requires individual scores to be provided.')
  }
  res <- (score_1 + 1)/(score_1 + score_2 + 2)
  return(res)
}

# update the ELO scores
update_elo <- function(elo_rating_1, elo_rating_2, 
                       score_1 = NULL, score_2 = NULL,
                       score_diff = NULL, 
                       K = 25, lambda = 400,
                       home_field_advantage = 0,
                       autocorrelation_adjust = FALSE,
                       margin_of_victory_adjust = FALSE,
                       scoring_method = c('classic','point_based')[1])
{
  
  if (is.null(score_diff)){
    if(is.null(score_1) | is.null(score_2)){
      stop('Either the individual team scores should be provided or the score_diff should be set.')
    }
    score_diff <- score_1 - score_2
  }
  
  # evaluate the result
  if (scoring_method == 'classic'){
    s <- scoring_evaluation_classical(score_diff)
  } else if (scoring_method == 'point_based'){
    s <- scoring_evaluation_point_based(score_1,score_2)
  } else{
    s <- scoring_evaluation_classical(score_diff)
  }
  
  # evaluate the expected result
  diff <- (elo_rating_1 - elo_rating_2 + home_field_advantage)/lambda
  u <- 1/(1+10^(-diff))
  
  k_adj <- K
  if (margin_of_victory_adjust == TRUE){
    
    k_adj <- k_adj*log(abs(score_diff)+1)
    
    # adjustment for the fact that higher rated team tends to win more with higher margin.
    # only applicable if margin of victory adjustment is applied
    if (autocorrelation_adjust == TRUE) { # this needs further thinking.formula from 538
      # calculate the ELO difference of winner and loser
      if (score_diff >=0){
        elo_diff <- elo_rating_1 - elo_rating_2
      } else {
        elo_diff <- elo_rating_2 - elo_rating_1
      }
      k_adj <- k_adj*2.2/(elo_diff*.001 + 2.2)
    }
  }
  
  elo_scores <- c(elo_rating_1 + k_adj*(s-u), elo_rating_2 - k_adj*(s-u))
    
  return(elo_scores)
}

# return the elo ratings after  game results and also some performance metrics
return_elo_scores <- function(results, initial_ratings, 
                              home_field_advantage_stats = NULL, K, lambda,
                              autocorrelation_adjust = FALSE,
                              margin_of_victory_adjust = FALSE,
                              home_field_advantage_adjust = FALSE,
                              home_field_advantage_coeff = 6,
                              scoring_method = c('classic','point_based')[1],
                              reg_to_mean_factor = 0.75)
{
  ratings <- copy(initial_ratings)
  seasons <- sort(results[,unique(season)]) 
  num_success_predictions <- 0
  for (sn in seasons){
        season_results <- results[season == sn]
        n_games <-  nrow(season_results)
      
        
        #regression to the mean adjustment
        ratings[,elo := elo*reg_to_mean_factor + 1500*(1-reg_to_mean_factor)]  
        
        
        # loop over each game and update ratings
        for (game in 1:n_games){
          
          ground <- season_results[game,venue]
          
          team_1 <- season_results[game,team1]
          team_2 <- season_results[game,team2]
          score_1 <- season_results[game,score_team1]
          score_2 <- season_results[game,score_team2]
          
          elo_team_1 <- ratings[team == team_1,elo]
          elo_team_2 <- ratings[team == team_2,elo]
          
          home_field_advantage <- 0
          if (!is.null(home_field_advantage_stats) & home_field_advantage_adjust == T){
            num_matches_team1 <- home_field_advantage_stats[season == sn & team == team_1 & venue == ground,
                                                            num_past_matches]
            num_matches_team2 <- home_field_advantage_stats[season == sn & team == team_2 & venue == ground,
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
          
          ratings[team == team_1, elo := updated_elo[1]]
          ratings[team == team_2, elo := updated_elo[2]]
          
          # keep track of the successful predictions for performance tuning
          if ((elo_team_1-elo_team_2)*(score_1-score_2) >= 0){
            num_success_predictions <- num_success_predictions + 1
          }
        }  
  }
  return(list(ratings = ratings, head_to_head_accuracy = num_success_predictions/nrow(results)))
}