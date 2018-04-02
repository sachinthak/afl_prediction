# simulate the outcome of a match
simulate_match <- function(elo_team_1,elo_team_2,home_field_advantage = 0,
                           lambda, point_spread_regression = NULL)
{
  diff <- elo_team_1 - elo_team_2 + home_field_advantage
  if (is.null(point_spread_regression)) {
    p <- 1/(1+10^(-diff/lambda))
    sim_winner <- rbinom(n=1, size = 1 , prob = p)
    if (sim_winner == 1){
      score_team_1 <- 1
      score_team_2 <- 0
      score_diff <- 1
    } else {
      score_team_1 <- 0
      score_team_2 <- 1
      score_diff <- -1  
    }
  } else { 
    # simulate the winner from drawing point_spread_regression and observing the sign of the point spread.
    # this will be the better approach. but i need to get a good regression working.
  }
  
  result <- list(winner = sim_winner,
                 score_team_1 = score_team_1,
                 score_team_2 = score_team_2,
                 score_diff = score_diff) 
  return(result)
}
  
# simulate a tournament for a given schedule and results so far.
# function assumes that the results_so_far and schedule allign, meaning
# that the n^{th} row in the result corresponds to the n^{th} match in the schedule.
# also currently this supports only results_so_far to contain only the preliminary rounds. 
simulate_tournament <- function(schedule, K, lambda, elo_ratings, 
                                results_so_far,
                                scoring_method = 'classic',
                                home_field_advantage_stats = NULL,
                                home_field_advantage_adjust = FALSE,                                         
                                home_field_advantage_coeff = 6,
                                autocorrelation_adjust = FALSE,
                                margin_of_victory_adjust = FALSE,
                                point_spread_regression = NULL,
                                n_sim_tournaments = 100)
{
  
  sn = schedule[1,season]
  
  matches_played_so_far <- nrow(results_so_far)
  fixed_results <- results_so_far[,.(team1,team2,score_team1,score_team2)]
  
  # data structures to store monte carlo counts
  teams <- unique(c(schedule$team1,schedule$team2))
  final_eight_frequency_table <- data.table(team = teams, count = 0) 
  final_four_frequency_table <- data.table(team = teams, count = 0) 
  final_two_frequency_table <- data.table(team = teams, count = 0) 
  championship_frequency_table <- data.table(team = teams, count = 0) 
  
  for (n_sim in 1:n_sim_tournaments){
      cat('tournament ', n_sim, '\n')
      simulate_elo <- copy(elo_ratings)
      random_results <- data.table(team1=character(0),team2=character(0),
                                   score_team1=numeric(0),score_team2=numeric(0))
      for (match in (matches_played_so_far+1):nrow(schedule)){
       
        ground <- schedule[match,venue]
        
        team_1 <- schedule[match,team1]
        team_2 <- schedule[match,team2]
      
        elo_team_1 <- simulate_elo[team == team_1,elo]
        elo_team_2 <- simulate_elo[team == team_2,elo]
        
        game_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = team_1,
                                                                            team_2 = team_2,
                                                                            elo_ratings = elo_ratings,
                                                                            K = K, lambda = lambda,
                                                                            autocorrelation_adjust = autocorrelation_adjust,
                                                                            margin_of_victory_adjust = margin_of_victory_adjust,
                                                                            scoring_method = scoring_method,
                                                                            ground = ground,
                                                                            home_field_advantage_stats = home_field_advantage_stats[season == sn],
                                                                            home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                                                            home_field_advantage_coeff = home_field_advantage_coeff,
                                                                            point_spread_regression = point_spread_regression)
        
        
        random_results <- rbind(random_results, data.table(team1=team_1,team2=team_2,
                                score_team1=game_results$score_1,score_team2=game_results$score_2))
        # update elo
        simulate_elo <- game_results$updated_elo_ratings
      }
      
      # play final series
      complete_results <- rbind(fixed_results,random_results)
      points_table <- calculate_team_points(complete_results)
      top_8 <- points_table[order(-points)][1:8] 
      
      final_series_results <- play_finals_series(finalists = top_8, elo = simulate_elo, K = K, lambda = lambda,
                         scoring_method = scoring_method, autocorrelation_adjust = autocorrelation_adjust,
                         margin_of_victory_adjust = margin_of_victory_adjust,
                         point_spread_regression = point_spread_regression)
      # update counts
      final_eight_frequency_table[team %in% final_series_results$final_eight, count := count + 1 ]
      final_four_frequency_table[team %in% final_series_results$final_four, count := count + 1 ]
      final_two_frequency_table[team %in% final_series_results$final_two, count := count + 1 ]
      championship_frequency_table[team %in% final_series_results$champion, count := count + 1]
  }
 
  # convert to probabilities
  final_eight_frequency_table[, prob := count/n_sim_tournaments]
  final_four_frequency_table[, prob := count/n_sim_tournaments]
  final_two_frequency_table[, prob := count/n_sim_tournaments]
  championship_frequency_table[, prob := count/n_sim_tournaments]
  
  # return results
  return(list(final_eight_frequency_table = final_eight_frequency_table,
              final_four_frequency_table = final_four_frequency_table,
              final_two_frequency_table = final_two_frequency_table,
              championship_frequency_table = championship_frequency_table))
}


# play a single match and return a list with a winner and the updated elo
play_single_match_and_return_winner_and_updated_elo <- function(team_1,team_2,
                                                                elo_ratings, K, lambda, 
                                                                autocorrelation_adjust = FALSE,
                                                                margin_of_victory_adjust = FALSE,
                                                                scoring_method = 'classic',
                                                                ground = '',
                                                                home_field_advantage_stats = NULL,
                                                                home_field_advantage_adjust = FALSE,                                         
                                                                home_field_advantage_coeff = 6,
                                                                point_spread_regression = NULL)
{
  
  elo_team_1 <- elo_ratings[team == team_1,elo]
  elo_team_2 <- elo_ratings[team == team_2,elo]
  
  home_field_advantage <- 0
  if (!is.null(home_field_advantage_stats) & home_field_advantage_adjust == T){
    num_matches_team1 <- home_field_advantage_stats[team == team_1 & venue == ground,
                                                    num_past_matches]
    num_matches_team2 <- home_field_advantage_stats[team == team_2 & venue == ground,
                                                    num_past_matches]
    home_field_advantage <- home_field_advantage_coeff*(log(1+num_matches_team1)-log(1+num_matches_team2))
  }
  
  # simulate a match
  game_results <- simulate_match(elo_team_1,elo_team_2,home_field_advantage,lambda, point_spread_regression)
  
  # get the updated elo ratings for the two teams based on the results
  new_elo <- update_elo(elo_team_1, elo_team_2, 
                        score_1 =  game_results$score_team_1,
                        score_2 = game_results$score_team_2,
                        score_diff = game_results$score_diff,
                        K = K, lambda = lambda,
                        home_field_advantage = home_field_advantage,
                        autocorrelation_adjust = autocorrelation_adjust,
                        margin_of_victory_adjust = margin_of_victory_adjust,
                        scoring_method = scoring_method)
  
  # updated the entire elo ratings table
  updated_elo_ratings <- copy(elo_ratings)
  updated_elo_ratings[team == team_1, elo := new_elo[1]]
  updated_elo_ratings[team == team_2, elo := new_elo[2]]
  
  # return results
  results <- list( winner = game_results$winner,
                   score_1 =  game_results$score_team_1,
                   score_2 = game_results$score_team_2,
                   score_diff = game_results$score_diff,
                   updated_elo_ratings = updated_elo_ratings)
}

# play the final series
play_finals_series <- function(finalists, elo, K, lambda, 
                               scoring_method = 'classic',
                               autocorrelation_adjust = FALSE,
                               margin_of_victory_adjust = FALSE,
                               point_spread_regression = NULL)
{
 
  elo_ratings <- elo
  
  # play QF 1
  qf1_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = finalists[1,team],
                                                                      team_2 = finalists[4,team],
                                                                      elo_ratings = elo_ratings,
                                                                      K = K, lambda = lambda,
                                                                      autocorrelation_adjust = autocorrelation_adjust,
                                                                      margin_of_victory_adjust = margin_of_victory_adjust,
                                                                      scoring_method = scoring_method,
                                                                      point_spread_regression = point_spread_regression)
  
  qf1_winner <- ifelse(qf1_results$winner == 1,finalists[1,team],finalists[4,team])
  qf1_loser <- ifelse(qf1_results$winner == 1,finalists[4,team],finalists[1,team])
  elo_ratings <- qf1_results$updated_elo_ratings
  
  
  # play QF 2
  qf2_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = finalists[2,team],
                                                                     team_2 = finalists[3,team],
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  qf2_winner <- ifelse(qf2_results$winner == 1,finalists[2,team],finalists[3,team])
  qf2_loser <- ifelse(qf2_results$winner == 1,finalists[3,team],finalists[2,team])
  elo_ratings <- qf2_results$updated_elo_ratings
  
  # play EF 1
  ef1_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = finalists[5,team],
                                                                     team_2 = finalists[8,team],
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  ef1_winner <- ifelse(ef1_results$winner == 1,finalists[5,team],finalists[8,team])
  elo_ratings <- ef1_results$updated_elo_ratings
  
  
  # play EF 2
  ef2_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = finalists[6,team],
                                                                     team_2 = finalists[7,team],
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  ef2_winner <- ifelse(ef2_results$winner == 1,finalists[6,team],finalists[7,team])
  elo_ratings <- ef2_results$updated_elo_ratings
  
  # play SF 1
  sf1_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = qf1_loser,
                                                                     team_2 = ef1_winner,
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  sf1_winner <- ifelse(sf1_results$winner == 1,qf1_loser,ef1_winner)
  elo_ratings <- sf1_results$updated_elo_ratings
  
  # play SF 2
  sf2_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = qf2_loser,
                                                                     team_2 = ef2_winner,
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  sf2_winner <- ifelse(sf2_results$winner == 1,qf2_loser,ef2_winner)
  elo_ratings <- sf2_results$updated_elo_ratings
 
  # play PF 1
  pf1_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = qf1_winner,
                                                                     team_2 = sf2_winner,
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  pf1_winner <- ifelse(pf1_results$winner == 1,qf1_winner,sf2_winner)
  elo_ratings <- pf1_results$updated_elo_ratings
  
  
  # play PF 2
  pf2_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = qf2_winner,
                                                                     team_2 = sf1_winner,
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  pf2_winner <- ifelse(pf2_results$winner == 1,qf2_winner,sf1_winner)
  elo_ratings <- pf2_results$updated_elo_ratings
  
  
  # play GF
  gf_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = pf1_winner,
                                                                     team_2 = pf2_winner,
                                                                     elo_ratings = elo_ratings,
                                                                     K = K, lambda = lambda,
                                                                     autocorrelation_adjust = autocorrelation_adjust,
                                                                     margin_of_victory_adjust = margin_of_victory_adjust,
                                                                     scoring_method = scoring_method,
                                                                     point_spread_regression = point_spread_regression)
  
  gf_winner <- ifelse(gf_results$winner == 1,pf1_winner,pf2_winner)
  elo_ratings <- gf_results$updated_elo_ratings
  
  results <- list( final_eight = finalists$team,
                   final_four = c(qf1_winner,qf2_winner,sf1_winner,sf2_winner),
                   final_two = c(pf1_winner,pf2_winner),
                   champion = gf_winner)
  
  return(results)
}


# calculate the points table for a given results table
calculate_team_points <- function(results)
{
  team_list <- union(results$team1,results$team2)
  points_table <- data.table(team = team_list, points = 0)
  
  for (game in 1:nrow(results)){
    team_1 <- results[game,team1]
    team_2 <- results[game,team2]
    
    score_1 <- results[game,score_team1]
    score_2 <- results[game,score_team2]
    
    if (score_1 > score_2){ # team 1 wins
      points_table[team==team_1, points := points + 4]
    }else if (score_2 > score_1) {# team 2 wins
      points_table[team==team_2, points := points + 4]
    }else { # draw
      points_table[team==team_1, points := points + 2]
      points_table[team==team_2, points := points + 2]
    }
  }
  
  return(points_table)
}
