# simulate the outcome of a match
simulate_match <- function(elo_team_1,elo_team_2,lambda, point_spread_regression = NULL)
{
  if (is.null(point_spread_regression)) {
    diff <- elo_team_1 - elo_team_2
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
                                results_so_far, point_spread_regression,
                                scoring_method = 'classic',
                                autocorrelation_adjust = FALSE,
                                margin_of_victory_adjust = FALSE,
                                point_spread_regression = NULL,
                                n_sim_tournaments = 100)
{
  matches_played_so_far <- nrow(results_so_far)
  fixed_results <- results_so_far[,.(team1,team2,score_team1,score_team2)]
  
  simulate_elo <- copy(elo_ratings)
  random_results <- data.table(team1=character(0),team2=character(0),
                               score_team1=numeric(0),score_team2=numeric(0))
  for (match in (matches_played_so_far+1):nrow(schedule)){
    team_1 <- schedule[match,team1]
    team_2 <- schedule[match,team2]
  
    
    elo_team_1 <- simulate_elo[team == team_1,elo]
    elo_team_2 <- simulate_elo[team == team_2,elo]
    
    game_results <- play_single_match_and_return_winner_and_updated_elo(team_1 = elo_team_1,
                                                                        team_2 = elo_team_2,
                                                                        elo_ratings = elo_ratings,
                                                                        K = K, lambda = lambda,
                                                                        autocorrelation_adjust = autocorrelation_adjust,
                                                                        margin_of_victory_adjust = margin_of_victory_adjust,
                                                                        scoring_method = scoring_method,
                                                                        point_spread_regression = point_spread_regression)
    
    
    random_results <- rbind(random_results, data.table(team1=team_1,team2=team_2,
                            score_team1=game_results$score_1,score_team2=score_1))
    # update elo
    simulate_elo <- game_results$updated_elo_ratings
  }
  
  complete_results <- rbind(fixed_results,random_results)
  points_table <- calculate_team_points(complete_results)
  top_8 <- points_table[order(-points)][1:8] 
  
  play_finals_series(top_8, simulate_elo)
}


# play a single match and return a list with a winner and the updated elo
play_single_match_and_return_winner_and_updated_elo <- function(team_1,team_2,
                                                                elo_ratings, K, lambda, 
                                                                autocorrelation_adjust = FALSE,
                                                                margin_of_victory_adjust = FALSE,
                                                                scoring_method = 'classic',
                                                                point_spread_regression = NULL)
{
  
  elo_team_1 <- elo_ratings[team == team_1,elo]
  elo_team_2 <- elo_ratings[team == team_2,elo]
  
  # simulate a match
  game_results <- simulate_match(elo_team_1,elo_team_2,lambda, point_spread_regression)
  
  # get the updated elo ratings for the two teams based on the results
  new_elo <- update_elo(elo_team_1, elo_team_2, 
                        score_1 =  game_results$score_team_1,
                        score_2 = game_results$score_team_2,
                        score_diff = game_results$score_diff,
                        K = K, lambda = lambda,
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
play_finals_series <- function(finalists, elo, K, lambda, scoring_method = 'classic')
{
 
   # play QF 1
  first <- finalists[1,team]
  fourth <- finalists[4,team]
  
  elo_first <- elo[team == first, elo]
  elo_fourth <- elo[team == fourth, elo]
  winner <- simulate_match(elo_first,elo_fourth,lambda)
  if (winner == 1) {
    score_first <- 1
    score_fourth <- 0
    qf1_winner <- first
    qf1_loser <- fourth
  }else {
    score_first <- 0
    score_fourth <- 1
    qf1_winner <- fourth
    qf1_loser <- first
  }
  new_elo <- update_elo(elo_first, elo_fourth, 
                        score_first, score_fourth,
                        K = K, lambda = lambda,
                        autocorrelation_adjust = FALSE,
                        margin_of_victory_adjust = FALSE,
                        scoring_method = 'classic')
  elo[team == first, elo := new_elo[1]]
  elo[team == fourth, elo := new_elo[2]]
  
  
  # play QF 2
  second <- finalists[2,team]
  third <- finalists[3,team]
  elo_second <- elo[team == second, elo]
  elo_third <- elo[team == third, elo]
  winner <- simulate_match(elo_second,elo_third,lambda)
  if (winner == 1) {
    score_second <- 1
    score_third <- 0
    qf1_winner <- second
    qf1_loser <- third
  }else {
    score_second <- 0
    score_third <- 1
    qf1_winner <- third
    qf1_loser <- second
  }
  new_elo <- update_elo(elo_second, elo_third, 
                        score_second, score_third,
                        K = K, lambda = lambda,
                        autocorrelation_adjust = FALSE,
                        margin_of_victory_adjust = FALSE,
                        scoring_method = 'classic')
  elo[team == second, elo := new_elo[1]]
  elo[team == third, elo := new_elo[2]]
  
  # play EF 1
  fifth <- finalists[5,team]
  eigth <- finalists[8,team]
  elo_fifth <- elo[team == fifth, elo]
  elo_eigth <- elo[team == eigth, elo]
  winner <- simulate_match(elo_fifth,elo_eigth,lambda)
  if (winner == 1) {
    score_fifth <- 1
    score_eigth <- 0
    ef1_winner <- fifth
    ef1_loser <- eigth
  }else {
    score_fifth <- 0
    score_eigth <- 1
    ef1_winner <- eigth
    ef1_loser <- fifth
  }
  new_elo <- update_elo(elo_fifth, elo_eigth, 
                        score_fifth, score_eigth,
                        K = K, lambda = lambda,
                        autocorrelation_adjust = FALSE,
                        margin_of_victory_adjust = FALSE,
                        scoring_method = 'classic')
  elo[team == fifth, elo := new_elo[1]]
  elo[team == eigth, elo := new_elo[2]]
  
  # play EF 2
  sixth <- finalists[6,team]
  seventh <- finalists[7,team]
  elo_sixth <- elo[team == sixth, elo]
  elo_seventh <- elo[team == seventh, elo]
  winner <- simulate_match(elo_sixth,elo_seventh,lambda)
  if (winner == 1) {
    score_sixth <- 1
    score_seventh <- 0
    ef1_winner <- sixth
    ef1_loser <- seventh
  }else {
    score_sixth <- 0
    score_seventh <- 1
    ef1_winner <- seventh
    ef1_loser <- sixth
  }
  new_elo <- update_elo(elo_sixth, elo_seventh, 
                        score_sixth, score_seventh,
                        K = K, lambda = lambda,
                        autocorrelation_adjust = FALSE,
                        margin_of_victory_adjust = FALSE,
                        scoring_method = 'classic')
  elo[team == sixth, elo := new_elo[1]]
  elo[team == seventh, elo := new_elo[2]]
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
