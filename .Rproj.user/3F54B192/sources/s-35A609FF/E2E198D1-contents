library(ProjectTemplate)
load.project()

sn <- 2016
team_list <- union(past_results[season==sn]$team1,past_results[season==season]$team2)

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(past_results[season==sn][grep('Round',round)], 
                                         initial_elo_ratings, 
                                         K = 25, 
                                         lambda = 400,
                                         autocorrelation_adjust = TRUE,
                                         margin_of_victory_adjust = TRUE,
                                         scoring_method = 'classic')

