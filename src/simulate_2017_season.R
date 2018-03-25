
# use 2016 season to calculate ELO at beginning of season  ------------------------------------------------

library(ProjectTemplate)
load.project()

sn <- 2015
team_list <- union(past_results[season==sn]$team1,past_results[season==season]$team2)
schedule = schedules[season == sn]

results <- past_results[season==sn]

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(results = results, 
                                         initial_elo_ratings, 
                                         K = 25, 
                                         lambda = 400,
                                         autocorrelation_adjust = FALSE,
                                         margin_of_victory_adjust = FALSE,
                                         scoring_method = 'classic')
