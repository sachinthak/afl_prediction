populate_ladders_and_final_series_winners <- function(results_history)
{
  # restrict the results to just the regular rounds
  results <- results_history[grep('Round',round)] 
  
  # calculate for and against scores. Used for tie-breaking in the ranks when calculating the final 8
  for_against_scores_1 <- results[, .(for_score = sum(score_team1), against_score = sum(score_team2)), by = .(team1,season)]
  for_against_scores_2 <- results[, .(for_score = sum(score_team2), against_score = sum(score_team1)), by = .(team2,season)]
  setnames(for_against_scores_1,'team1','team')
  setnames(for_against_scores_2,'team2','team')
  for_against_scores <- rbind(for_against_scores_1,for_against_scores_2)
  for_against_scores <- for_against_scores[, .(for_score = sum(for_score), 
                                               against_score = sum(against_score)), by = .(team,season)]
  for_against_scores[, for_against_ratio := for_score/against_score]
  
  # reorder according to the team_list
  #for_against_scores <- for_against_scores[sapply(team_list, function(t){which(t == team)}),] 
  #for_against_ratio <- for_against_scores$for_score/for_against_scores$against_score 
  
  team_list <- union(unique(results$team1),unique(results$team2))
  # calculate team points for the ladder board
  points_ladder <- lapply(team_list, function(team){
    # win is a 4 and draw is a 2
    s1 <- results[team1 == team, 
                             sum(ifelse(score_team1 >  score_team2,4,
                                        ifelse(score_team1 == score_team2,2,0))), by = season]
    s2 <- results[team2 == team, 
                             sum(ifelse(score_team2 >  score_team1,4,
                                        ifelse(score_team1 == score_team2,2,0))), by = season]
    s <- s1[s2, on = 'season']
    setnames(s,names(s),c('season','s1','s2'))
    s[, points := s1 + s2]
    s[, team := team]
    return(s[, .(season,team,points)])
  })
  
  points_ladder <- rbindlist(points_ladder)
  
  # merge the for_against_scores
  points_ladder <- for_against_scores[,.(season,team,for_against_ratio)][points_ladder, on = c('season','team')]
  setorder(points_ladder,season,-points,-for_against_ratio)
  
  # add ladder position
  points_ladder <- points_ladder[, ladder_pos := 1:.N, by = season]
  
  # there was an exception in 2013 where essendon was moved from ladder pos 8 to 9 and carlton
  # entered as 8 and Port Adelaide as 7
  if (nrow(points_ladder[season == 2013 & team == 'Carlton'])==1){
    points_ladder[season == 2013 & team == 'Carlton', ladder_pos := 8]
    points_ladder[season == 2013 & team == 'Essendon', ladder_pos := 9]
    points_ladder[season == 2013 & team == 'Port Adelaide', ladder_pos := 7]
  }  
  
  # we are only interested in the first 8
  points_ladder <- points_ladder[ladder_pos <= 8]
   
  
  
 # identify finals series match (i.e. QF1 or QF2)    
  finals_week1_results <- results_history[grep('Elimination Final|Qualifying Final',round),]
  finals_week1_results <- points_ladder[finals_week1_results, on = c('season','team==team1')]
  
  finals_week1_results[ladder_pos %in% c(1,4), round_full_desc := 'Qualifying Final 1']
  finals_week1_results[ladder_pos %in% c(2,3), round_full_desc := 'Qualifying Final 2']
  finals_week1_results[ladder_pos %in% c(5,8), round_full_desc := 'Elimination Final 1']
  finals_week1_results[ladder_pos %in% c(6,7), round_full_desc := 'Elimination Final 2']
  
  
  finals_week1_results[, winner := ifelse(score_team1 > score_team2, team,team2)]
  
  finals_week1_results <- dcast.data.table(finals_week1_results, formula = season ~ round_full_desc, value.var = 'winner')
  # rename some columns
  col_names_to_rename <- names(finals_week1_results)[2:5]
  setnames(finals_week1_results,col_names_to_rename,
           paste0(gsub(pattern = ' ',replacement = '_',col_names_to_rename),'_winner')  )
  
  # filter semi finals results
  semifinal_results <- results_history[grep('Semi Final',round),]
  
  # identify semi final 1 and semi final 2
  semifinal_results <- finals_week1_results[semifinal_results, on = 'season']
  semifinal_results[, round_full_desc := ifelse(team1 == Elimination_Final_1_winner | team2 == Elimination_Final_1_winner,
                                                'Semi_Final_1','Semi_Final_2')]
  
  semifinal_results[ , winner := ifelse(score_team1 > score_team2, team1,team2)]
  semifinal_results <- dcast.data.table(semifinal_results, formula = season ~ round_full_desc, value.var = 'winner')
  setnames(semifinal_results, c('Semi_Final_1','Semi_Final_2'), c('Semi_Final_1_winner','Semi_Final_2_winner')) 

  # identify preliminary final 1 and 2
  prelimfinal_results <- results_history[grep('Preliminary Final',round),]
  prelimfinal_results <- semifinal_results[prelimfinal_results, on = 'season']
  prelimfinal_results[, round_full_desc := ifelse(team1 == Semi_Final_1_winner | team2 == Semi_Final_1_winner,
                                                'Preliminary_Final_2','Preliminary_Final_1')]
  prelimfinal_results[ , winner := ifelse(score_team1 > score_team2, team1,team2)]
  prelimfinal_results <- dcast.data.table(prelimfinal_results, formula = season ~ round_full_desc, value.var = 'winner')

  
  premiership_results <- results_history[grep('Grand Final',round),]
  premiership_results[ , winner := ifelse(score_team1 > score_team2, team1,team2)]
  premiership_results <- premiership_results[, .(season, Grand_Final_winner = winner)]
  
  # widen the points ladder
  points_ladder <- dcast.data.table(points_ladder, formula = season ~ ladder_pos, value.var = 'team')
  setnames(points_ladder,as.character(1:8), paste0('ladder_position_',1:8))
  
  # merge all the results
  finals_results <- finals_week1_results[points_ladder, on = 'season']
  finals_results <- semifinal_results[finals_results, on = 'season']
  finals_results <- prelimfinal_results[finals_results, on = 'season']
  finals_results <- premiership_results[finals_results, on = 'season']
  
  return(finals_results)
}
