return_finals_input_list_for_stan <- function(results_so_far,upcoming_schedule,
                                                                team_list,ladder_pos_and_finals_winners)
{
  
  # set default values
  final_8_fixed <- 0 # Do we have the final 8 fixed so far? Used in simulating the future matches
  semi_final_4_fixed <- 0  # Do we have the semi final 4 fixed so far? Used in simulating the future matches
  prelim_final_4_fixed <- 0 # Do we have the prelim final 4 fixed so far? Used in simulating the future matches
  final_2_fixed <- 0  # Do we have the final 2 fixed so far? Used in simulating the future matches
  premiership_team_fixed <- 0 # Do we have the premiership fixed so far? Used in simulating the 'future matches'
  final_8_team_names <- replicate(8,team_list[1]) 
  semi_final_4_team_names <- replicate(4,team_list[1]) 
  prelim_final_4_team_names <- replicate(4,team_list[1]) 
  final_2_team_names <- replicate(2,team_list[1]) 
  premiership_team_name <- team_list[1] 

  
  # finals 8 are fixed if in the results_so_far already has 'Qualifying Final/Elimination Final' result 
  # or the first match in the upcoming_schedule is a QF/EF match
  has_match_played_in_past <- ifelse(nrow(results_so_far[grep('Qualifying Final|Elimination Final',round_full_desc)]) == 0,
                                     0,1)
  is_match_immediately_in_future <- ifelse(nrow(upcoming_schedule[1][grep('Qualifying Final|Elimination Final',round_full_desc)]) == 0,
                                           0,1)
  if (has_match_played_in_past | is_match_immediately_in_future){
    final_8_fixed <- 1
    final_8_team_names <- ladder_pos_and_finals_winners[, paste0('ladder_position_',1:8), with = F]
  }
  
  # semi final 4 are fixed if in the results_so_far already has 'Semi Final' result 
  # or the first match in the upcoming_schedule is a SF match
  has_match_played_in_past <- ifelse(nrow(results_so_far[grep('Semi Final',round_full_desc)]) == 0,
                                     0,1)
  is_match_immediately_in_future <- ifelse(nrow(upcoming_schedule[1][grep('Semi Final',round_full_desc)]) == 0,
                                           0,1)
  if (has_match_played_in_past | is_match_immediately_in_future){
    semi_final_4_fixed <- 1
    
    # loser of QF1
    semi_final_4_team_names[1] <- setdiff(ladder_pos_and_finals_winners[,.(ladder_position_1,ladder_position_4)],
                                ladder_pos_and_finals_winners[,Qualifying_Final_1_winner])
    
    # winner of EF1
    semi_final_4_team_names[2] <- ladder_pos_and_finals_winners[,Elimination_Final_1_winner]
    
    # loser of QF2
    semi_final_4_team_names[3] <- setdiff(ladder_pos_and_finals_winners[,.(ladder_position_2,ladder_position_3)],
                                ladder_pos_and_finals_winners[,Qualifying_Final_2_winner])
    
    # winner of EF1
    semi_final_4_team_names[4] <- ladder_pos_and_finals_winners[,Elimination_Final_2_winner]
    
    # winner QF1
    prelim_final_4_team_names[1] <-  ladder_pos_and_finals_winners[,Qualifying_Final_1_winner]
    
    # winner QF2
    prelim_final_4_team_names[3] <-  ladder_pos_and_finals_winners[,Qualifying_Final_2_winner]
    
    semi_final_team_names <- c(semi_final_team1,semi_final_team2,semi_final_team3,semi_final_team4)
    semi_final_4_team_ids_input <- as.array(sapply(semi_final_team_names, function(team){which(team == team_list)}))
  }
  
  # preliminary final 4 are fixed if in the results_so_far already has 'Semi Final' result 
  # or the first match in the upcoming_schedule is a SF match
  has_match_played_in_past <- ifelse(nrow(results_so_far[grep('Preliminary Final',round_full_desc)]) == 0,
                                     0,1)
  is_match_immediately_in_future <- ifelse(nrow(upcoming_schedule[1][grep('Preliminary Final',round_full_desc)]) == 0,
                                           0,1)
  if (has_match_played_in_past | is_match_immediately_in_future){
    prelim_final_4_fixed <- 1
    
    # winner of SF2
    prelim_final_4_team_names[2] <-  ladder_pos_and_finals_winners[,Semi_Final_2_winner]
    
    # winner of SF1
    prelim_final_4_team_names[4] <-  ladder_pos_and_finals_winners[,Semi_Final_1_winner]
    
  }
  
  # final 2 are fixed if in the results_so_far already has 'Grand Final' result 
  # or the first match in the upcoming_schedule is a Grand Final match
  has_match_played_in_past <- ifelse(nrow(results_so_far[grep('Grand Final',round_full_desc)]) == 0,
                                     0,1)
  is_match_immediately_in_future <- ifelse(nrow(upcoming_schedule[1][grep('Grand Final',round_full_desc)]) == 0,
                                           0,1)
  if (has_match_played_in_past | is_match_immediately_in_future){
    final_2_fixed <- 1
    
    # winner PF1
    final_2_team_names[1] <-  ladder_pos_and_finals_winners[,Preliminary_Final_1_winner]
    
    # winner of PF2
    final_2_team_names[2] <-  ladder_pos_and_finals_winners[,Preliminary_Final_2_winner]
    
  }
  
  # Premiership team is fixed if the results_so_far already has 'Grand Final' result 
  if (has_match_played_in_past){
    premiership_team_fixed <- 1
    premiership_team_name <- ladder_pos_and_finals_winners[,Grand_Final_winner]
    
  }
  
  # get the team ids
  final_8_team_ids_input <- as.array(sapply(final_8_team_names, function(team){which(team == team_list)}))
  prelim_final_4_team_ids_input <- as.array(sapply(prelim_final_4_team_names, function(team){which(team == team_list)}))
  final_2_team_ids_input <- as.array(sapply(final_2_team_names, function(team){which(team == team_list)}))
  premiership_team_id_input <- which(premiership_team_name == team_list) 
  
  # collate the results in a list
  results <- list( 
    final_8_fixed = final_8_fixed,
    final_8_team_ids_input = as.vector(final_8_team_ids_input),
    semi_final_4_team_ids_input = as.vector(semi_final_4_team_ids_input),
    prelim_final_4_team_ids_input = as.vector(prelim_final_4_team_ids_input),
    final_2_team_ids_input = as.vector(final_2_team_ids_input),
    premiership_team_id_input = premiership_team_id_input)
  
  return(results)
}