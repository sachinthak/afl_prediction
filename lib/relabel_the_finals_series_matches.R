#' Title
#'
#' @param ladder_pos_and_finals_winners A table with each season as a row and columns such as ladder_position_1,semi_final_1_winner, etc.
#' @param match_schedule A table with match schedules (each row is a match)
#'
#' @return Same table as match_schedule except the column round_full_desc correctly labeled for finals series. i.e. Preliminary Final 1 
#' (opposed to Preliminary Final 2)
relabel_the_finals_series_matches <- function(ladder_pos_and_finals_winners, match_schedule)
{
  col_names <- names(match_schedule)
  
  if (class(match_schedule$season) == 'character') {
    ladder_pos_and_finals_winners[, season := as.character(season)]
  } else {
    ladder_pos_and_finals_winners[, season := as.numeric(season)]
  }
  match_schedule <- ladder_pos_and_finals_winners[match_schedule, on = 'season']
  
  # label QF1
  match_schedule[grepl('Elimination Final|Qualifying Final',round) & (team1 == ladder_position_1 | team2 == ladder_position_1), 
                                                           c('round_full_desc','round'):=.('Qualifying Final 1','Qualifying Final')]
  # label QF2
  match_schedule[grepl('Elimination Final|Qualifying Final',round) & (team1 == ladder_position_2 | team2 == ladder_position_2), 
                 c('round_full_desc','round'):=.('Qualifying Final 2','Qualifying Final')]
  
  # label EF1
  match_schedule[grepl('Elimination Final|Qualifying Final',round) & (team1 == ladder_position_5 | team2 == ladder_position_5), 
                 c('round_full_desc','round'):=.('Elimination Final 1','Elimination Final')]
  # label EF2
  match_schedule[grepl('Elimination Final|Qualifying Final',round) & (team1 == ladder_position_6 | team2 == ladder_position_6), 
                 c('round_full_desc','round'):=.('Elimination Final 2','Elimination Final')]
  # label SF1
  match_schedule[grepl('Semi Final',round) & (team1 == Elimination_Final_1_winner | team2 == Elimination_Final_1_winner), 
                 round_full_desc:='Semi Final 1']
  
  # label SF2
  match_schedule[grepl('Semi Final',round) & (team1 == Elimination_Final_2_winner | team2 == Elimination_Final_2_winner), 
                 round_full_desc:='Semi Final 2']
  
  # label PF1
  match_schedule[grepl('Preliminary Final',round) & (team1 == Qualifying_Final_1_winner | team2 == Qualifying_Final_1_winner), 
                 round_full_desc:='Preliminary Final 1']
  
  # label PF2
  match_schedule[grepl('Preliminary Final',round) & (team1 == Qualifying_Final_2_winner | team2 == Qualifying_Final_2_winner), 
                 round_full_desc:='Preliminary Final 2']
  
  return(match_schedule[, col_names, with = F])
  
}
