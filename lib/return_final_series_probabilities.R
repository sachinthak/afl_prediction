#' Title Return the probabilities that each team will reach some milestones
#'
#' @param simulated_samples It is a matrix of posterior samples from the simulation
#' @param team_list List of teams (used to de-index team ids)
#'
#' @return A data table with probability of each team (rows) making it to the milestones (columns)
#'
#' 
return_final_series_probabilities <- function(simulated_samples, team_list)
{
  milestone_list <- c('premiership','final2','prelim_final4','semi_final4','final8')

  # calculate the probability of each team reaching the milestones
  final_series_probability_dat <- lapply(milestone_list, function(milestone){
    # find the relevant columns of the matrix of posterior samples for the milestone
    filtered_col_names <- grep(paste0(milestone,'_sim.*'),colnames(simulated_samples),value = T)
    probability_table <- table(simulated_samples[,filtered_col_names])/nrow(simulated_samples)
    probability_table <- as.data.table(probability_table,keep.rownames = T)
    setnames(probability_table,c('V1','N'),c('team_id','probability'))
    probability_table[, team_name := team_list[as.numeric(team_id)]]
    probability_table[, milestone := milestone]
  })

  final_series_probability_dat <- rbindlist(final_series_probability_dat)
  final_series_probability_dat <- dcast.data.table(final_series_probability_dat, 
                                                 formula = team_name ~ milestone,value.var =  'probability',fill = 0)
  setcolorder(final_series_probability_dat,c('team_name','final8','semi_final4','prelim_final4','final2','premiership'))
  setorder(final_series_probability_dat, -premiership)
  
  # For the sake of completeness append the teams that did not make it to the table (of course
  # with 0 as probabilities)
  unsuccessful_teams <- setdiff(team_list, unique(final_series_probability_dat$team_name))
  unsuccessful_teams_dat <- data.table(team_name = unsuccessful_teams,
                                       final8 = 0, semi_final4 = 0, prelim_final4 = 0,
                                       final2 = 0, premiership = 0)
  final_series_probability_dat <- rbind(final_series_probability_dat,unsuccessful_teams_dat)
  
  return(final_series_probability_dat)
}
