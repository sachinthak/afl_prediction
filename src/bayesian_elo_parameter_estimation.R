library(bayesplot)

curr_season <- 2022
final_8_fixed <- 0  # Do we have the final 8 fixed so far? Used in simulating the future matches
semi_final_4_fixed <- 0  # Do we have the semi final 4 fixed so far? Used in simulating the future matches
prelim_final_4_fixed <- 0 # Do we have the prelim final 4 fixed so far? Used in simulating the future matches
final_2_fixed <- 0  # Do we have the final 2 fixed so far? Used in simulating the future matches
premiership_team_fixed <- 0 # Do we have the premiership fixed so far? Used in simulating the 'future matches'

# dummy ids or actual teams ids to pass on to stan. (Later on the code these will be fixed if we actually know them)
final_8_team_ids_input <- 1:8 
semi_final_4_team_ids_input <- 1:4
prelim_final_4_team_ids_input <- 1:4
final_2_team_ids_input <- 1:2
premiership_team_id_input <- 1

results <- past_results[season == curr_season,]
future_schedule <- schedules[season == curr_season][!results, on = c('round','team1','team2')]

# generate input data to pass on to Stan ----------------------------------

round_ids <- results[, as.numeric(gsub(pattern = "Round (.*)",replacement = '\\1',x = round))]
futr_round_ids <- future_schedule[, as.numeric(gsub(pattern = "Round (.*)",replacement = '\\1',x = round))]
n_rounds <- uniqueN(round_ids)
futr_n_rounds <- uniqueN(futr_round_ids)
  
team_list <- sort(unique(union(results$team1,results$team2)))
n_teams <- length(team_list)

# get the ids of teams
team1_ids <- sapply(results$team1, function(team){get_team_id(team,team_list)})
team2_ids <- sapply(results$team2, function(team){get_team_id(team,team_list)})
futr_team1_ids <- sapply(future_schedule$team1, function(team){get_team_id(team,team_list)})
futr_team2_ids <- sapply(future_schedule$team2, function(team){get_team_id(team,team_list)})



# get scores
team1_score <- results$score_team1
team2_score <- results$score_team2
team1_win_indictr <- as.numeric(team1_score >= team2_score)


# calculate for and against scores. Used for tie-breaking in the ranks when calculating the final 8
for_against_scores_1 <- results[, .(for_score = sum(score_team1), against_score = sum(score_team2)), by = team1]
for_against_scores_2 <- results[, .(for_score = sum(score_team2), against_score = sum(score_team1)), by = team2]
setnames(for_against_scores_1,'team1','team')
setnames(for_against_scores_2,'team2','team')
for_against_scores <- rbind(for_against_scores_1,for_against_scores_2)
for_against_scores <- for_against_scores[, .(for_score = sum(for_score), 
                                             against_score = sum(against_score)), by = team]
# reorder according to the team_list
for_against_scores <- for_against_scores[sapply(team_list, function(t){which(t == team)}),] 
for_against_ratio <- for_against_scores$for_score/for_against_scores$against_score 
  
# calculate team points for the ladder board
points_ladder <- sapply(team_list, function(team){
  # win is a 4 and draw is a 2
  s1 <- as.numeric(results[team1 == team, 
                           sum(ifelse(score_team1 >  score_team2,4,
                               ifelse(score_team1 == score_team2,2,0)))])
  s2 <- as.numeric(results[team2 == team, 
                           sum(ifelse(score_team2 >  score_team1,4,
                               ifelse(score_team1 == score_team2,2,0)))])
  s1+s2
})


# temporary hack to get the final 8 and to correctly label the QF and EF matches
if (final_8_fixed){
  points_ladder_dat <- data.table(team = team_list, points = points_ladder, 
                                for_against_ratio = for_against_ratio)
  setorder(points_ladder_dat, -points, -for_against_ratio)
  final_8_team_names <- points_ladder_dat[1:8,team]
  final_8_team_ids_input <- sapply(final_8_team_names, function(team) {which(team == team_list)})
  
  # QF 1
  future_schedule[grep('Final',round_full_desc) & team1 == final_8_team_names[1],  
                                                            round_full_desc := 'Qualifying Final 1']
  # QF 2
  future_schedule[grep('Final',round_full_desc) & team1 == final_8_team_names[2],  
                                                            round_full_desc := 'Qualifying Final 2']
  
  # EF 1
  future_schedule[grep('Final',round_full_desc) & team1 == final_8_team_names[5],  
                                                            round_full_desc := 'Elimination Final 1']
  # EF 2
  future_schedule[grep('Final',round_full_desc) & team1 == final_8_team_names[6],  
                                                             round_full_desc := 'Elimination Final 2']
}

futr_rnd_type <- sapply(future_schedule$round_full_desc,function(rnd)(encode_rnd(rnd)))

# assemble input data to a list to be passed onto stan
input_list_stan <- list(round_ids = round_ids, n_rounds = n_rounds, n_matches = nrow(results),
                        n_teams = n_teams, team1_ids = as.numeric(team1_ids),
                        team2_ids = as.numeric(team2_ids), team1_win_indictr = team1_win_indictr,
                        futr_team1_ids = futr_team1_ids, futr_team2_ids = futr_team2_ids,
                        futr_round_ids = futr_round_ids, first_futr_round = futr_round_ids[1],
                        futr_n_rounds = futr_n_rounds, futr_n_matches = nrow(future_schedule),
                        futr_rnd_type = futr_rnd_type, points_ladder = points_ladder,
                        for_against_ratio = for_against_ratio, final_2_fixed = final_2_fixed,
                        semi_final_4_fixed = semi_final_4_fixed, prelim_final_4_fixed = prelim_final_4_fixed,
                        final_8_fixed = final_8_fixed, premiership_team_fixed = premiership_team_fixed, 
                        final_2_team_ids_input = final_2_team_ids_input, semi_final_4_team_ids_input = semi_final_4_team_ids_input,
                        prelim_final_4_team_ids_input = prelim_final_4_team_ids_input, final_8_team_ids_input = final_8_team_ids_input,
                        premiership_team_id_input = premiership_team_id_input)


# fit the stan model
fit <- stan(file = 'src/bayesian_elo_parameter_estimation.stan', data = input_list_stan, 
            iter = 20000, chains = 4, cores = 4)

# do some plotting
posterior <- as.matrix(fit)

mcmc_areas(posterior, 
           pars = c("xi","K"), 
           prob = 0.8) 

# plot how ELO has changed for a given team
team <- 'St Kilda'
team_id <- which(team_list == team)
mcmc_areas(posterior, regex_pars = paste0("elo_score\\[[[:digit:]]+,",team_id,"\\]")) 

#  plot the ELO distributions for all teams as of last round
rnd <- n_rounds
mcmc_areas(posterior, regex_pars = paste0("^elo_score\\[",n_rounds,",[[:digit:]]+\\]")) 


# calculate the probabilities of the future matches

n_sims <- nrow(posterior)
future_schedule[, team1_win_prob := sapply(1:nrow(future_schedule), function(match){
  col_name <- paste0('futr_match_outcome[',match,']')  
  num_sim_wins <- sum(posterior[,col_name])
  num_sim_wins/n_sims})]


future_schedule[round == 'Round 24',]

# calculate the probabiliy of each team making a milestone in the finals series
final_series_probabilities <- return_final_series_probabilities(simulated_samples = posterior, 
                                                                team_list = team_list)

