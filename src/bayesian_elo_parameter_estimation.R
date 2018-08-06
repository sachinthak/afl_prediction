library(bayesplot)

curr_season <- 2018

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
team1_ids <- sapply(results$team1, function(team){which(team == team_list)})
team2_ids <- sapply(results$team2, function(team){which(team == team_list)})
futr_team1_ids <- sapply(future_schedule$team1, function(team){which(team == team_list)})
futr_team2_ids <- sapply(future_schedule$team2, function(team){which(team == team_list)})


# get scores
team1_score <- results$score_team1
team2_score <- results$score_team2
team1_win_indictr <- as.numeric(team1_score >= team2_score)

# assemble input data to a list to be passed onto stan
input_list_stan <- list(round_ids = round_ids, n_rounds = n_rounds, n_matches = nrow(results),
                        n_teams = n_teams, team1_ids = as.numeric(team1_ids),
                        team2_ids = as.numeric(team2_ids), team1_win_indictr = team1_win_indictr,
                        futr_team1_ids = futr_team1_ids, futr_team2_ids = futr_team2_ids,
                        futr_round_ids = futr_round_ids, first_futr_round = futr_round_ids[1],
                        futr_n_rounds = futr_n_rounds, futr_n_matches = nrow(future_schedule))


# fit the stan model
fit <- stan(file = 'src/bayesian_elo_parameter_estimation.stan', data = input_list_stan, 
            iter = 5000, chains = 4)

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
mcmc_areas(posterior, regex_pars = paste0("elo_score\\[",n_rounds,",[[:digit:]]+\\]")) 



# calculate the probabilities of the future matches
n_sims <- nrow(posterior)
future_schedule[, team1_win_prob := sapply(1:nrow(future_schedule), function(match){
  col_name <- paste0('futr_match_outcome[',match,']')  
  num_sim_wins <- sum(posterior[,col_name])
  num_sim_wins/n_sims})]


future_schedule[round == 'Round 21',]

