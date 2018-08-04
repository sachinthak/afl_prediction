library(bayesplot)

curr_season <- 2018

results <- past_results[season == curr_season,]


# generate input data to pass on to Stan ----------------------------------

round_ids <- results[, as.numeric(gsub(pattern = "Round (.*)",replacement = '\\1',x = round))]
n_rounds <- uniqueN(round_ids)
  
team_list <- sort(unique(union(results$team1,results$team2)))
n_teams <- length(team_list)

# get the ids of teams
team1_ids <- sapply(results$team1, function(team){which(team == team_list)})
team2_ids <- sapply(results$team2, function(team){which(team == team_list)})

# get scores
team1_score <- results$score_team1
team2_score <- results$score_team2
team1_win_indictr <- as.numeric(team1_score >= team2_score)

# assemble input data to a list for stan
input_list_stan <- list(round_ids = round_ids, n_rounds = n_rounds, n_matches = nrow(results),
                        n_teams = n_teams, team1_ids = as.numeric(team1_ids),
                        team2_ids = as.numeric(team2_ids), team1_win_indictr = team1_win_indictr)


# fit the stan model
fit <- stan(file = 'src/bayesian_elo_parameter_estimation.stan', data = input_list_stan, 
            iter = 1000, chains = 4)

# do some plotting
posterior <- as.matrix(fit)

mcmc_dens(posterior, 
           pars = c("xi","K"), 
           prob = 0.8) 

mcmc_areas(posterior, regex_pars = "elo_score\\[[[:digit:]]+,1\\]") 