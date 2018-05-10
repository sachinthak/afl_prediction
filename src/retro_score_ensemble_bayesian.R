
library(ProjectTemplate)
load.project()

# stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# current seasn
sn_current <- 2018
# how many rounds to retro score in the current seasn
rounds_so_far <- 7
# model names to ensemble
ensemble_models <- c('weighted_expert_tips','simple_elo')

retro_score_cpy <- copy(retro_scores)

retro_score_cpy <- retro_score_cpy[model %in% ensemble_models]

retro_score_cpy <- dcast.data.table(retro_score_cpy, formula = 'season + round + team1 + team2 ~ model', 
                                    value.var = 'team1_win_prob')

# merge historical results
retro_score_cpy[, season := as.numeric(season)]
retro_score_cpy <- past_results[,.(season,round,team1,team2,score_team1,score_team2)][retro_score_cpy, 
                                                                                      on = .(season,round,team1,team2),
                                                                                      nomatch = 0]

retro_score_cpy[, team1_won := ifelse(score_team1>=score_team2,1,0)]


round_prediction <- data.table(season = numeric(0), round = character(0),
                               team1 = character(0), team2 = character(0), 
                               team1_win_prob = numeric(0))

# use round 1 to initiate scoring
dat <- retro_score_cpy[season == sn_current & round <= 'Round 1']

stan_input <- list(alpha = c(1,1),
                   num_models = 2,
                   simple_elo = dat$simple_elo,
                   weighted_expert_tips = dat$weighted_expert_tips,
                   team1_won = dat$team1_won,
                   N = nrow(dat)
)

stan_fit <- stan(file = 'src/bayesian_ensemble_fit.stan', data = stan_input, 
                 iter = 1000, chains = 4)
smps <- extract(stan_fit)
theta <- apply(smps$theta,2,mean)

for (rnd in 2:rounds_so_far){
  # score the round using the most recent bayesian  fit
  dat <- retro_score_cpy[season == sn_current & round == paste0('Round ',rnd)]
  dat[, team1_win_prob := theta[1]*simple_elo + theta[2]*weighted_expert_tips ]
  
  round_prediction <- rbind(round_prediction, dat[, .(season,round,team1,team2,
                                                      team1_win_prob)])
  
  # add this round to training
  dat <- retro_score_cpy[season == sn_current & round <= paste0('Round ',rnd)]
  
  # fit the stan model
  stan_input <- list(alpha = c(1,1),
                     num_models = 2,
                     simple_elo = dat$simple_elo,
                     weighted_expert_tips = dat$weighted_expert_tips,
                     team1_won = dat$team1_won,
                     N = nrow(dat)
  )
  
  stan_fit <- stan(file = 'src/bayesian_ensemble_fit.stan', data = stan_input, 
                   iter = 1000, chains = 4)
  smps <- extract(stan_fit)
  theta <- apply(smps$theta,2,mean)
}

# save retro scores 
round_prediction[, model := 'ensemble_bayesian']
saveRDS(round_prediction, 'data/retro_score_ensemble_bayesian.RDS')
