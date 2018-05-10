
library(ProjectTemplate)
load.project()


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
retro_score_cpy <- past_results[,.(season,round,team1,team2,score_team1,score_team2)][retro_score_cpy, on = .(season,round,team1,team2)]

retro_score_cpy[, team1_won := ifelse(score_team1>=score_team2,1,0)]

round_prediction <- data.table(season = numeric(0), round = character(0),
                               team1 = character(0), team2 = character(0), 
                               team1_win_prob = numeric(0))

# use round 1 to initiate scoring
dat <- retro_score_cpy[season == sn_current & round <= 'Round 2']

logistic_fit <- glm(formula = 'team1_won ~ 1 + simple_elo + weighted_expert_tips', data = dat,
                    family = binomial(link = 'logit'))
for (rnd in 3:rounds_so_far){
  # score the round using the most recent logistic fit
  dat <- retro_score_cpy[season == sn_current & round == paste0('Round ',rnd)]
  dat[, team1_win_prob := predict(logistic_fit,dat,type = 'response')]
  
  round_prediction <- rbind(round_prediction, dat[, .(season,round,team1,team2,
                                                      team1_win_prob)])
  
  # add this round to training
  dat <- retro_score_cpy[season == sn_current & round <= paste0('Round ',rnd)]
  
  logistic_fit <- glm(formula = 'team1_won ~ 1 + simple_elo + weighted_expert_tips', data = dat,
                      family = binomial(link = 'logit'))
}

# save retro scores 
round_prediction[, model := 'ensemble_logistic']
saveRDS(round_prediction, 'data/retro_score_ensemble_logistic.RDS')
