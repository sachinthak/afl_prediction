
library(ProjectTemplate)
load.project()

sn <- 2018
rnd <- 'Round 8'


schedule_for_the_round <- schedules[season == sn & round == rnd]

# data structure to store predictions for the current round using different models
round_prediction <- data.table(season = character(0),round = character(0),
                               team1 = character(0), team2 = character(0), 
                               team1_win_prob = numeric(0),
                               margin = numeric(0),
                               model = character(0))

# score using the simple elo method ---------------------------------------

#  settings for simple elo 

K <-  25#10 #25
lambda <- 400 # 700 # 400
autocorrelation_adjust <- TRUE
margin_of_victory_adjust <- TRUE
home_field_advantage_adjust <- TRUE
scoring_method <- 'classic'
point_spread_regression <- NULL
n_sim_tournaments <- 2000
reg_to_mean_factor <- 0.75
home_field_advantage_coeff <- 15


# calculate elo by using the data after 2016 

sn_pre <- 2016
team_list <- union(past_results[season>=sn_pre]$team1,past_results[season>=sn_pre]$team2)


results <- past_results[season>=sn_pre]

initial_elo_ratings <- data.table(team = team_list, elo = 1500)
updated_elo_ratings <- return_elo_scores(results = results, 
                                         initial_ratings = initial_elo_ratings, 
                                         home_field_advantage_stats = hfa_helper_dataset,
                                         K = K, 
                                         lambda = lambda,
                                         autocorrelation_adjust = autocorrelation_adjust,
                                         margin_of_victory_adjust = margin_of_victory_adjust,
                                         home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                         home_field_advantage_coeff = home_field_advantage_coeff,
                                         scoring_method = scoring_method)

updated_elo_ratings <- updated_elo_ratings$ratings


# fit the logistic regression to predict the point spread 

simple_logit_point_spread_fit <- simple_logit_point_spread_regression(results = results, initial_elo_ratings = initial_elo_ratings,
                                                                      home_field_advantage_stats = hfa_helper_dataset,
                                                                      K = K, lambda = lambda, 
                                                                      autocorrelation_adjust = autocorrelation_adjust,
                                                                      margin_of_victory_adjust = margin_of_victory_adjust,
                                                                      scoring_method = scoring_method,
                                                                      home_field_advantage_adjust = home_field_advantage_adjust,                                         
                                                                      home_field_advantage_coeff = home_field_advantage_coeff,
                                                                      reg_to_mean_factor = reg_to_mean_factor)


for (match in 1:nrow(schedule_for_the_round)) {
  
  team_1 <- schedule_for_the_round[match,team1]
  team_2 <-  schedule_for_the_round[match,team2]
  ground <- schedules[season == sn & round == rnd & team1 == team_1 & team2 == team_2, venue]
  num_past_matches_team_1 <- hfa_helper_dataset[season == sn-1 & venue == ground & team == team_1, num_past_matches]
  num_past_matches_team_2 <- hfa_helper_dataset[season == sn-1 & venue == ground & team == team_2, num_past_matches]
  home_field_advantage <- home_field_advantage_coeff*(log(1+num_past_matches_team_1)-log(1+num_past_matches_team_2))
  elo_diff <- updated_elo_ratings[team==team_1,elo] - updated_elo_ratings[team==team_2,elo] 
  if (home_field_advantage_adjust) {
    elo_diff <- elo_diff + home_field_advantage
  }
  p <- 1/(1+10^(-elo_diff/lambda))
  logit_p <- log(p/(1-p))
  margin <- predict.lm(simple_logit_point_spread_fit, newdata = data.table(logit_p = logit_p)) 
  
  round_prediction <- rbind(round_prediction, data.table(season = sn, round = rnd,
                                                         team1 = team_1, team2 = team_2,
                                                         team1_win_prob = p , margin = margin,
                                                         model = 'simple_elo'))
}


# score using the weighted expert tips ------------------------------------

# the following file will calculated the weighted scores based on expert tips
source('src/retro_score_weighted_expert_tips.R')

expert_tip_preds <- copy(weighted_score_dat[season == sn & round == rnd])
expert_tip_preds[, margin := NA]
expert_tip_preds[, model := 'weighted_expert_tips']
setnames(expert_tip_preds,'team1_win_weighted_score','team1_win_prob')

# add prediction to data structure storing predictions
round_prediction <- rbind(round_prediction,expert_tip_preds)


# score using the ensemble logistic model ---------------------------------

# train the logistic model

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

dat <- retro_score_cpy[season == sn & round <= rnd]

logistic_fit <- glm(formula = 'team1_won ~ 1 + simple_elo + weighted_expert_tips', data = dat,
                    family = binomial(link = 'logit'))

ensemble_score_data <- copy(round_prediction)
ensemble_score_data <- dcast.data.table(ensemble_score_data, formula = 'season + round + team1 + team2 ~ model',
                                        value.var = 'team1_win_prob')
ensemble_score_data[, team1_win_prob := predict(logistic_fit,ensemble_score_data, type = 'response')]

# add prediction to data structure storing predictions
round_prediction <- rbind(round_prediction,ensemble_score_data[, .(season,round,team1,team2,
                                                                   team1_win_prob,margin = NA, model = 'ensemble_logistic')])



# score using the ensemble bayesian model ---------------------------------

# fit the bayesian model
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

bayes_ensemble_score_data <- copy(round_prediction[model %in% ensemble_models])
bayes_ensemble_score_data <- dcast.data.table(bayes_ensemble_score_data, formula = 'season + round + team1 + team2 ~ model',
                                        value.var = 'team1_win_prob')
bayes_ensemble_score_data[, team1_win_prob := theta[1]*simple_elo + theta[2]*weighted_expert_tips]

round_prediction <- rbind(round_prediction,bayes_ensemble_score_data[, .(season,round,team1,team2,
                                                                         team1_win_prob,margin = NA, model = 'ensemble_bayesian')])

# plot results ------------------------------------------------------------

ggplot(round_prediction) + geom_bar(aes(x=model, y= team1_win_prob, 
                                        fill = model), stat = 'identity', position = 'dodge') +
  facet_grid(.~team1)

