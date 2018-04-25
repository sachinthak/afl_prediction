library(ProjectTemplate)
load.project()

expert_tips_cpy <- copy(expert_tips)
tipster_scores <- unique(expert_tips_cpy[, .(season,round,tipster,tipster_score)])

tipster_scores[, sum_score := sum(tipster_score), by = .(season,round)]

# handle first round tipster score of 0 
tipster_scores[sum_score == 0, tipster_score := 1]
tipster_scores[sum_score == 0, sum_score := sum(tipster_score)]

# weight each tipster
tipster_scores[, tipster_weight := tipster_score/sum_score]

# merge tipster scores back to expert tips
expert_tips_cpy <- tipster_scores[,.(season,round,tipster,tipster_weight)][expert_tips_cpy,
                                                                           on = c('season','round','tipster')]

expert_tips_cpy[, tip_team1 := ifelse(tip==team1,1,0)]

# get the weighted 'probability' for team 1 win
weighted_score_dat <- expert_tips_cpy[, .(team1_win_weighted_score = sum(tip_team1*tipster_weight)), 
                                      by = .(season,round,team1,team2)]

# save retro scores
saveRDS(weighted_score_dat[, .(season,round,team1,team2,
                               team1_win_prob = team1_win_weighted_score,
                               model = 'weighted_expert_tips')], 'data/retro_score_weighted_expert_tips.RDS')
