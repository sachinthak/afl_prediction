library(ProjectTemplate)
load.project()

# analyse different model performances ------------------------------------

retro_score_cpy <- copy(retro_scores)

# merge results to evaluate
retro_score_cpy[, season := as.numeric(season)]
retro_score_cpy <- past_results[retro_score_cpy, on = .(season,round,team1,team2), nomatch = 0]

# convert prob to 1 or 0 
retro_score_cpy[, team1_tipped := ifelse(team1_win_prob > .5,1,-1)]

# actual result team1 won = 1, team2 won = -1, draw = 0
retro_score_cpy[, team1_actually_won := ifelse(score_team1>score_team2,1,
                                                  ifelse(score_team1<score_team2,-1,0))]
retro_score_cpy[, prediction_score := ifelse(team1_tipped*team1_actually_won >=0,1,0)]
retro_summary <- retro_score_cpy[, .(score = sum(prediction_score)), by = .(season,round,model)]

# compare models - breakdown by round
ggplot(retro_summary) + geom_bar(aes(x=model,y=score, fill = model),
                                 stat = 'identity',position = 'dodge') + facet_grid(.~round) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# compare models - all rounds so far aggregated
ggplot(retro_summary[, .(score = sum(score)), by = .(season,model)]) + geom_bar(aes(x=model,y=score, fill = model),
                                 stat = 'identity',position = 'dodge') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

