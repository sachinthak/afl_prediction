# load expert tips that was previously downloaded -------------------------

files <- list.files('data/',pattern = '^expert_tips.*.RDS')
expert_tips <- rbindlist(lapply(files, function(file){readRDS(paste0('data/',file))}))


# correct team name discrepencies between expert tips  and schedule--------------------

team_pseudonames <- data.table(original = c('GWS Giants','Geelong Cats','Brisbane','Adelaide Crows'),
                               pseudo = c('Greater Western Sydney','Geelong','Brisbane Lions','Adelaide'))

expert_tips[, tip_cpy := tip]
expert_tips <- team_pseudonames[expert_tips, on = 'original==tip_cpy']
expert_tips[!is.na(pseudo), tip := pseudo]

# delete dummy columsn
expert_tips[, original := NULL]
expert_tips[, pseudo := NULL]

team_names_from_schedule <- unique(unique(schedules$team1),unique(schedules$team2))
team_names_from_expert_tips <- unique(expert_tips$tip)
diff_length <- length(setdiff(team_names_from_expert_tips,team_names_from_schedule))

if (diff_length>0){
  warning('Expert tips team names do not match with the schedule team names! Please look into this.')
}

# get the team 1 and team 2 details into the expert tips data set
schedules_cpy <- copy(schedules)
schedules_cpy[, team1_dmy := team1]
schedules_cpy[, team2_dmy := team2]
schedules_cpy <- melt.data.table(schedules_cpy, id.vars = c('season','round','team1','team2'), 
                                 measure.vars = c('team1_dmy','team2_dmy'), value.name = 'tip')
schedules_cpy[, variable := NULL]
expert_tips[, season := as.character(season)]
expert_tips <- schedules_cpy[expert_tips, on = c('season','round','tip')]

# some times when the tipster hasnt given tips for a particular week then junk values get 
# into the team name. detect those and remove from the data set
legit_team_names <- unique(c(past_results$team1,past_results$team2,
                             team_pseudonames$original,team_pseudonames$pseudo))

expert_tips[, valid_team_name := tip %in% legit_team_names]
tipster_rounds_to_remove <- unique(expert_tips[valid_team_name == F, .(season,round,tipster)])
expert_tips <- tipster_rounds_to_remove[, cbind(.SD, remove = T)][expert_tips, on = .(season,round,tipster)]
expert_tips <- expert_tips[is.na(remove)]
expert_tips[, remove := NULL]

if (nrow(expert_tips[is.na(team1)])>0){
  stop('Problem occured while trying to merge team1,team2 into expert tips dataset!')
}

