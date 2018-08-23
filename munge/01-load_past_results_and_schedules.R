# load the past results and schedule --------------------------------------

past_results <- readRDS('data/past_results.rds')
past_results[, season := as.numeric(season)]
past_results[,  date_venue := NULL]
past_results[, result := NULL]
schedules <- readRDS('data/schedules.rds')
schedules[,  date_venue := NULL]

# 2018 fixture is from https://fixturedownload.com
results_2018 <- copy(setDT(afl.2018.AUSEasternStandardTime))
setnames(results_2018, c('Home.Team','Away.Team','Round.Number','Date','Location'), c('team1','team2','round','date','venue'))

# format the 2018 results to be consistant with the older results
results_2018[team1 == 'GWS Giants', team1 := 'Greater Western Sydney']
results_2018[team2 == 'GWS Giants', team2 := 'Greater Western Sydney']
results_2018[team1 == 'Brisbane', team1 := 'Brisbane Lions']
results_2018[team2 == 'Brisbane', team2 := 'Brisbane Lions']

setdiff(union(results_2018$team1,results_2018$team2),union(schedules$team1,schedules$team2))
setdiff(union(schedules$team1,schedules$team2),union(results_2018$team1,results_2018$team2))

results_2018[, round := paste0('Round ',round)]
results_2018[, date := strftime(as.Date(substr(date,start = 1,10),format = '%d/%m/%Y'),'%d-%b-%Y')]

results_2018[, season := 2018]
results_2018[, score_team1 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\1',Result))]
results_2018[, score_team2 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\2',Result))]

results_2018[, team1 := as.character(team1)]
results_2018[, team2 := as.character(team2)]

results_2018[, Result := NULL]

# venue's from the current season data does not match correctly with 
# the venue names from the historical results. the reconcillation is done below
results_2018[ , venue := as.character(venue)]
mismatched.venue.names.2018 <- setDT(mismatched.venue.names.2018)
mismatched.venue.names.2018[, venue_current := as.character(venue_current)]
mismatched.venue.names.2018[, venue_matched := as.character(venue_matched)]

results_2018 <- mismatched.venue.names.2018[results_2018, on = 'venue_current==venue']
results_2018[is.na(venue_matched), venue_matched := venue_current]

results_2018[, venue_current := NULL]
setnames(results_2018, 'venue_matched', 'venue')
results_2018[ , date := as.character(date)]

# later into the season https://fixturedownload.com introduces records corresponding to the 
# the finals series into the csv file which we have downloaded and started processing. 
# If they have done so we have to rename the round names of the finals series to be consistant 
# with the past_results (which was sourced from a different source)
results_2018[round == 'Round Finals W1', round := c('Qualifying Final' ,'Qualifying Final' ,
                                                    'Elimination Final','Elimination Final')]
results_2018[round == 'Round Semi Finals', round := 'Semi Final' ]
results_2018[round == 'Round Prelim Finals', round := 'Preliminary Final' ]
results_2018[round == 'Round Grand Final', round := 'Grand Final' ]

# set the coloumn order as the past_results
setcolorder(results_2018,names(past_results))

# create a duplicate coloumn of round for later use
past_results[, round_full_desc := round]
schedules[, round_full_desc := round]
results_2018[,round_full_desc := round]



# combine with the past results and schedules
past_results <- rbind(past_results,results_2018[!is.na(score_team1)])
schedules <- rbind(schedules, results_2018[,.(season,round,team1,team2,date,venue,round_full_desc)])


# Append 1 and 2 for finals series to distinguish, i.e. Preliminary Final 1, Preliminary Final 2. 
# I assume that in the data set they occur in the order. i.e. 2 after 1. 
# Clearly this is not true as in 2017 semifinals. Address this issue later.
past_results[grep('Final',round),  round_full_desc := paste0(round_full_desc,' ',1:.N), 
             by =  .(season,round)]
schedules[grep('Final',round),  round_full_desc := paste0(round_full_desc,' ',1:.N), 
             by =  .(season,round)]



# If the finals series is not already in the schedule add it using the template created
# else dont add
missing_schedule <- schedules[season == 2018, .(round_full_desc, exist = T)][X2018.finals.series.template.schedule, on = 'round_full_desc']
missing_schedule <- missing_schedule[exist == F, .(season,round,team1,team2,date,venue,round_full_desc)]
schedules <- rbind(schedules,missing_schedule)

# Replace the 'round' column for the final series with a numeric round.
# For example 'Preliminary Final' with 'Round 24'.
col_names_schedules <- names(schedules)
col_names_past_results <- names(past_results)
finals_series_offset <- data.table(round = c('Qualifying Final','Elimination Final',
                                           'Semi Final','Preliminary Final',
                                           'Grand Final'),
                                 offset = c(1,1,2,3,4))
non_finals_rounds <- schedules[grep('Round',round), .(non_final_rounds = uniqueN(round)), by = season]

schedules <- finals_series_offset[schedules, on = 'round']
schedules <- non_finals_rounds[schedules, on = 'season']
schedules[grep('Final',round), round := paste0('Round ',non_final_rounds + offset)]

past_results <- finals_series_offset[past_results, on = 'round']
non_finals_rounds[, season := as.numeric(season)]
past_results <- non_finals_rounds[past_results, on = 'season']
past_results[grep('Final',round), round := paste0('Round ',non_final_rounds + offset)]

schedules <- schedules[, (col_names_schedules), with = F]
past_results <- past_results[,(col_names_past_results), with = F]

# Populate some statistics to handle home field advantage -----------------

# For each season we will calculate the number of times a team has played in a venue for the last n = three
# seasons

venue_history <- melt.data.table(past_results, id.vars = c('season','venue','round'), 
                      measure.vars = c('team1','team2'), value.name = 'team')
venue_history[,  variable := NULL]

venue_history_summary <- venue_history[,.(num_matches = .N), by = .(season,team,venue)]

# create all combinations of season, venue, team 
teams <- unique(venue_history_summary$team)
venues <- unique(venue_history_summary$venue)
seasons <- unique(venue_history_summary$season)

hfa_helper_dataset <- CJ(teams,venues,seasons)
setnames(hfa_helper_dataset,c('V1','V2','V3'), c('team','venue','season'))
                                 
setnames(venue_history_summary, 'season','past_season' )
# create a duplicate column as non equi join that i'm about to do will destroy the original
venue_history_summary[, season_cpy := past_season]

hfa_helper_dataset <- venue_history_summary[hfa_helper_dataset, on = c('team','venue','season_cpy<season'), 
                                            allow.cartesian = T]
setnames(hfa_helper_dataset, 'season_cpy','season')

setorder(hfa_helper_dataset, season,venue,team,-past_season)

# just extract the most recent 3 season reords per season, team, venue
hfa_helper_dataset <- hfa_helper_dataset[, head(.SD, n =  3), by = .(season,team,venue)]
hfa_helper_dataset[season-past_season>3, num_matches := 0]
hfa_helper_dataset <- hfa_helper_dataset[, .(num_past_matches = sum(num_matches,na.rm = T)), 
                                         by = .(season,team,venue)]
