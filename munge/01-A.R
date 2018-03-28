# load the past results and schedule --------------------------------------

past_results <- readRDS('data/past_results.RDS')
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

results_2018[, season := '2018']
results_2018[, score_team1 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\1',Result))]
results_2018[, score_team2 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\2',Result))]

results_2018[, team1 := as.character(team1)]
results_2018[, team2 := as.character(team2)]

results_2018[, Result := NULL]

# still need to make sure the venues match
results_2018[ , venue := as.character(venue)]
results_2018[ , date := as.character(date)]

# set the coloumn order as the past_results
setcolorder(results_2018,names(past_results))

# combine with the past results and schedules
past_results <- rbind(past_results,results_2018[!is.na(score_team1)])
schedules <- rbind(schedules, results_2018[,.(season,round,team1,team2,date,venue)])


