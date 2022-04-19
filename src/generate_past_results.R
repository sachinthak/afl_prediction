# Modify and run this script once each year to generate the starting dataset 

past_results <- readRDS('data/past_results.rds')
past_results[, season := as.numeric(season)]
past_results[,  date_venue := NULL]
past_results[, result := NULL]
schedules <- readRDS('data/schedules.rds')
schedules[,  date_venue := NULL]

# 2018 fixture is from https://fixturedownload.com
season_names <- c(2018,2019,2020,2021)
  

results <- list()
for (season in season_names){
  res  <- copy(setDT(get(paste0('afl.',season,'.AUSEasternStandardTime'))))
  
  setnames(res, c('Home.Team','Away.Team','Round.Number','Date','Location'), c('team1','team2','round','date','venue'))
  
  
  # format of the new results to be consistant with the older results
  res[team1 == 'GWS Giants', team1 := 'Greater Western Sydney']
  res[team2 == 'GWS Giants', team2 := 'Greater Western Sydney']
  res[team1 == 'Brisbane', team1 := 'Brisbane Lions']
  res[team2 == 'Brisbane', team2 := 'Brisbane Lions']
  res[team1 == 'Adelaide Crows', team1 := 'Adelaide']
  res[team2 == 'Adelaide Crows', team2 := 'Adelaide']
  res[team1 == 'Geelong Cats', team1 := 'Geelong']
  res[team2 == 'Geelong Cats', team2 := 'Geelong']
  res[team1 == 'Sydney Swans', team1 := 'Sydney']
  res[team2 == 'Sydney Swans', team2 := 'Sydney']
  res[team1 == 'West Coast Eagles', team1 := 'West Coast']
  res[team2 == 'West Coast Eagles', team2 := 'West Coast']
  res[team1 == 'Gold Coast Suns', team1 := 'Gold Coast']
  res[team2 == 'Gold Coast Suns', team2 := 'Gold Coast']
  
  setdiff(union(res$team1,res$team2),union(schedules$team1,schedules$team2))
  setdiff(union(schedules$team1,schedules$team2),union(res$team1,res$team2))
  
  res[, round := paste0('Round ',round)]
  res[, date := strftime(as.Date(substr(date,start = 1,10),format = '%d/%m/%Y'),'%d-%b-%Y')]
  
  res[, season := season]
  res[, score_team1 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\1',Result))]
  res[, score_team2 := as.numeric(gsub('([0-9]*)\\s+-\\s([0-9]*)','\\2',Result))]
  
  res[, team1 := as.character(team1)]
  res[, team2 := as.character(team2)]
  
  res[, Result := NULL]
  
  # venue's from the current season data does not match correctly with 
  # the venue names from the historical results. the reconcillation is done below
  res[ , venue := as.character(venue)]
  mismatched.venue.names <- setDT(mismatched.venue.names)
  mismatched.venue.names[, venue_current := as.character(venue_current)]
  mismatched.venue.names[, venue_matched := as.character(venue_matched)]
  
  res <- mismatched.venue.names[res, on = 'venue_current==venue']
  res[is.na(venue_matched), venue_matched := venue_current]
  
  res[, venue_current := NULL]
  setnames(res, 'venue_matched', 'venue')
  res[ , date := as.character(date)]
  res[, Match.Number := NULL]
  
  # later into the season https://fixturedownload.com introduces records corresponding to the 
  # the finals series into the csv file which we have downloaded and started processing. 
  # If they have done so we have to rename the round names of the finals series to be consistant 
  # with the past_results (which was sourced from a different source)
  res[round == 'Round Finals W1', round := c('Qualifying Final' ,'Qualifying Final' ,
                                                      'Elimination Final','Elimination Final')]
  res[round == 'Round Semi Finals', round := 'Semi Final' ]
  res[round == 'Round Prelim Finals', round := 'Preliminary Final' ]
  res[round == 'Round Grand Final', round := 'Grand Final' ]
  
  # set the coloumn order as the past_results
  setcolorder(res,names(past_results))
  results[[as.character(season)]] <- res
}


# do some sense checks
for (season in season_names) {
  print(season)
  print(setdiff(union(results[[as.character(season)]]$team1,results[[as.character(season)]]$team2),union(schedules$team1,schedules$team2)))
  print(setdiff(union(schedules$team1,schedules$team2),union(results[[as.character(season)]]$team1,results[[as.character(season)]]$team2)))
}

# create a duplicate coloumn of round for later use



results <- rbindlist(results)

# combine with the past results and schedules
past_results <- rbind(past_results,results[!is.na(score_team1)])
schedules <- rbind(schedules, results[,.(season,round,team1,team2,date,venue)])

# do some sense checks to see whether the same venue is represented by slighly different names
dcast.data.table(formula = 'venue ~ season', data = past_results[season >2015, .N, by = .(venue,season)], value.var = 'N')

saveRDS(past_results, 'data/past_results_2021.rds')
saveRDS(schedules, 'data/schedules_2021.rds')
