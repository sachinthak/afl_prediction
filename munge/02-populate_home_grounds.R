# I use the following heuristic that for each season the ground that a team has played
# the most is that team's home ground

melted_schedules <- melt.data.table(schedules, id.vars = c('season','round','venue'), 
                measure.vars = c('team1','team2'), value.name = 'team')

melted_schedules <- melted_schedules[, .(num_matches = .N ), by = .(team,season,venue)]
setorder(melted_schedules,team,season, -num_matches)
home_grounds <- melted_schedules[season > 2000, head(.SD, n=1), by = .(season,team)]
