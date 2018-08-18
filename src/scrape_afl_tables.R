# This file scrapes the data from 2000 to 2017 from afltables.com and
# stores the results and the schedule to data directory for be loaded automatically using projectTemplate.

library('ProjectTemplate') 
load.project()


# scrape the previous afl results from the afltables.com ------------------
scrape_season_results <- function(season = 2017) 
{

  cat('Scraping data for ', season, ' season\n' )
  link <- paste0('https://afltables.com/afl/seas/',season,'.html') 
  
  scrape_dat <- data.table(season = character(0),
                           round = character(0),
                           team1 = character(0),
                           team2 = character(0),
                           score_team1 = numeric(0),
                           score_team2 = numeric(0),
                           date_venue = character(0),
                           date = character(0),
                           venue = character(0),
                           result = character(0))
  
  afl_tables <- read_html(link)
  nodes <- html_nodes(afl_tables, 'table')
  
  for ( ind in 1:length(nodes)){
    tbl <- html_table(nodes[ind],fill= T)[[1]]
    
    
    # identify round
    if (nrow(tbl)==1){
      rnd <- grep('Round|Final', tbl$X1, value = T)
    }
    
    # if the table has two rows it describes a match
    if (nrow(tbl) == 2) {
      dat <- data.table(season = season,
                        round = rnd,
                        team1 = tbl$X1[1],
                        team2 = tbl$X1[2],
                        score_team1 = tbl$X3[1],
                        score_team2 = tbl$X3[2],
                        date_venue = tbl$X4[1],
                        date = substr(tbl$X4[1],5,15),
                        venue = gsub('.*Venue: (.*)',tbl$X4[1],replacement = '\\1'),
                        result = tbl$X4[2])
      scrape_dat <- rbind(scrape_dat,dat)
    }
    
  }
  return(scrape_dat)
}

# run the scraping from 2000 to 2017 --------------------------------------
seasons <- 2000:2017
res <- lapply(seasons, scrape_season_results)
res <- rbindlist(res)
past_results <- res
schedules <- past_results[, .(season,round,team1,team2,date_venue,date,venue)]



# store the results in the data folder ------------------------------------
saveRDS(past_results, 'data/past_results.rds')
saveRDS(schedules, 'data/schedules.rds')
