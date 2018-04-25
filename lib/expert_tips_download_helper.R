
download_expert_tips <- function(link,
                                 season = 2018,
                                 round,
                                 num_matches_in_round = 9,
                                 tipsters,
                                 pseudonames
                                 ) {
  
  scrape_result <- scrape_expert_tips(link = link,
                                      season = season,
                                      round = round,
                                      num_matches_in_round = num_matches_in_round,
                                      tipsters = tipsters,
                                      pseudonames = pseudonames)
  
  if(nrow(scrape_result) > 0){
    file_name <- paste0('data/expert_tips_',season,'_',gsub(' ','_',round),'.RDS')
    saveRDS(scrape_result,file = file_name)
  }else {
    stop('Problem downloading expert tips!')
  }
  
  return(scrape_result)
}
