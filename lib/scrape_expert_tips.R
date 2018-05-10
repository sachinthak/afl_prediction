
scrape_expert_tips <- function(link,
                               season = 2018,
                               round,
                               num_matches_in_round = 9,
                               tipsters,
                               pseudonames)
{
  tips_page <- read_html(link)
  node <- html_nodes(tips_page, 'p')
  
  start_positions <- unlist(lapply(tipsters, function(tipster){grep(tipster,node,ignore.case = T)}))
  if (length(start_positions) != length(tipsters)) {
    warning('There was a mismatch between the number of tipsters and the starting poisions!')
  }
  
  # initialise a data table to store parsed results
  scraped_result <- data.table(tipster = character(0), tipster_score = numeric(0),tip = character(0), margin = numeric(0))

  # loop though tipsters to read the data
  for (record in 1:length(start_positions)) {
    # header record indicating the name and the score so far
    tipster_header <- node[start_positions[record]]
    tipster_name <- gsub('.*>([[:alpha:]]*.*[[:alpha:]])(\\s?)([[:digit:]]*)</.*','\\1',tipster_header)
    
    # check whether the parsed name is infact a tipster. if not probably this is an invalid
    # starting position, therefore skip to the next iteration
    if(!tolower(tipster_name) %in% tolower(tipsters)){
      warning('mismatch while parsing the tipster name!')
      next
    }
    
    tipster_score <- gsub('.*>([[:alpha:]]*.*[[:alpha:]])(\\s?)([[:digit:]]*)</.*','\\3',tipster_header)
    tipster_score <- ifelse(tipster_score == '',0,as.numeric(tipster_score))
    for (match in 1:num_matches_in_round){
      tip <- node[start_positions[record] + match]
      tipped_team  <- gsub('.*>([[:alpha:]]*.*[[:alpha:]])(\\s?)([[:digit:]]*)</p.*','\\1',tip)
      tipped_margin <- gsub('.*>([[:alpha:]]*.*[[:alpha:]])(\\s?)([[:digit:]]*)</p.*','\\3',tip)
      tipped_margin <- as.numeric(tipped_margin)
      
      scraped_result <- rbind(scraped_result,
                              data.table(tipster = tipster_name, 
                                         tipster_score = tipster_score,
                                         tip = tipped_team, 
                                         margin = tipped_margin))        
    }
  }
  
  scraped_result[, season := season]
  scraped_result[, round := round]
  
  # handle pseudo names 
  #(i.e. tipsters appearing by different names eg, Daniel Andrews vs Dan Andrews)
  scraped_result[, tipster := toupper(tipster)]
  scraped_result[,tipster_copy := tipster]
  scraped_result <- pseudonames[scraped_result, on = 'pseudo==tipster_copy'] 
  scraped_result[!is.na(original), tipster := original]
  
  # remove dummy columns
  scraped_result[, original := NULL]
  scraped_result[, pseudo := NULL]
  
  return(scraped_result)
}
