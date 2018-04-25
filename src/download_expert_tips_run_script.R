
library(ProjectTemplate)
load.project()

tipsters <- c('JON ANDERSON','DERMOTT BRERETON','DAVID KING',
              'SHANE WARNE','MICHAEL WARNER','LAUREN WOOD','NATHAN BROWN',
              'GILBERT GARDINER','MAX GAWN','SCOTT GULLAN','JAMES HIRD','SARAH JONES',
              'MATTHEW LLOYD','MARK ROBINSON','GERARD WHATELEY', 'CHRIS CAVANAGH',
              'BIANCA CHATFIELD','PATRICK DANGERFIELD', 'JAY CLARK', 'SAM EDMUND',
              'MATTHEW GUY','BRENDON GODDARD','SAM LANDSBERGER','MICK MALTHOUSE',
              'JON RALPH','DANIEL ANDREWS','DAN ANDREWS','GLENN MCFARLANE','ELIZA SEWELL','TIM WATSON',
              'KISS OF DEATH')

# handling multiple names for the same tipster
pseudonames <- data.table(original = 'DANIEL ANDREWS', pseudo = 'DAN ANDREWS')


link_round_1 <- 'http://www.heraldsun.com.au/sport/afl/more-news/afl-tipping-2018-expert-predictions-for-round-1-2018/news-story/1f9c2a6dd59cbd6316a01fd93891f03f'
link_round_2 <- 'http://www.heraldsun.com.au/sport/afl/more-news/afl-tipping-2018-expert-predictions-for-round-2-2018/news-story/e8fe5f3116fbbffe41ddf8df7142ce2e'
link_round_3 <- 'http://www.heraldsun.com.au/sport/afl/afl-tipping-2018-expert-predictions-for-round-3-2018/news-story/40fd7ace495bc5d69e0399cdaa5b0670'
link_round_4 <- 'http://www.heraldsun.com.au/sport/afl/more-news/afl-tipping-2018-our-expert-predictions-for-round-4-2018/news-story/ba7e776b9910dea4465d19c8928245db'
link_round_5 <- 'http://www.heraldsun.com.au/sport/afl/expert-opinion/see-who-the-herald-sun-experts-are-tipping-ahead-of-round-5/news-story/ac65f986aa35bf719f7b685c02b45aa3'


link <- link_round_5
round_str <- 'Round 5'
season_str <- 2018
num_matches_in_round <- schedules[round == round_str & season == season_str, .N]

expert_tips <- download_expert_tips (link = link,
                      season = season_str,
                      round = round_str,
                      num_matches_in_round = num_matches_in_round,
                      tipsters = tipsters,
                      pseudonames = pseudonames)

  