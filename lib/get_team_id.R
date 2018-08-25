get_team_id <- function(team_name, team_list)
{
  id <- which(team_name == team_list)
  # return 0 if no match
  ifelse(length(id)==0,0,id)
}