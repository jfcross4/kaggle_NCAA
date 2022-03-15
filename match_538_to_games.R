match_538_to_games = function(submission_file, x538_ratings, 
                              tournament){
  
  if(tournament=="M"){
    names = read.csv("stage2data/MTeamSpellings.csv")
  }else{
    names = read.csv("stage2data/WTeamSpellings.csv")
  }
  
  x538_matched <- left_join(x538_ratings %>%
                              mutate(team_name = tolower(team_name)), 
                            names, by=c("team_name"="TeamNameSpelling"))
  
  
  games = games_to_predict(read.csv(submission_file))
  
  games = left_join(games,
                    x538_matched %>% 
                      dplyr::select(TeamID, 
                                    team1rating = team_rating),
                    by=c("team1"="TeamID"))
  
  games = left_join(games,
                    x538_matched %>% 
                      dplyr::select(TeamID, team2rating = team_rating),
                    by=c("team2"="TeamID"))
  
  return(games)
}
