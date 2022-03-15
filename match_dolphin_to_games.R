match_dolphin_to_games = function(submission_file, dolphin_ratings){
  
  games = games_to_predict(read.csv(submission_file))
  
  games = left_join(games,
            dolphin_ratings %>% 
              dplyr::select(TeamID,
                            team1rating = predictive.RATING),
            by=c("team1"="TeamID"))
  
  games = left_join(games,
                    dolphin_ratings %>% 
                      dplyr::select(TeamID,
                                    team2rating = predictive.RATING),
                    by=c("team2"="TeamID"))
  
  return(games)
}
