match_dolphin_to_games = function(submission_file, dolphin_ratings){
  
  games = games_to_predict(read.csv(submission_file))
  
  games = left_join(games,
            dolphin_ratings %>% 
              dplyr::select(team_id, season, 
                            team1rating = predictive.RATING),
            by=c("team1"="team_id", "season"))
  
  games = left_join(games,
                    dolphin_ratings %>% 
                      dplyr::select(team_id, season, team2rating = predictive.RATING),
                    by=c("team2"="team_id", "season"))
  
  return(games)
}
