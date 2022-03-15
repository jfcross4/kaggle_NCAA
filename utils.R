# utility functions

games_to_predict = function(SampleSubmission){
  games.to.predict <- cbind(SampleSubmission$ID, 
                            colsplit(SampleSubmission$ID, 
                                     pattern = "_", 
                                     names = c('season', 'team1', 'team2')))   
  colnames(games.to.predict)[1] <- "ID"
  games.to.predict$home <- 0
  return(games.to.predict)
}


write_submission_file = function(games, name){
  write.csv(games %>% 
              dplyr::select(ID, Pred), 
            file=paste0(name, ".csv"), row.names=FALSE)
}
