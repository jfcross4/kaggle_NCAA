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

dolphin_pred <- function(team1rating, team2rating){
  z = team1rating - team2rating
  pnorm(z)
}

pred538 <- function(r1, r2){
  1/(1+ 10^((r2-r1)*30.464/400))
}

home_adj <- function(pred, home){
  odds <- (pred/(1-pred))*1.656^home
  return(odds/(odds+1))
}