add_win_it_all_guess = function(preds, team_name, tournament){
  print(paste("Predicting", team_name, "to win it all!"))
  
  if(tournament=="M"){
    names = read.csv("stage2data/MTeamSpellings.csv")
  }else{
    names = read.csv("stage2data/WTeamSpellings.csv")
  }
  
  teamID = names %>% filter(TeamNameSpelling==tolower(team_name)) %>%
    dplyr::select(TeamID) %>% as.numeric()
  
  try(if(is.na(teamID)) 
    stop(paste(team_name, "was not found in the", tournament, "tournament")))
  
  print(paste(team_name, "is team number", teamID))
  print("Adjusted their games accordingly")
  
  preds[preds$team1==teamID,]$Pred = 1
  preds[preds$team2==teamID,]$Pred = 0
  
  return(preds)
}


#### Add One Team Beats Another Team

add_teamA_beats_teamB_guess = function(preds, team_nameA, team_nameB, tournament){
  if(tournament=="M"){
    names = read.csv("stage2data/MTeamSpellings.csv")
  }else{
    names = read.csv("stage2data/WTeamSpellings.csv")
  }
  
  AteamID = names %>% filter(TeamNameSpelling==tolower(team_nameA)) %>%
    dplyr::select(TeamID) %>% as.numeric()
  
  BteamID = names %>% filter(TeamNameSpelling==tolower(team_nameB)) %>%
    dplyr::select(TeamID) %>% as.numeric()
  
  try(if(is.na(AteamID)) 
    stop(paste(team_nameA, "was not found in the", tournament, "tournament")))
  
  try(if(is.na(BteamID)) 
    stop(paste(team_nameB, "was not found in the", tournament, "tournament")))
  
  print(paste(team_nameA, "is team number", AteamID))
  print(paste(team_nameB, "is team number", BteamID))
  print("Adjusting their match-up accordingly")
  
  if(AteamID < BteamID){
    preds[preds$team1==AteamID & preds$team2==BteamID,]$Pred = 1
  } else{
    preds[preds$team2==AteamID & preds$team1==BteamID,]$Pred = 0
  }
  return(preds)
}
