# compact_results_file = "WRegularSeasonCompactResults.csv"
# submission_file = "WSampleSubmissionStage1.csv"
# 
# 
# 
# weight_scores = 0.8
# weight_outcomes = 0.2

###

cross_predictions <- 
  function(compact_results_file, 
           submission_file,
           weight_scores, 
           weight_outcomes){

source("utils.R")
results = read.csv(compact_results_file)

sub1 <- results %>% 
  mutate(team1=as.factor(WTeamID), team2=as.factor(LTeamID), outcome=1,
         home = case_when(
           WLoc == "H" ~ 1,
           WLoc ==  "A" ~ -1,
           WLoc == "N" ~ 0
         ),
         margin = WScore - LScore) %>% select(Season, team1, team2, home, outcome, margin)


sub2 <- results %>% 
  mutate(team1=as.factor(LTeamID), team2=as.factor(WTeamID), outcome=0,
         home = case_when(
           WLoc == "H" ~ -1,
           WLoc ==  "A" ~ 1,
           WLoc == "N" ~ 0
         ),
         margin = LScore - WScore) %>% select(Season, team1, team2, home, outcome, margin)

reg_results <- rbind(sub1, sub2)

games = games_to_predict(read.csv(submission_file))

games$outcome_based_pred <- NA
games$score_based_pred <- NA

for(season in unique(games$season)){
  print(season)
  mlogistic <- glmer(outcome ~ home +  (1 | team1) + 
                 (1 | team2), data = reg_results %>% 
                 filter(Season==season), family = binomial) 
  
  games[games$season==season,"outcome_based_pred"] <- 
    predict(mlogistic, games[games$season==season,], 
            type="response")
  
  mlinear <- lmer(margin ~ home +  (1 | team1) + 
                    (1 | team2), data = reg_results %>% 
                    filter(Season==season)) 
  
  standard_error = sd(resid(mlinear))
  pred_scores = predict(mlinear, games[games$season==season,], type="response")
  z_pred_scores = pred_scores/standard_error
  games[games$season==season,"score_based_pred"] <- 
    pnorm(z_pred_scores)
  
}



games = games %>% mutate(
  Pred = (outcome_based_pred^weight_outcomes)*
    (score_based_pred^weight_scores))

return(games)
  }


 
