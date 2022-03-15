
### Strategy: 
# 1. Use 538's predictions for first round games
# 2. Use 538's team ratings to predict games in other rounds
# 3. Adjust second round games for home field advantage
# 4. Give Baylor 100% to win all games
# 5. Have one bracket where Chicago beats Albany and another where Albany beats Chicago


## Step 1: Adding Team ID's to 538's NCAA Tournament Ratings

library(dplyr);library(reshape)

five <- read.csv("/home/jcross/MarchMadness/data/stage2/fivethirtyeight_ncaa_forecasts_womens.csv")

WTeamSpellings <- read.csv("/home/jcross/MarchMadness/data/WTeamSpellings.csv")

five_w_id <- left_join(five %>% mutate(name = tolower(team_name)) %>% rename(team_id_538 = team_id), 
                       WTeamSpellings %>% rename(team_id = TeamID), by=c("name"="TeamNameSpelling"))

write.csv(five_w_id, "/home/jcross/MarchMadness/data/stage2/fivethirtyeight_ncaa_forecasts_womens_ids.csv")

## Step 2: Determine the round in which every game would be played

Teams <- read.csv('/home/jcross/MarchMadness/data/stage2/WTeams.csv')
Seeds <- read.csv('/home/jcross/MarchMadness/data/stage2/WNCAATourneySeeds.csv')
SeedRoundSlots <- read.csv('/home/jcross/MarchMadness/data/stage2/WNCAATourneySlots.csv')
SeedRoundSlots <- read.csv('/home/jcross/MarchMadness/data/stage2/NCAATourneySeedRoundSlots.csv')
SampleSubmission <- read.csv('/home/jcross/MarchMadness/data/stage2/WSampleSubmissionStage2.csv')


games.to.predict <- cbind(SampleSubmission$ID, 
          colsplit(SampleSubmission$ID, split = "_", names = c('season', 'team1', 'team2')))   
colnames(games.to.predict)[1] <- "ID"


SRSjoin <- left_join(SeedRoundSlots, SeedRoundSlots %>% select(GameSlot, Seed), by="GameSlot") %>%
  filter(Seed.x != Seed.y)

temp <- left_join(games.to.predict, Seeds, by=c("team1"="TeamID", "season"="Season"))
games.to.predict.Seeds <- left_join(temp, Seeds, by=c("team2"="TeamID", "season"="Season"))
head(games.to.predict.Seeds)

games.to.predict.SeedsRounds <- left_join(games.to.predict.Seeds, SRSjoin, by=c("Seed.x"="Seed.x", "Seed.y"="Seed.y"))

games.to.predict.with.rounds <- games.to.predict.SeedsRounds %>% 
  group_by(ID) %>% summarize(GameRound = min(GameRound)) %>% ungroup()

games.to.predict.SeedsRounds %>% rename(seed1 = Seed.x, seed2 = Seed.y) %>% 
  select(-EarlyDayNum, -LateDayNum )

write.csv(games.to.predict.with.rounds, 'Wgame_round.csv', row.names=FALSE)

## Step 3: Make Predictions

### The Formula for using 538's ELO ratings:
pred538 <- function(r1, r2){
  1/(1+ 10^((r2-r1)*30.464/400))
}


five <- read.csv("/home/jcross/MarchMadness/data/stage2/fivethirtyeight_ncaa_forecasts_womens_ids.csv")
rounds <- read.csv("/home/jcross/MarchMadness/data/stage2/Wgame_round.csv")
SampleSubmission <- read.csv('/home/jcross/MarchMadness/data/stage2/WSampleSubmissionStage2.csv')
Teams <- read.csv('/home/jcross/MarchMadness/data/stage2/WTeams.csv')
Seeds <- read.csv('/home/jcross/MarchMadness/data/stage2/WNCAATourneySeeds.csv')



games.to.predict <- cbind(SampleSubmission$ID, colsplit(SampleSubmission$ID, split = "_", names = c('season', 'team1', 'team2')))   
colnames(games.to.predict)[1] <- "ID"
head(games.to.predict)

games.to.predict <- left_join(games.to.predict, rounds, by="ID")

team1_538 <- five %>% select(team1 = team_id, team1name = name, team1_rd1_win = rd2_win, team1_rating = team_rating, team1_rd0_win=rd1_win,
                             team1_region = team_region)
team2_538 <- five %>% select(team2 = team_id, team2name = name, team2_rd1_win = rd2_win, team2_rating = team_rating, team2_rd0_win=rd1_win,
                             team2_region = team_region)

games.to.predict <- left_join(games.to.predict, team1_538, by=c("team1"))
games.to.predict <- left_join(games.to.predict, team2_538, by=c("team2"))




games.to.predict <- games.to.predict %>% mutate(pred538_ratings = pred538(team1_rating, team2_rating))



Seeds <- Seeds %>% filter(Season==2019) %>% mutate(SeedNum = as.numeric(substr(Seed,2,3)))

games.to.predict <- left_join(games.to.predict, Seeds %>% 
                                dplyr::rename(team1seed = SeedNum) %>% select(TeamID, team1seed), by=c("team1"="TeamID"))

games.to.predict <- left_join(games.to.predict, Seeds %>% 
                                dplyr::rename(team2seed = SeedNum) %>% select(TeamID, team2seed), by=c("team2"="TeamID"))


games.to.predict <- games.to.predict %>% 
  mutate(Pred = ifelse(GameRound==1&team1_rd0_win==1&team2_rd0_win==1,
                       team1_rd1_win, pred538_ratings))


### Adding HFA to second round games

home_adj <- function(pred, home){
  odds <- (pred/(1-pred))*1.656^home
  return(odds/(odds+1))
}

games.to.predict <- games.to.predict %>% mutate(
  home = ifelse(GameRound==2 & team1seed <= 4, 1, 0),
  home = ifelse(GameRound==2 & team2seed <= 4, -1, home)
)

games.to.predict <- games.to.predict %>% mutate(Pred = home_adj(Pred, home))

womens_bracket_1 <- games.to.predict
womens_bracket_2 <- games.to.predict


### Picking Baylor to win all of its games

womens_bracket_1 <- womens_bracket_1 %>% mutate(pred_plus_pick = ifelse(team1name == "baylor", 1, Pred),
                                                pred_plus_pick = ifelse(team2name == "baylor", 0, pred_plus_pick)
)

womens_bracket_1  %>% ggplot(aes(Pred, pred_plus_pick))+geom_point()


womens_bracket_2 <- 
  womens_bracket_2 %>% mutate(pred_plus_pick = ifelse(team1name == "baylor", 1, Pred),
                                                pred_plus_pick = ifelse(team2name == "baylor", 0, pred_plus_pick)
)

womens_bracket_2  %>% ggplot(aes(Pred, pred_plus_pick))+geom_point()


# Bracket 1:
# Chicago beats Albany

womens_bracket_1 %>% filter(team1_region=="Chicago" & team2_region=="Albany")


womens_bracket_1 <- womens_bracket_1 %>% mutate(pred_plus_pick = ifelse(team1_region=="Chicago" & team2_region=="Albany", 1, pred_plus_pick),
                                                pred_plus_pick = ifelse(team1_region=="Albany" & team2_region=="Chicago", 0, pred_plus_pick)
)

# Bracket 2:
# Albany beats Chicago

womens_bracket_2 <- womens_bracket_2 %>% mutate(pred_plus_pick = ifelse(team1_region=="Chicago" & team2_region=="Albany", 0, pred_plus_pick),
                                                pred_plus_pick = ifelse(team1_region=="Albany" & team2_region=="Chicago", 1, pred_plus_pick)
)


write.csv(womens_bracket_1 %>% select(ID, Pred=pred_plus_pick), "BaylorWins_Chicago_beats_Albany.csv", row.names = FALSE)
write.csv(womens_bracket_2 %>% select(ID, Pred=pred_plus_pick), "BaylorWins_Albany_beats_Chicago.csv", row.names = FALSE)
