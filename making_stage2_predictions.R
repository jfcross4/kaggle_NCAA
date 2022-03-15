library(rvest) 
library(XML) 
library(RCurl)
library(dplyr)
library(reshape2)
library(lme4)
library(tidyverse)

source("add_win_it_all_guess.R")
source("cross_ratings.R")
source("scrape_dolphin.R")
source("utils.R")
source("match_dolphin_to_games.R")
source("match_538_to_games.R")
source("modify_538_ratings.R")

##--------------------------------------------------------------

# Using 538 Ratings to Predict Games from Past Seasons

x538 <- read.csv("stage2data/fivethirtyeight_ncaa_forecasts.csv")

M538 = x538 %>% filter(gender=="mens")
W538 = x538  %>% filter(gender=="womens")

M538 <- M538 %>% dplyr::select(team_name, team_rating)
W538 <- W538 %>% dplyr::select(team_name, team_rating)

Wgames = match_538_to_games("stage2data/WSampleSubmissionStage2.csv", W538, "W")
Mgames = match_538_to_games("stage2data/MSampleSubmissionStage2.csv", M538, "M")
Wgames = Wgames %>% mutate(Pred = pred538(team1rating, team2rating))
Mgames = Mgames %>% mutate(Pred = pred538(team1rating, team2rating))


pred538 <- function(r1, r2){
  1/(1+ 10^((r2-r1)*30.464/400))
}


###--------------------
### Adding in Guesses
###---------------------

# Women's first bracket (example)
Wpreds_SCwins = add_win_it_all_guess(Wgames, "South Carolina", "W")

Wpreds_SCwins_NC_beats_Stanf = 
  add_teamA_beats_teamB_guess(Wpreds_SCwins, "NC State", "Stanford", "W")


# Women's second bracket (example)
Wpreds_NCwins = add_win_it_all_guess(Wgames, "NC State", "W")



###--------------------
### Creating Submission Files
###---------------------


write_submission_file(Wpreds_SCwins_NC_beats_Stanf, "Wpreds_SCwins_NC_beats_Stanf") 
write_submission_file(Wpreds_NCwins, "Wpreds_NCwins") 


