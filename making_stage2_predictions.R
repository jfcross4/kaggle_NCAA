### --------------
### 1. Loading the relevant packages and scripts
### ---------------

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
# Using 538 Ratings to Make Projections
##--------------------------------------------------------------

x538 <- read.csv("stage2data/fivethirtyeight_ncaa_forecasts.csv")

M538 = x538 %>% filter(gender=="mens")
W538 = x538  %>% filter(gender=="womens")

M538 <- M538 %>% dplyr::select(team_name, team_rating)
W538 <- W538 %>% dplyr::select(team_name, team_rating)

##--------------------------------------------------------------
# Modifying 538 Ratings
##--------------------------------------------------------------

## Examples!  You may not want to run this:
M538 = modify_538_ratings(M538, "Gonzaga", 100)

W538 = modify_538_ratings(W538, "Stanford", 95)

##--------------------------------------------------------------
# Matching up 538 ratings with games
##--------------------------------------------------------------


Wgames = match_538_to_games("stage2data/WSampleSubmissionStage2.csv", W538, "W")
Mgames = match_538_to_games("stage2data/MSampleSubmissionStage2.csv", M538, "M")

pred538 <- function(r1, r2){
  1/(1+ 10^((r2-r1)*30.464/400))
}

Wgames = Wgames %>% mutate(Pred = pred538(team1rating, team2rating))
Mgames = Mgames %>% mutate(Pred = pred538(team1rating, team2rating))


###--------------------
### Adding in Guesses
###---------------------

# Examples!  You may not want to run this.

Wgames = add_win_it_all_guess(Wgames, "Stanford", "W")

#or#

Wgames = add_win_it_all_guess(Wgames, "Stanford", "W")
Wgames = add_win_it_all_guess(Wgames, "South Carolina", "W")

#or#

Wgames = add_teamA_beats_teamB_guess(Wgames, "Miami Fl", "South Florida", "W")

###--------------------
### Creating a .csv File to submit to Kaggle
###---------------------

write_submission_file(Wgames, "MyAwesomePredictions") 

###--------------------
# 3. Alternate Method: Creating Projections from Scratch
###---------------------

compact_results_file = "stage2data/MRegularSeasonCompactResults.csv"
submission_file = "stage2data/MSampleSubmissionStage2.csv"

Mgames <- 
  cross_predictions(compact_results_file = compact_results_file,
                    submission_file = submission_file,
                    weight_scores = 0.8,
                    weight_outcomes = 0.2)

### Example: you may not want to run these
Mgames = add_win_it_all_guess(Mgames, "Gonzaga", "M")
Mgames = add_teamA_beats_teamB_guess(Mgames, "Boise St", "Memphis", "M")

write_submission_file(Mgames, "AwesomeCrossRatingsPreds") 

###---------------------
# 4. Alternate Method (Men's Tournament): Scraping Dolphin Ratings
###---------------------

dolphin_ratings <- scrape_dolphin()
submission_file = "stage2data/MSampleSubmissionStage2.csv"
Mgames = match_dolphin_to_games(submission_file, dolphin_ratings)

dolphin_pred <- function(team1rating, team2rating){
  z = team1rating - team2rating
  pnorm(z)
}

Mgames = Mgames %>% 
  mutate(Pred = dolphin_pred(team1rating, team2rating))
