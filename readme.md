# Kaggle March Madness Predictions

The following code can be used to create projections for the Men's and Women's Kaggle March Madness Tournament.

# 1. Loading the relevant packages and scripts

```r
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
source("scrape_dolphin_historic.R")
source("match_dolphin_to_games.R")
source("match_538_to_games.R")
```


# 2. Using 538 Team Ratings to Make Projections

A full description of Nate Silver's Methodology is described [here](https://fivethirtyeight.com/features/how-our-march-madness-predictions-work-2/).

## Reading In 538 Ratings for Men's and Womens Teams

The following code reads in the 538's ratings and splits them into Men's and Women's ratings.

```r
x538 <- read.csv("stage2data/fivethirtyeight_ncaa_forecasts.csv")

M538 = x538 %>% filter(gender=="mens")
W538 = x538  %>% filter(gender=="womens")

M538 <- M538 %>% dplyr::select(team_name, team_rating)
W538 <- W538 %>% dplyr::select(team_name, team_rating)
```

## Modifying 538 Ratings

Suppose that I think that the Men's Gonzaga team is underated and should have a rating of 100 instead of 96.47.  I can change their score to 100 using the *modify_538_ratings* function.

```r
M538 = modify_538_ratings(M538, "Gonzaga", 100)
```

Likewise, if I think that the women's Stanford team is over-rated by 538 and should really only have a rating of 95, I use the code:

```r
W538 = modify_538_ratings(W538, "Stanford", 95)
```

Note, that I need to use 538's team name spellings when using this function so take a look at *M538* ann *W538* before using it.

## Matching up 538 ratings with games

Next, we read in the Sample Submission files (with the games we need to project) and match up each game with 538 ratings.

```r
Wgames = match_538_to_games("stage2data/WSampleSubmissionStage2.csv", W538, "W")
Mgames = match_538_to_games("stage2data/MSampleSubmissionStage2.csv", M538, "M")
```

The last step is to make predictions for each game.  We can use the following formula for the change that team 1 (with a rating of r1) beats team 2 (with a rating of r2):

$$prediction\ = \frac{1}{1 + 10^{(r_2 - r_1)\cdot\frac{30.464}{400}}}$$

In R, we create the function...

```r
pred538 <- function(r1, r2){
  1/(1+ 10^((r2-r1)*30.464/400))
}
```

... and then apply this function to each game.

```r
Wgames = Wgames %>% mutate(Pred = pred538(team1rating, team2rating))
Mgames = Mgames %>% mutate(Pred = pred538(team1rating, team2rating))
```

## Adding in Guesses

While added pure guesses is likely to doom your submission to a terrible log loss score, it might also give a chance to win!

I've written two functions to help add guesses.

**add_win_it_all_guess** gives one team a 100% chance to win all of its games.  If this team doesn't win the tournament, you'll be out of luck.  If I want to bank on Stanford winning all of it's games in the women's tournament, I would run the following (note the third parameter is "W", this would be "M" if you are making predictions for the men's tournament):

```r
Wgames = add_win_it_all_guess(Wgames, "Stanford", "W")
```

This function can be used twice in succession, however.  If I think that Stanford will win all of it's games *except* its match-up against South Carolina and South Carolina will win it all, I would run this function twice, first with Stanford and then with South Carolina:

```r
Wgames = add_win_it_all_guess(Wgames, "Stanford", "W")
Wgames = add_win_it_all_guess(Wgames, "South Carolina", "W")
```

The other guessing function *add_teamA_beats_teamB_guess* simply predicts the outcome of one matchup.  If, for instance, I want to predict that Miami beats South Florida in the first round of the women's tournament, I can run the code:

```r
Wgames = add_teamA_beats_teamB_guess(Wgames, "Miami Fl", "South Florida", "W")
```

**Two warnings!**  First, I likely isn't obvious that we need to use "Miami Fl" for Miami.  Please check the *WTeamSpellings.csv* and *MTeamSpellings.csv* files within the stage2data folder to see the acceptable spellings of these team names.  Second, if you've run **all** of the code above, you will have prediction that South Carolina wins it all, Stanford wins except when it faces South Carolina **and** that Miami beats South Florida in the first round.  You can run the code above, to recreate *W538* and *M538* as well as *Wgames* and *Mgames* and start fresh, only making the adjustments you want.

## Creating a .csv File to submit to Kaggle

Running all the code above produces two data frames **Wgames** and **Mgames**.  We can turn these into .csv files that we can submit to Kaggle using the **write_submission_file** function.  The code below takes the *Wgames* predictions and creates a file called "MyAwesomePredictions.csv" which could be submitted directly to Kaggle.

```r
write_submission_file(Wgames, "MyAwesomePredictions") 
```

# 3. Alernate Method: Creating Projections from Scratch

The *cross_ratings* function uses the outcomes and scores of regular season games to predict the outcomes of tournament games. We have to point it to the regular season results and the sample submission file and can tell it how much weight to give to win/loss outcomes and how much weight to give to score differentials.  The following code would give 80% weight to score differential and 20% weight to win/loss outcomes:

```r
compact_results_file = "stage2data/MRegularSeasonCompactResults.csv"
submission_file = "stage2data/MSampleSubmissionStage2.csv"

Mgames <- 
  cross_predictions(compact_results_file = compact_results_file,
                    submission_file = submission_file,
                    weight_scores = 0.8,
                    weight_outcomes = 0.2)

```

I can modify these predictions in the same ways that I modified predictions based on 538 ratings. For example, I can make Gonzaga win it all:

```r
Mgames = add_win_it_all_guess(Mgames, "Gonzaga", "M")

Mgames = add_teamA_beats_teamB_guess(Mgames, "Boise St", "Memphis", "M")

```

I can also also write these to a .csv file the same way:

```r
write_submission_file(Mgames, "AwesomeCrossRatingsPreds") 

```