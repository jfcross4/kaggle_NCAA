modify_538_ratings <- function(ratings, team_name, new_rating){
  ratings[ratings$team_name==team_name,]$team_rating = new_rating
  return(ratings)
}