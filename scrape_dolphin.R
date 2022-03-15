library(rvest); library(XML); library(RCurl);library(dplyr)

scrape_dolphin = function(link = "http://www.dolphinsim.com/ratings/ncaa_mbb/",
                          spelling_file = "stage2data/MTeamSpellings.csv"){
  

TeamSpellings <- read.csv(spelling_file)



dolphin_link <-
  read_html(link)


dolphintable <- dolphin_link %>% 
  html_node("pre") %>%
  html_text() 


x <- strsplit(dolphintable, split="\n")
x.names <- substr(x[[1]][2:(length(x[[1]])-1)], 0, 25)
x.rest <- substr(x[[1]][2:(length(x[[1]])-1)], 28, 600)
x.rest.split <- strsplit(x.rest, split='\\s+')

values = unlist(x.rest.split)[unlist(x.rest.split)!="P"]
mx <- matrix(values, ncol=26, byrow = TRUE)

names <- x.names
dx <- as.data.frame(mx)
trim.trailing <- function (x) sub("\\s+$", "", x)

dx <- cbind(trim.trailing(names), dx)

colnames(dx) <- unname(as.vector(t(dx[1,])))
dx <- dx[-1, ]
dx.colnames <- c(colnames(dx)[1:7], 
                 paste(c(rep("standard",2), rep("med",2), 
                         rep("predictive",2), rep("improved",2), rep("rpi",2), 
                         rep("pairwise",2), rep("poll",2), rep("scoring",2), 
                         rep("offense",2), rep("defense",2)),
                       colnames(dx)[8:27], sep=".")) 

colnames(dx) <- dx.colnames

dx <- dx %>% dplyr::filter(TEAM !="TEAM")


dx <- dx %>% mutate(TEAM = tolower(TEAM)) %>% 
  dplyr::select(TEAM, predictive.RATING) %>% 
  mutate(predictive.RATING=as.numeric(as.character(predictive.RATING)))

# dx.matched <- inner_join(dx, TeamSpellings, by=c("TEAM"="name_spelling"))
# nrow(dx.matched)
# dx.unmatched <- anti_join(dx, TeamSpellings, by=c("TEAM"="name_spelling"))
# nrow(dx.unmatched)
# dx.unmatched$team_id <- NA
#write.csv(dx.unmatched, 'dx.unmatched.csv')
#dolphin.spellings <- read.csv('dx.unmatched.csv')

#TeamSpellings.plus <- rbind(TeamSpellings, dolphin.spellings %>% select(name_spelling, team_id))
#write.csv(TeamSpellings.plus, 'TeamSpellings.plus.csv', row.names=FALSE)

#write.csv(M_dolphin_matched, 'Mdolphinmatched.csv')
dolphin_matched <- left_join(dx, TeamSpellings, by=c("TEAM"="TeamNameSpelling")) %>%
  filter(predictive.RATING < 5)
return(dolphin_matched)
}