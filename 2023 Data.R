#Import Data

Games_2023 <- read_csv("~/Blog/College Gameday Project/Games_by_season/2023_Games.csv") %>%
  rename(gameId = Id) %>%
  mutate(Gameday = 0) %>%
  filter(Week < 14)

Rankings_2023 <- read_csv("~/Blog/College Gameday Project/Rankings_by_Season/2023_Rankings.csv") %>%
  filter(Poll %in% c('AP Top 25','Playoff Committee Rankings')) %>%
  filter(!(Week > 9 & Poll == 'AP Top 25')) 

Win_Probabilities_2023 <- read_csv("~/Blog/College Gameday Project/Win_probability_by_season/2023_Probabilities.csv") %>%
  select(GameId, Spread, HomeWinProb) %>%
  mutate(Spread = abs(Spread))

# Convert column names to lowercase, workaround for column name changes

colnames(Games_2023) <- gsub(' ','_',colnames(Games_2023)) %>%
  tolower()

colnames(Rankings_2023) <- gsub(' ','_',colnames(Rankings_2023)) %>%
  tolower()

colnames(Win_Probabilities_2023) <- gsub(' ','_',colnames(Win_Probabilities_2023)) %>%
  tolower()

# Update Rankings dataframe to fit existing function

for (i in seq(2,14)){
  print(i)
  rankings_copy <- Rankings_2023 %>%
    filter(week == 1) %>%
    mutate(week = i)
  
  Rankings_2023 <- bind_rows(Rankings_2023,rankings_copy)
}

Rankings_2023 <- Rankings_2023 %>%
  mutate(rank_id = paste(season,week,school, sep = '')) %>%
  select(season,week,poll,rank,school,points,rank_id)


Win_Probabilities_2023 <- Win_Probabilities_2023 %>%
  rename(gameId = gameid)


test_games_2023 <- Games_2023 %>%
  rename(gameId = gameid) %>%
    cleaning_function(Rankings_2023,Win_Probabilities_2023,CFB_Rivalries) %>%
  rename(homeWinProb = homewinprob,
         Gameday = gameday)


# Imputing

test_games_2023 <- test_games_2023 %>%
  select(Gameday,week,season,home_team,away_team,
         home_rank,away_rank,home_conference,away_conference,
         home_pregame_elo,away_pregame_elo,spread,homeWinProb,
         neutral_site,conference_game,rivalry) %>%
  mutate_at(c('rivalry','conference_game','neutral_site'), 
            as.numeric) %>%
  as.data.frame(stringsAsFactors = FALSE)

test_games_2023[is.na(test_games_2023$home_rank) == TRUE,'home_rank'] <- 0
test_games_2023[is.na(test_games_2023$away_rank) == TRUE,'away_rank'] <- 0

test_games_2023 <- test_games_2023 %>%
  mutate_at(c('home_team','away_team','home_rank','away_rank','Gameday','season','week',
              'neutral_site','conference_game','rivalry'), 
            as.factor)



library(mice)
set.seed(4767)

test_games_2023 <- test_games_2023 %>%
  mutate(home_pregame_elo_na = is.na(home_pregame_elo),
         away_pregame_elo_na = is.na(away_pregame_elo),
         spread_na = is.na(spread),
         homeWinProb_na = is.na(homeWinProb)
  ) 

init = mice(test_games_2023, maxit=0) 

meth = init$method
predM = init$predictorMatrix


predM[,c('Gameday','week','season')] = 0
meth[c('home_pregame_elo','away_pregame_elo','spread','homeWinProb')] <- 'rf'

Imputer <- mice(test_games_2023, method=meth, predictorMatrix=predM, m=5)

test_games_2023_Imputed <- complete(Imputer) %>%
  mutate(elo_combined = home_pregame_elo + away_pregame_elo) 


test_games_2023 <- test_games_2023_Imputed %>%
  filter(!away_conference %in% c('Big South-OVC', 'FCS Independents', 'UAC'))


# Apply existing model

prediction_2023 <- predict(rfFit, test_games_2023, type = 'prob')

prediction_2023

College_Gameday_Model_2023_Results <- bind_cols(test_games_2023,prediction_2023) 


# Save to csv

write.csv(College_Gameday_Model_2023_Results, "College_Gameday_Model_2023_Results.csv", row.names=FALSE)
write.csv(College_Gameday_Model_Test_Results %>%
            filter(Gameday == 'Yes') %>%
            select(Gameday,season,week,home_team,home_rank,away_team,away_rank,spread, Yes), "College_Gameday_Model_Test_Results_Include_Teams_Actual.csv")

# Make Predictions

final_2023_results <- College_Gameday_Model_2023_Results %>%
  group_by(season,week) %>%
  select(Gameday,season,week,home_team,home_rank,away_team,away_rank,spread,Yes)%>%
  arrange(desc(Yes)) %>%
  top_n(1) %>%
  arrange(season,week) %>%
  ungroup()

write.csv(final_2023_results, "College_Gameday_Model_2023_Predictions.csv", row.names=FALSE)
