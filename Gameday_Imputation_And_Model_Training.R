library(caret)
library(stringr)

# Convert necessary variables to Factors

Games_ML <- Final_Games %>%
  select(Gameday,week,season,home_team,away_team,
         home_rank,away_rank,home_conference,away_conference,
         home_pregame_elo,away_pregame_elo,spread,homeWinProb,
         neutral_site,conference_game,rivalry) %>%
  mutate_at(c('rivalry','conference_game','neutral_site'), 
            as.numeric) %>%
  as.data.frame(stringsAsFactors = FALSE)

Games_ML[is.na(Games_ML$home_rank) == TRUE,'home_rank'] <- 0
Games_ML[is.na(Games_ML$away_rank) == TRUE,'away_rank'] <- 0
Games_ML[Games_ML$Gameday == 1,'Gameday'] <- 'Yes'
Games_ML[Games_ML$Gameday == 0,'Gameday'] <- 'No'

Games_ML <- Games_ML %>%
  mutate_at(c('home_rank','away_rank','Gameday','season','week',
              'neutral_site','conference_game','rivalry'), 
            as.factor)



# Imputing

library(mice)
set.seed(4767)

Games_ML <- Games_ML %>%
  mutate(home_pregame_elo_na = is.na(home_pregame_elo),
         away_pregame_elo_na = is.na(away_pregame_elo),
         spread_na = is.na(spread),
         homeWinProb_na = is.na(homeWinProb)
         ) 

init = mice(Games_ML, maxit=0) 

meth = init$method
predM = init$predictorMatrix

predM[,c('Gameday','week','season')] = 0
meth[c('home_pregame_elo','away_pregame_elo','spread','homeWinProb')] <- 'rf'


Imputer <- mice(Games_ML, method=meth, predictorMatrix=predM, m=5)

Games_ML_Imputed <- complete(Imputer) %>%
  mutate(elo_combined = home_pregame_elo + away_pregame_elo) 


Games_ML <- Games_ML_Imputed


# Partition

pre_2020 <- Games_ML %>%
  filter(!season %in% c(2020,2021,2022))

test <- Games_ML %>%
  filter(season %in% c(2020,2021,2022)) %>%
  filter(!home_team %in% c('Jacksonville State','James Madison'),
         !away_team %in% c('Jacksonville State','James Madison','Bryant', 'Cal Poly', 'Campbell', 'Hampton',
                           'Kennesaw State', 'Lafayette', 'Lamar', 'Long Island University', 'Mercer',
                           'Missouri State', 'Montana State', 'Norfolk State', 'North Alabama', 'North Dakota',
                           'North Dakota State', 'Northern Colorado', 'Northwestern State', 'Rhode Island',
                           'Robert Morris', 'Samford', 'South Dakota State', 'Southeastern Louisiana', 'St Francis (PA)',
                           'Stony Brook', 'Tarleton State', 'Tennessee State', 'Towson', 'Utah Tech', 'Valparaiso', 'Yale',
                           'Bucknell', 'East Tennessee State', 'Florida A&M', 'Houston Christian', 'Jackson State', 'Monmouth', 
                           'Nicholls', 'Texas Southern', 'The Citadel'),
         !away_conference %in% c('Atlantic Sun', 'AWC', 'Western Athletic'))

gamedays <- pre_2020 %>%
  filter(Gameday == 'Yes')

non_gamedays <- pre_2020 %>%
  filter(Gameday == 'No')

training_index <- createDataPartition(non_gamedays$season, p = .2, list = FALSE)

training <- bind_rows(gamedays, non_gamedays[training_index,])
 

# Model Trianing

training <- na.omit(training)

ctrl <- trainControl(method="repeatedcv",
                     repeats = 3,
                     number = 3,
                     classProbs=TRUE)

rfFit <- train(Gameday ~ #home_team + away_team + 
                 elo_combined + home_conference + away_conference +
                  home_rank + away_rank + home_pregame_elo +
                  away_pregame_elo + spread + homeWinProb + neutral_site +
                  conference_game + rivalry + home_pregame_elo_na +
                  away_pregame_elo_na + spread_na + homeWinProb, 
                data = training, method = "rf", 
                trControl = ctrl)

rfFit

rf_prediction <- predict(rfFit, test, type = 'prob')

College_Gameday_Model_Test_Results <- bind_cols(test,rf_prediction) 

# Save to csv

write.csv(College_Gameday_Model_Test_Results, "College_Gameday_Model_Test_Results_No_Teams.csv", row.names=FALSE)

#Filter to top predictions by week

final_results <- College_Gameday_Model_Test_Results %>%
  group_by(season,week) %>%
  select(Gameday,season,week,home_team,home_rank,away_team,away_rank,spread,Yes)%>%
  arrange(desc(Yes)) %>%
  top_n(1) %>%
  arrange(season,week) %>%
  ungroup()

# Save to csv

# write.csv(final_results, "College_Gameday_Model_Test_Results_No_Teams_Predictions.csv", row.names=FALSE)
# write.csv(College_Gameday_Model_Test_Results %>%
#             filter(Gameday == 'Yes') %>%
#             select(Gameday,season,week,home_team,home_rank,away_team,away_rank,spread, Yes), "College_Gameday_Model_Test_Results_Include_Teams_Actual.csv")








