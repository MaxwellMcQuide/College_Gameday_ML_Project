cleaning_function <- function(games,rankings,probabilities,rivalries = CFB_Rivalries){
  
  # Beginning to Merge Datasets (use by = 'gameid' for 2023 Data and 'gameId' for all other data)
  
  games <- left_join(games,probabilities, by = "gameId")
                                          #by = 'gameid')
  
  # Rankings Merge
  games <- games %>%
    mutate(home_rank_id = paste(season,week,home_team, sep = ''),
           away_rank_id = paste(season,week,away_team, sep = '')) %>%
    left_join(rankings %>%
                select(rank_id,rank) %>%
                rename(home_rank_id = rank_id,
                       home_rank = rank),
              by = 'home_rank_id') %>%
    left_join(rankings %>%
                select(rank_id,rank) %>%
                rename(away_rank_id = rank_id,
                       away_rank = rank),
              by = 'away_rank_id') %>%
    select(-c(home_rank_id,away_rank_id))
  
  # Rivalries
  
  
  # Remove accent in José
  
  games[games$home_team == "San José State","home_team"] <- 'San Jose State'
  games[games$away_team == "San José State","away_team"] <- 'San Jose State'
  
  # Adding Rivalries
  
  games <- games %>%
    rowwise() %>%
    mutate(teams = as.character(list(sort(c(home_team,away_team))))) %>%
    mutate(rivalry = ifelse(teams %in% CFB_Rivalries$teams,TRUE,FALSE)) %>%
    select(-c(teams))
  
  return(games)
  
}

Final_Games <- cleaning_function(Games,Rankings,Probabilities,CFB_Rivalries)

# Updating all of the College Gameday Locations

  # 2022

Final_Games[Final_Games$season == 2022 & Final_Games$week == 1 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 2 & Final_Games$home_team == 'Texas' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 3 & Final_Games$home_team == 'Appalachian State' & Final_Games$away_team == 'Troy','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 4 & Final_Games$home_team == 'Tennessee' & Final_Games$away_team == 'Florida','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 5 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'NC State','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 6 & Final_Games$home_team == 'Kansas' & Final_Games$away_team == 'TCU','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 7 & Final_Games$home_team == 'Tennessee' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 8 & Final_Games$home_team == 'Oregon' & Final_Games$away_team == 'UCLA','Gameday'] <- 1

Final_Games[Final_Games$season == 2022 & Final_Games$week == 10 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Tennessee','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 11 & Final_Games$home_team == 'Texas' & Final_Games$away_team == 'TCU','Gameday'] <- 1

Final_Games[Final_Games$season == 2022 & Final_Games$week == 13 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2022 & Final_Games$week == 14 & Final_Games$home_team == 'TCU' & Final_Games$away_team == 'Kansas State','Gameday'] <- 1

# 2021

Final_Games[Final_Games$season == 2021 & Final_Games$week == 1 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Georgia','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 2 & Final_Games$home_team == 'Iowa State' & Final_Games$away_team == 'Iowa','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 3 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 4 & Final_Games$home_team == 'Wisconsin' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 5 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Arkansas','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 6 & Final_Games$home_team == 'Texas' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 7 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Kentucky','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 8 & Final_Games$home_team == 'UCLA' & Final_Games$away_team == 'Oregon','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 9 & Final_Games$home_team == 'Michigan State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 10 & Final_Games$home_team == 'Cincinnati' & Final_Games$away_team == 'Tulsa','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 11 & Final_Games$home_team == 'Ole Miss' & Final_Games$away_team == 'Texas A&M','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 12 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Michigan State','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 13 & Final_Games$home_team == 'Michigan' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2021 & Final_Games$week == 14 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Georgia','Gameday'] <- 1

# 2020


Final_Games[Final_Games$season == 2020 & Final_Games$week == 2 & Final_Games$home_team == 'Wake Forest' & Final_Games$away_team == 'Clemson','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 3 & Final_Games$home_team == 'Louisville' & Final_Games$away_team == 'Miami','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 4 & Final_Games$home_team == 'Miami' & Final_Games$away_team == 'Florida State','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 5 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 6 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Miami','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 7 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Georgia','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 8 & Final_Games$home_team == 'Minnesota' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 9 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 10 & Final_Games$home_team == 'Notre Dame' & Final_Games$away_team == 'Clemson','Gameday'] <- 1

Final_Games[Final_Games$season == 2020 & Final_Games$week == 12 & Final_Games$home_team == 'Oklahoma' & Final_Games$away_team == 'Oklahoma State','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 13 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2020 & Final_Games$week == 14 & Final_Games$home_team == 'Coastal Carolina' & Final_Games$away_team == 'BYU','Gameday'] <- 1

# 2019

Final_Games[Final_Games$season == 2019 & Final_Games$week == 1 & Final_Games$home_team == 'Auburn' & Final_Games$away_team == 'Oregon','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 2 & Final_Games$home_team == 'Texas' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 3 & Final_Games$home_team == 'Iowa State' & Final_Games$away_team == 'Iowa','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 4 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 5 & Final_Games$home_team == 'Nebraska' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 6 & Final_Games$home_team == 'Florida' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 7 & Final_Games$home_team == 'LSU' & Final_Games$away_team == 'Florida','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 8 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1

Final_Games[Final_Games$season == 2019 & Final_Games$week == 10 & Final_Games$home_team == 'Memphis' & Final_Games$away_team == 'SMU','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 11 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 12 & Final_Games$home_team == 'Baylor' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 13 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Penn State','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 14 & Final_Games$home_team == 'Minnesota' & Final_Games$away_team == 'Wisconsin','Gameday'] <- 1
Final_Games[Final_Games$season == 2019 & Final_Games$week == 15 & Final_Games$home_team == 'LSU' & Final_Games$away_team == 'Georgia','Gameday'] <- 1

# 2018

Final_Games[Final_Games$season == 2018 & Final_Games$week == 1 & Final_Games$home_team == 'Notre Dame' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 2 & Final_Games$home_team == 'Texas A&M' & Final_Games$away_team == 'Clemson','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 3 & Final_Games$home_team == 'TCU' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 4 & Final_Games$home_team == 'Oregon' & Final_Games$away_team == 'Stanford','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 5 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 6 & Final_Games$home_team == 'Oklahoma' & Final_Games$away_team == 'Texas','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 7 & Final_Games$home_team == 'Michigan' & Final_Games$away_team == 'Wisconsin','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 8 & Final_Games$home_team == 'Washington State' & Final_Games$away_team == 'Oregon','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 9 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Florida','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 10 & Final_Games$home_team == 'LSU' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 11 & Final_Games$home_team == 'Boston College' & Final_Games$away_team == 'Clemson','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 12 & Final_Games$home_team == 'UCF' & Final_Games$away_team == 'Cincinnati','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 13 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2018 & Final_Games$week == 14 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'Alabama','Gameday'] <- 1

# 2017

Final_Games[Final_Games$season == 2017 & Final_Games$week == 1 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Florida State','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 2 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 3 & Final_Games$home_team == 'Louisville' & Final_Games$away_team == 'Clemson','Gameday'] <- 1

Final_Games[Final_Games$season == 2017 & Final_Games$week == 5 & Final_Games$home_team == 'Virginia Tech' & Final_Games$away_team == 'Clemson','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 6 & Final_Games$home_team == 'TCU' & Final_Games$away_team == 'West Virginia','Gameday'] <- 1

Final_Games[Final_Games$season == 2017 & Final_Games$week == 8 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 9 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Penn State','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 10 & Final_Games$home_team == 'Oklahoma State' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 11 & Final_Games$home_team == 'Miami' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 12 & Final_Games$home_team == 'Wisconsin' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 13 & Final_Games$home_team == 'Auburn' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2017 & Final_Games$week == 14 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Miami','Gameday'] <- 1

# 2016

Final_Games[Final_Games$season == 2016 & Final_Games$week == 1 & Final_Games$home_team == 'Wisconsin' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 2 & Final_Games$home_team == 'Tennessee' & Final_Games$away_team == 'Virginia Tech','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 3 & Final_Games$home_team == 'Louisville' & Final_Games$away_team == 'Florida State','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 4 & Final_Games$home_team == 'Tennessee' & Final_Games$away_team == 'Florida','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 5 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Louisville','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 6 & Final_Games$home_team == 'Texas A&M' & Final_Games$away_team == 'Tennessee','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 7 & Final_Games$home_team == 'Wisconsin' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 8 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Texas A&M','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 9 & Final_Games$home_team == 'Utah' & Final_Games$away_team == 'Washington','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 10 & Final_Games$home_team == 'LSU' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 11 & Final_Games$home_team == 'Washington' & Final_Games$away_team == 'USC','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 12 & Final_Games$home_team == 'Western Michigan' & Final_Games$away_team == 'Buffalo','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 13 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Michigan','Gameday'] <- 1
Final_Games[Final_Games$season == 2016 & Final_Games$week == 14 & Final_Games$home_team == 'Penn State' & Final_Games$away_team == 'Wisconsin','Gameday'] <- 1

# 2015

Final_Games[Final_Games$season == 2015 & Final_Games$week == 1 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Wisconsin','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 2 & Final_Games$home_team == 'Michigan State' & Final_Games$away_team == 'Oregon','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 3 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Ole Miss','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 4 & Final_Games$home_team == 'Arizona' & Final_Games$away_team == 'UCLA','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 5 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 6 & Final_Games$home_team == 'Utah' & Final_Games$away_team == 'California','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 7 & Final_Games$home_team == 'Michigan' & Final_Games$away_team == 'Michigan State','Gameday'] <- 1

Final_Games[Final_Games$season == 2015 & Final_Games$week == 9 & Final_Games$home_team == 'Temple' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 10 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 11 & Final_Games$home_team == 'Baylor' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 12 & Final_Games$home_team == 'Ohio State' & Final_Games$away_team == 'Michigan State','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 13 & Final_Games$home_team == 'Oklahoma State' & Final_Games$away_team == 'Oklahoma','Gameday'] <- 1
Final_Games[Final_Games$season == 2015 & Final_Games$week == 14 & Final_Games$home_team == 'Iowa' & Final_Games$away_team == 'Michigan State','Gameday'] <- 1

# 2014

Final_Games[Final_Games$season == 2014 & Final_Games$week == 1 & Final_Games$home_team == 'Oklahoma State' & Final_Games$away_team == 'Florida State','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 2 & Final_Games$home_team == 'Oregon' & Final_Games$away_team == 'Michigan State','Gameday'] <- 1

Final_Games[Final_Games$season == 2014 & Final_Games$week == 4 & Final_Games$home_team == 'Florida State' & Final_Games$away_team == 'Clemson','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 5 & Final_Games$home_team == 'South Carolina' & Final_Games$away_team == 'Missouri','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 6 & Final_Games$home_team == 'Ole Miss' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 7 & Final_Games$home_team == 'Mississippi State' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 8 & Final_Games$home_team == 'Florida State' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 9 & Final_Games$home_team == 'LSU' & Final_Games$away_team == 'Ole Miss','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 10 & Final_Games$home_team == 'West Virginia' & Final_Games$away_team == 'TCU','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 11 & Final_Games$home_team == 'Michigan State' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 12 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Mississippi State','Gameday'] <- 1

Final_Games[Final_Games$season == 2014 & Final_Games$week == 14 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'Auburn','Gameday'] <- 1
Final_Games[Final_Games$season == 2014 & Final_Games$week == 15 & Final_Games$home_team == 'Baylor' & Final_Games$away_team == 'Kansas State','Gameday'] <- 1

# 2013

Final_Games[Final_Games$season == 2013 & Final_Games$week == 1 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Georgia','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 2 & Final_Games$home_team == 'Michigan' & Final_Games$away_team == 'Notre Dame','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 3 & Final_Games$home_team == 'Texas A&M' & Final_Games$away_team == 'Alabama','Gameday'] <- 1

Final_Games[Final_Games$season == 2013 & Final_Games$week == 5 & Final_Games$home_team == 'Georgia' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 6 & Final_Games$home_team == 'Northwestern' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 7 & Final_Games$home_team == 'Washington' & Final_Games$away_team == 'Oregon','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 8 & Final_Games$home_team == 'Clemson' & Final_Games$away_team == 'Florida State','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 9 & Final_Games$home_team == 'Oregon' & Final_Games$away_team == 'UCLA','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 10 & Final_Games$home_team == 'Florida State' & Final_Games$away_team == 'Miami','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 11 & Final_Games$home_team == 'Alabama' & Final_Games$away_team == 'LSU','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 12 & Final_Games$home_team == 'USC' & Final_Games$away_team == 'Stanford','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 13 & Final_Games$home_team == 'Oklahoma State' & Final_Games$away_team == 'Baylor','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 14 & Final_Games$home_team == 'Auburn' & Final_Games$away_team == 'Alabama','Gameday'] <- 1
Final_Games[Final_Games$season == 2013 & Final_Games$week == 15 & Final_Games$home_team == 'Michigan State' & Final_Games$away_team == 'Ohio State','Gameday'] <- 1

# Optional: Remove all datasets besides Final_Games

# rm(list=setdiff(ls(), "Final_Games"))

