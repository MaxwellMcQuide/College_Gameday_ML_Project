library(rvest)
library(dplyr)

rivalry_link = "https://en.wikipedia.org/wiki/List_of_NCAA_college_football_rivalry_games"
rivalry_page = read_html(rivalry_link)


rivalry_table = rivalry_page %>% html_nodes("table") %>% 
  .[1] %>% 
  html_table(fill = TRUE) %>% .[[1]]

View(rivalry_table)

write.csv(rivalry_table, "CFB_Rivalries.csv", row.names=FALSE)
