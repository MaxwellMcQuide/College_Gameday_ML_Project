library(caret)
View(Final_Games_2022)

dummy_test <- dummyVars(spread ~ .,data = Final_Games_2022)
View(dummy_test)


head(predict(dummy_test, newdata = Final_Games_2022)) %>%
  View()
