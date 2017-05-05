library(httr)
library(jsonlite)
library(dplyr)
library(stats)


system('python pittsburghliens.py')
load.liens <- fromJSON("./pittsburghliens.txt")
liens <- load.liens$result$records
liens$lien_description <- as.factor(liens$lien_description)
liens$pin <- as.factor(liens$pin)
by_pin <- liens %>% 
  group_by(pin) %>%
  summarise(amount = sum(amount), owedto = length(lien_description))
write.csv(by_pin, "./Max_Apps/BEV/Finance/parcel_viewer/liens.csv", row.names = FALSE)
write.csv(by_pin, "/home/linadmin/liens.csv", row.names = FALSE)
