# create temp summer files 

# name the id's that are compromised 
compromised_rats_summer <- data.frame(rfid = pregnant) %>% 
  mutate(notes = "pregnant", resolution = "FLAG_ALL")

write.csv(compromised_rats_summer, "~/Desktop/Database/csv files/u01_suzanne_mitchell/compromised_rats_summer.csv", row.names = F)