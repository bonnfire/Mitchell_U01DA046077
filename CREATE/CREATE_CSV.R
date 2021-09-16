## CREATE CSV FILES FOR DATABASE
setwd("~/Desktop/Database/csv files/u01_suzanne_mitchell")

# create csv's for master tables 
write.csv(mitchell_wfu_metadata_c01_05, "~/Desktop/Database/csv files/u01_suzanne_mitchell/mitchell_wfu_metadata_c01_05.csv", row.names = F)
write.csv(mitchell_wfu_metadata_c06, "~/Desktop/Database/csv files/u01_suzanne_mitchell/mitchell_wfu_metadata_c06.csv", row.names = F)


# create csv's for phenotypes 
write.csv(locomotor_gwas_metadata_c01_05, "~/Desktop/Database/csv files/u01_suzanne_mitchell/locomotor_gwas_c01_05.csv", row.names = F)
# write.csv(discounting_gwas_metadata_c01_03, "discounting_gwas_c01_03.csv", row.names = F)
write.csv(mitchell_c01_04_gwas_dd, "~/Desktop/Database/csv files/u01_suzanne_mitchell/dd_gwas_c01_04.csv", row.names = F)

write.csv(locomotor_fecal_phenotypes_df, "~/Desktop/Database/csv files/u01_suzanne_mitchell/locomotor_fecal_c01_05.csv", row.names = F)


## create gwas phenotypes for db
full_join(read.csv("~/Desktop/Database/csv files/u01_suzanne_mitchell/locomotor_gwas_c01_05.csv", stringsAsFactors = F) %>% 
            mutate(rfid = as.character(rfid)),
  read.csv("~/Desktop/Database/csv files/u01_suzanne_mitchell/dd_gwas_c01_04.csv", stringsAsFactors = F) %>% 
    mutate(rfid = as.character(rfid)) %>% 
    setNames(tolower(gsub("(.*)", "dd_\\1", names(.)))) %>% 
    setNames(tolower(gsub("dd_dd", "dd", names(.)))) %>%
    setNames(tolower(gsub("dd_(cohort|rfid|sex)", "\\1", names(.)))) %>% 
    select(-dd_missing_indiff_point,
           -dd_missing_indiff_count)) %>% 
  full_join(read.csv("~/Desktop/Database/csv files/u01_suzanne_mitchell/locomotor_fecal_c01_05.csv", stringsAsFactors = F) %>% 
              mutate(rfid = as.character(rfid)) %>% 
              left_join(read.csv("~/Desktop/Database/csv files/u01_suzanne_mitchell/mitchell_wfu_metadata_c01_05.csv", stringsAsFactors = F) %>% 
                          mutate(rfid = as.character(rfid)) %>% 
                          select(rfid, dob)) %>% 
              mutate(locomotor_fecal_boli_1_age = round(as.numeric(as.Date(date) - as.Date(dob))),
                     locomotor_fecal_boli_2_age = round(as.numeric(as.Date(date_2) - as.Date(dob)))) %>% 
              rename("locomotor_fecal_boli_1" = "fecal_boli",
                     "locomotor_fecal_boli_2" = "fecal_boli_2",
                     "locomotor_fecal_boli_change" = "change") %>% 
              select(cohort, rfid, sex, matches("locomotor_fecal"))) %>% 
  setNames(tolower(gsub("(.*)(_t\\d)$", "locomotor\\2_\\1", names(.)))) %>%
  setNames(tolower(gsub("day_", "t", names(.)))) %>% 
  rename("locomotor_cage" = "cage") %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/generated/gwas_phenotypes.csv", row.names = F)

  

