# RAW QC AND EDITS 

library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)

### create phenotypes tables by joining data together



# locomotor
# fix the raw data before summarizing and spreading
# using the data edits document for Shipment 1 (in Dropbox)
c01_locomotor_df <- locomotor_raw_df %>%
  subset(cohort == "C01") %>% 
  mutate(rfid = replace(rfid, rfid == "933000320045912"&experiment =="U01-t1-gp11", "933000320045915"),
         rfid = replace(rfid, rfid == "933000320046501"&experiment =="U01-t1-gp17", "933000320046051"),
         rfid = replace(rfid, rfid == "933000320046059"&experiment %in% c("U01-t1-gp6", "U01-t2-gp6"), "933000320046057"),
         rfid = replace(rfid, rfid == "933000320046195"&experiment == "U01-t1-gp20", "933000320046196")) 

c02_locomotor_df <- locomotor_raw_df %>%
  subset(cohort == "C02") %>% 
  mutate(rfid = replace(rfid, rfid == "933000320046353"&experiment =="U01-t1-gp25", "933000320046343"))

c03_locomotor_df <- locomotor_raw_df %>%
  subset(cohort == "C03")  %>% 
  mutate(experiment = replace(experiment, experiment == "U01-t3-gp1", "U01-t1-gp1")) %>% 
  mutate(rfid = replace(rfid, rfid == "933000320046896"&experiment =="U01-t2-gp8", "933000320046986"),
         rfid = replace(rfid, rfid == "933000320037769"&experiment =="U01-t2-gp20", "933000320047769"),
         rfid = replace(rfid, rfid == "933000320046800"&experiment == "U01-t2-gp18", "933000320047800")) 

c04_locomotor_df <- locomotor_raw_df %>%
  subset(cohort == "C04")  %>% 
  mutate(comment = "NA") %>% 
  mutate(comment = replace(comment, 
                           rfid %in% c("47724", "47723", "47974", "47530"), 
                           "Severe seizure phenotypes, might EXCLUDE FROM ANALYSIS"))
  
  
c05_locomotor_df <- locomotor_raw_df %>%
  subset(cohort == "C05")  %>% 
  mutate(experiment = replace(experiment, experiment == "U01-t1-gp2a", "U01-t1-gp2"),
         experiment = replace(experiment, experiment == "U01-t2a-gp21", "U01-t1-gp21"),
         experiment = replace(experiment, experiment == "U01-t2a-gp22", "U01-t1-gp22")) 


locomotor_raw_fix_c01_05 <- bind_rows(c01_locomotor_df, c02_locomotor_df) %>% 
  bind_rows(c03_locomotor_df) %>% 
  bind_rows(c04_locomotor_df) %>% 
  bind_rows(c05_locomotor_df)


  

### GWAS phenotypes

locomotor_gwas <- locomotor_raw_fix_c01_05 %>% 
  mutate(time = str_match(experiment, "U01-(.*?)-.*")[,2]) %>% # after manually fixing above, repopulate the column
  select(cohort, rfid, time, 
         # group, 
         cage, total_distance_cm, rest_time_s, rest_episode_count, 
         movement_episode_count, vertical_activity_count, center_time_legacy_s, comment) %>% 
  group_by(cohort, rfid, time, 
           # group, 
           comment, cage) %>% # after fixes, regroup
  summarize_if(is.numeric, sum, na.rm = T) %>% 
  ungroup() %>% 
  pivot_wider(names_from = time, 
              values_from = c(total_distance_cm, rest_time_s, rest_episode_count, 
                              movement_episode_count, vertical_activity_count, center_time_legacy_s)) %>% 
  mutate(cage = parse_number(cage))

# join the metadata (box and age)
locomotor_gwas_metadata_c01_05 <- locomotor_gwas %>% 
  left_join(locomotor_metadata_c01_05 %>% 
              select(rfid, 
                     # locomotor_testing_cage, # drop box for now, XX 10/13/2020 resolve these two locomotor_gwas_metadata %>% subset(cage != locomotor_testing_cage) %>% View() when they respond
                     matches("day")), by = "rfid") %>% # add box and dates
  left_join(mitchell_wfu_metadata_c01_05[, c("sex", "rfid", "dob")], by = c("rfid")) %>% # add sex, dob
  mutate_at(vars(matches("locomotor_day_\\d")), list(age = ~difftime(., dob, units = "days") %>% as.numeric)) %>% 
  select(-dob, -matches("locomotor_day_\\d$")) %>% 
  mutate(cage = as.character(cage)) %>% 
  subset(!rfid %in% c("933000320186695", "933000320187713")) # rfid's don't match as the ones in the wfu metadata, waiting for deborah's and katie's responses
  






## GWAS DELAYED DISCOUNTING (after suzanne post nida meeting)
mitchell_c01_04_gwas_dd <- mitchell_c01_04_dd_xl_df %>% 
  left_join(dd_metadata_c01_04, by = "rfid")

# after checking that operant_box == dd_operant_box and computer is proxy for box_color
mitchell_c01_04_gwas_dd <- mitchell_c01_04_gwas_dd %>% 
  rename("auc_trad" = "auc_traditional", 
         "auc_norm0" = "auc_normal0s",
         "hyperbolic_lnk" = "hyperbolic_ln_k") %>% 
  select(cohort, rfid, sex, dd_squad_number, dd_computer, dd_operant_box, dd_assigned_lever, s_or_ns, hyperbolic_k, hyperbolic_lnk, hyperbolic_b, auc_trad, auc_norm0, quasi_h_k_beta, quasi_h_s_delta)




### GWAS delayed discounting (before suzanne post nida meeting)
# discounting_c01_03 <- rbind(discountingvalidtraits, 
#                             discountingvalidtraits_c03)
# discounting_gwas <- discounting_c01_03 %>%
#   left_join(mitchell_wfu_metadata_c01_05 %>% 
#               mutate(subject = gsub(".*(\\d{5})$", "\\1", rfid)) %>% 
#               select(one_of("cohort", "subject", "sex", "rfid", "dob")), by = c("subject")) %>% # add sex, dob
#   select(-one_of("filename", "time", "date", "subject")) %>% 
#   mutate(delay = as.character(delay)) %>% 
#   group_by(cohort, rfid, sex, delay) %>% 
#   summarize_if(is.numeric, mean) %>% 
#   ungroup() %>% 
#   rename_if(is.numeric, ~ paste0(., "_mean")) %>% 
#   pivot_wider(names_from = delay, 
#               values_from = matches("_mean"))
# 
# discounting_gwas_metadata_c01_03 <- discounting_gwas %>% 
#   full_join(mitchell_macro_summary_extremes_xl_df_wide, by = c("cohort", "rfid", "sex")) %>% # add mitchell extreme variables
#   full_join(mitchell_macro_summary_noextremes_xl_df_wide, by = c("cohort", "rfid", "sex", "dob")) %>% # add mitchell extreme variables
#   left_join(dd_metadata_c01_04, by = "rfid") %>% # add wfu and mitchell metadata
#   left_join(discounting_c01_03 %>%
#               left_join(mitchell_wfu_metadata_c01_05 %>% 
#                           mutate(subject = gsub(".*(\\d{5})$", "\\1", rfid)) %>% 
#                           select(one_of("cohort", "subject", "sex", "rfid", "dob")), 
#                         by = c("subject")) %>% 
#               arrange(date) %>% 
#               select(rfid, dob, date, delay) %>% 
#               group_by(rfid, delay) %>% 
#               slice(1) %>% 
#               ungroup() %>% 
#               mutate(age = difftime(date, dob, units = "days") %>% as.numeric(), 
#                      delay = paste0("age_", delay)) %>% 
#               select(-dob, -date) %>% 
#               pivot_wider(names_from = delay, values_from = age), 
#             by = "rfid") %>% # add date 
#   select(-dob)





##################### 

mitchell_discounting_excel %>% dim
discountingvalidtraits %>% dim

left_join(discountingvalidtraits, mitchell_discounting_excel) %>% dim
left_join(mitchell_discounting_excel, discountingvalidtraits) %>% dim

discountingvalidtraits_rep <- cbind(discountingvalidtraits %>% arrange(subject, date), ship1_2_bind) # assign the delay and rep according to the date discountingvalidtraits_rep %>% subset(subject == "46260") %>% select(delay, rep) %>% table() and for 46266 BOTH WORK!!!! 
discountingvalidtraits_rep 


left_join(mitchell_discounting_excel, discountingvalidtraits) %>% 
  geom_path(aes(x = rep, y = avg_rxn_time_free, color = subject)) + 
  facet_grid(rows = vars(cohort), cols = vars(delay))
## check if dates are correct from macro? 


# joining xl TO raw
mitchell_rawvsxl <- left_join(mitchell_raw_macro, mitchell_macro_xl, by = "filename") %>% 
  left_join(discounting_filenames %>% as.data.frame() %>% rename("filename" = ".") %>% mutate(filename = as.character(filename), 
                                                                                              cohort = readr::parse_number(str_match(filename, "Ship\\d")) %>% as.character(),
                                                                                              filename = gsub("./Ship\\d_Latin-square/", "", filename)), ., by = "filename")



left_join(mitchell_macro_xl, discounting_filenames %>% as.data.frame() %>% rename("filename" = ".") %>% mutate(filename = as.character(filename), 
                                                                                                               cohort = readr::parse_number(str_match(filename, "Ship\\d")) %>% as.character(),
                                                                                                               filename = gsub("./Ship\\d_Latin-square/", "", filename)), by = "filename") %>% select(cohort) %>% table()


names(mitchell_rawvsxl) <- gsub("[.]x", "_raw", names(mitchell_rawvsxl))
names(mitchell_rawvsxl) <- gsub("[.]y", "_xl", names(mitchell_rawvsxl))
mitchell_rawvsxl %>% subset(delay_raw!=delay_xl) # none
mitchell_rawvsxl %>% 
  mutate(median_raw = round(median_raw,0),
         median_xl = as.numeric(median_xl) %>% round(0)) %>% subset(median_raw < median_xl - 1 | median_raw > median_xl + 1) ## only one off
# tried truncating both; but got 679 not matching
mitchell_rawvsxl %>% 
  mutate(median_raw = round(median_raw, 0),
         median_xl = round(as.numeric(median_xl), 0)) %>% 
ggplot(aes(x = median_raw, y = median_xl)) + 
  geom_point() + 
  facet_grid(~ cohort) + 
  labs(title = paste0("Excel vs Raw Comparison by Cohort"),
       y = "Excel Median", x = "Raw Median") + 
  theme(axis.text.x = element_text(hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 1, size = 12)) 



# joining raw TO xl
mitchell_xlvsraw <- left_join(mitchell_macro_xl, mitchell_raw_macro, by = "filename")  %>% 
  left_join(discounting_filenames %>% as.data.frame() %>% rename("filename" = ".") %>% mutate(filename = as.character(filename), 
                                                                                              cohort = readr::parse_number(str_match(filename, "Ship\\d")) %>% as.character(),
                                                                                              filename = gsub("./Ship\\d_Latin-square/", "", filename)), ., by = "filename") # last join is for cohort information

names(mitchell_xlvsraw) <- gsub("[.]x", "_xl", names(mitchell_xlvsraw))
names(mitchell_xlvsraw) <- gsub("[.]y", "_raw", names(mitchell_xlvsraw))
mitchell_xlvsraw %>% 
  mutate(median_raw = round(median_raw, 0),
         median_xl = trunc(as.numeric(median_xl))) %>% subset(is.na(median_raw)&!is.na(median_xl)) %>% dim ## but diff DADs
mitchell_xlvsraw %>% 
  mutate(median_raw = round(median_raw, 0),
         median_xl = trunc(as.numeric(median_xl))) %>% 
  subset(median_raw != median_xl)
mitchell_xlvsraw %>% 
  mutate(median_raw = round(median_raw, 0),
         median_xl = trunc(as.numeric(median_xl))) %>% 
  ggplot(aes(x = median_raw, y = median_xl)) + 
  geom_point() +
  facet_grid(~ cohort) + 
  labs(title = paste0("Raw vs Excel Comparison by Cohort"),
       y = "Raw Median", x = "Excel Median") + 
  theme(axis.text.x = element_text(hjust = 1, size = 12),
        axis.text.y = element_text(hjust = 1, size = 12)) 


## why are there missing data in the raw
mitchell_xlvsraw %>% subset(!is.na(median_xl) & is.na(median_raw))

