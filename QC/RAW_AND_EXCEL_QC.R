# RAW QC AND EDITS 

library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)

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


