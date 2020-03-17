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


mitchell01_rawvsxl <- left_join(ship1_raw_macro, mitchell_c01_xl, by = "filename") 
names(mitchell01_rawvsxl) <- gsub("[.]x", "_raw", names(mitchell01_rawvsxl))
names(mitchell01_rawvsxl) <- gsub("[.]y", "_xl", names(mitchell01_rawvsxl))
mitchell01_rawvsxl %>% subset(delay_raw!=delay_xl) # none
mitchell01_rawvsxl %>% 
  mutate(median_raw = round(median_raw,0),
         median_xl = as.numeric(median_xl) %>% round(0)) %>% subset(median_raw < median_xl - 1 | median_raw > median_xl + 1) ## only one off
# (median_raw != median_xl) | 