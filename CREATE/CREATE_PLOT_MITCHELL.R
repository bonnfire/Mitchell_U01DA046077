## CREATE PLOTS

## merge on metadata
# add rep and delay information


## positive outliers here: avg_rxn_time_free, negative outliers here: avg_choice_rxn_time_free

## gget wfu sex and birthdate; join with metadata 

discountingvalidtraits_graph <- discountingvalidtraits %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], ., by = c("rfid"= "subject")) %>% 
  left_join(metadata, by = c("cohort", "rfid")) %>% 
  mutate(experimentage = round(as.numeric(date - lubridate::ymd(as.character(dob))), 0)) %>% 
  mutate_at(vars(c("boxcolor", "computer", "assignedlever")), toupper) %>% 
  select(cohort, rfid, everything()) %>% 
  select(-c(filename, dob), filename)
discountingvalidtraits_graph$delay <- factor(discountingvalidtraits_graph$delay, levels = sort(discountingvalidtraits_graph$delay %>% unique))
# WFU_Mitchell_test_df %>% 
#   select(cohort, sex, rfid, dob) %>% 
#   mutate(rfid = str_sub(rfid,-5,-1)) %>%


## create names for otology db (XX PICK UP HERE TO ADD MACROS FROM DISCOUNTING AND LOCOMOTOR)
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Mitchell_U01DA046077/CREATE")
set("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting")

openxlsx::write.xlsx(unique(c(paste0("discounting_", names(discountingvalidtraits_graph)), paste0("discounting_", names(mitchell_macro_xl)))), "discounting_data_dictionary.xlsx")


## how much raw data do we have for spleen/ceca data 
mitchell_spleenceca_toprocess %>% left_join(.,
                                            discountingvalidtraits_graph %>% 
                                              distinct(rfid, assignedlever),
                                            by = c("rfid")) %>%
  subset(is.na(assignedlever))


discountingtraits_extract <- data.frame(var_abv = grep(pattern = "_", names(discountingvalidtraits_graph), perl = T, value = T) %>% as.character(),
                                    var_graph = c("total number of trials", "number of free choice trials", "number of forced delay trials", "number of forced immediate trials",
                                                  "number of forced trials", "number of free choice events before center nose poke", "number of forced delay events before center",
                                                  "number of forced immediate events before center", "total events prior to center nose poke", "events prior to free choice", 
                                                  "events prior to forced delay choice", "events prior to forced immediate choice", "total events prior to choice",
                                                  "events during free choice timeout", "events during forced delay timeout", "events during forced immediate timeout", "total events during timeout",
                                                  "average reaction time for free choice", "average choice reaction time for free choice", 
                                                  "average timeout duration for free choice", "average collection time for free choice",
                                                  "average number of events before immediate reward collection (free choice)", "average number of events before delayed reward collection (free choice)", 
                                                  "total number of events before collection (free choice)", "percent rewards collected (free choice)")) %>% 
                                                  # "average reaction times from start to center on the delayed side", "average reaction times from center to choice on the delayed side",
                                                  # "average choice reaction times from start to center on the immediate side", "average choice reaction times from center to choice on the immediate side", 
                                                  # "experiment age")) %>% 
  mutate(var_abv = unlist(var_abv) %>% as.character,
         var_graph = unlist(var_graph) %>% as.character)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Mitchell_U01DA046077/QC")


discountingvalidtraits_graph %>% split(.$cohort) %>% map(summary)



pdf("mitchell_discounting_raw.pdf", onefile = T)
for (i in seq_along(discountingtraits_extract$var_abv)){

  # plot_by_cohort <- ggplot(discountingvalidtraits_graph, aes(x = cohort, group = cohort)) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = paste0(toupper(discountingtraits_extract$var_graph[i]), "_Discounting_U01_Mitchell", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # plot_by_computer <- ggplot(discountingvalidtraits_graph, aes(x = as.factor(computer), group = as.factor(computer))) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = paste0(toupper(discountingtraits_extract$var_graph[i]), "_Discounting_U01_Mitchell", "\n"), x = "Box", fill = "Box") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plot_by_sex <- discountingvalidtraits_graph %>%
    dplyr::filter(!is.na(delay)) %>% 
    # ggplot(aes(x = as.factor(sex), group = as.factor(sex))) +
    ggplot(aes(x = as.factor(delay), color = sex)) +
    geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
    # geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
    facet_grid(~ cohort) +
    labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]),
                                 "_Discounting_U01_Mitchell", "\n"), width = 60),
         x = "Delay",
         y = discountingtraits_extract$var_graph[i]) +  # scale_fill_discrete(name = "New Legend Title")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # plot_by_cohort_sex = discountingvalidtraits_graph %>% 
  #   ggplot(aes(x = delay, color = cohort)) + ### XXX CHANGE THE FOR LOOP HERE ## NOTE THE OUTLIERS (POSITIVE)
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.5) + 
  #   facet_grid( ~ sex) + 
  #   labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
  #                                         "_Discounting_U01_Mitchell", "\n"), width = 60),
  #        y = discountingtraits_extract$var_graph[i])
  
  

  # print(plot_by_cohort)
  # print(plot_by_computer)
  print(plot_by_sex)
  # print(plot_by_cohort_sex)
  
}
dev.off()


## extract outliers for PI


boxplot.stats(discountingvalidtraits_graph$events_during_to)


ggplot_build(discountingvalidtraits_graph %>%
               dplyr::filter(!is.na(delay)) %>% 
               ggplot(aes(x = as.factor(delay), color = sex)) +
               geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
               facet_grid(~ cohort) +
               labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]),
                                                     "_Discounting_U01_Mitchell", "\n"), width = 60),
                    x = "Delay",
                    y = discountingtraits_extract$var_graph[i]) +  # scale_fill_discrete(name = "New Legend Title")
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
             )$data


dt_filt <- discountingvalidtraits_graph[, 
              .SD[
                ((events_before_choice > quantile(events_before_choice, probs = 0.97)) & 
                   (events_before_choice_fi > quantile(events_before_choice_fi, probs = 0.97)) & 
                   (events_during_to >  quantile(events_during_to, probs = 0.97)) &
                   (events_during_to_fi >  quantile(events_during_to_fi, probs = 0.97)) & 
                   (avg_collection_time_free >  quantile(avg_collection_time_free, probs = 0.97)) & 
                   (avg_events_before_collect_imm >  quantile(avg_events_before_collect_imm, probs = 0.97)) &
                   (avg_events_before_collect_del >  quantile(avg_events_before_collect_del, probs = 0.97)) &
                   (percent_reward_collected >  quantile(percent_reward_collected, probs = 0.97))
                   )  
                ], by = c("cohort", "sex", "delay")
              ]



dt_filt <- discountingvalidtraits_graph %>%
  mutate(delay =as.character(delay)) %>% 
  group_by(cohort, sex, delay) %>%
  mutate_all(mu = remove_outliers()) %>% 
  anti_join(., discountingvalidtraits_graph)


library(openxlsx)

dt_filt <- discountingvalidtraits_graph %>% 
  subset(events_before_center > 250|
           events_before_center_fd > 75|
           events_before_center_fi > 90|
           events_before_choice > 750|
           events_before_choice_fd > 150|
           events_before_choice_fi > 300|
           events_during_to > 7500|
           events_during_to_fd > 300|
           events_during_to_fi > 600|
           avg_rxn_time_free > 30000|
           avg_choice_rxn_time_free < 0|
           avg_choice_rxn_time_free > 25000|
           avg_collection_time_free > 30000|
           avg_events_before_collect_imm > 7500|
           avg_events_before_collect_del > 1000|
           events_before_collect_tot > 2500|
           percent_reward_collected<25)
wb <- createWorkbook()
addWorksheet(wb, "outliers_discounting")
writeData(wb, 1, dt_filt)
posStyle <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=11, rows = 1:nrow(dt_filt), rule=" > 250",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=12, rows = 1:nrow(dt_filt), rule=" > 75",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=13, rows = 1:nrow(dt_filt), rule=" > 90",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=15, rows = 1:nrow(dt_filt), rule=" > 750",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=16, rows = 1:nrow(dt_filt), rule=" > 150",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=17, rows = 1:nrow(dt_filt), rule=" > 300",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=19, rows = 1:nrow(dt_filt), rule=" > 7500",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=20, rows = 1:nrow(dt_filt), rule=" > 300",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=21, rows = 1:nrow(dt_filt), rule=" > 600",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=23, rows = 1:nrow(dt_filt), rule=" > 30000",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=24, rows = 1:nrow(dt_filt), rule=" < 0",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=24, rows = 1:nrow(dt_filt), rule=" > 25000",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=26, rows = 1:nrow(dt_filt), rule=" > 30000",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=27, rows = 1:nrow(dt_filt), rule=" > 7500",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=28, rows = 1:nrow(dt_filt), rule=" > 1000",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=29, rows = 1:nrow(dt_filt), rule=" > 2500",style = posStyle)
conditionalFormatting(wb=wb, sheet='outliers_discounting', cols=30, rows = 1:nrow(dt_filt), rule=" < 25",style = posStyle)

openxlsx::saveWorkbook(wb, "outliers_discounting.xlsx", TRUE)












conditionalFormatting(wb=wb1, sheet='sheet1', cols=numeric_cols, rows=1:100, rule=" > 4", type = "expression",style = posStyle)
# this rule assumes normalcy in the data which is not true 
library(plyr)
df2 = ddply(discountingvalidtraits_graph, .(avg_events_before_collect_imm, events_during_to), function(d){
  limits = median(d$y) + 3*c(-1, 1)*mad(d$y)
  d[(d$y - limits[1])*(limits[2] - d$y) > 0,]
})




#### CHECKING THE WITHIN SUBJECT RANDOMNESS 
discountingvalidtraits_graph %>% 
  subset(!is.na(cohort)&!is.na(delay)) %>% 
  ggplot() + 
  geom_path(aes(x = rep, y = avg_rxn_time_free, color = subject)) + 
  facet_grid(rows = vars(cohort), cols = vars(delay)) + 
  theme(legend.title = element_blank(), legend.position = "none",
        text = element_text(size=20)) + 
  labs(title = "Average reaction time (free) trials for each subject")


## calculate summary stats for all trials for each delay
discountingvalidtraits_graph_summary <- discountingvalidtraits_graph %>% 
  subset(!is.na(cohort)&!is.na(delay)) %>% 
  group_by(cohort, subject, delay) %>% 
  summarise_at(.vars = discountingtraits_extract$var_abv,
               .funs = c(mean="mean", min = "min", max = "max", sd = "sd", var = "var"), na.rm = T) 
discountingvalidtraits_graph_summary %>% select(cohort, subject, delay, matches("var"))
# %>% 
#   ggplot(aes(x = rep, y = avg_rxn_time_free_mean)) + 
#   geom_path(stat = "identity") + 
#   geom_errorbar(aes(x = rep, ymin= avg_rxn_time_free_mean -avg_rxn_time_free_sd, ymax=avg_rxn_time_free_mean+avg_rxn_time_free_sd), na.rm = T, stat = "identity", width=0.4, colour="orange", alpha=0.9, size=1.3) +
#   facet_grid(rows = vars(cohort), cols = vars(delay)) +
#   theme(legend.title = element_blank(), legend.position = "none",
#         text = element_text(size=20))




my.rmc <- rmcorr::rmcorr(participant = subject, measure1 = delay, measure2 = events_during_to, dataset = discountingvalidtraits_graph)
library(RColorBrewer)
blueset <- brewer.pal(8, 'Blues')
pal <- colorRampPalette(blueset)
plot(my.rmc, overall = TRUE, palette = pal, overall.col = 'black')




pdf("mitchell_discounting_within.pdf", onefile = T)
for (i in seq_along(discountingtraits_extract$var_abv)){
  
  plot_by_sex <- discountingvalidtraits_graph %>% 
    subset(!is.na(cohort)&!is.na(delay)) %>% 
    ggplot(aes(x = rep, color = subject)) + 
    geom_path(aes_string(y = discountingtraits_extract$var_abv[i])) + 
    facet_grid(rows = vars(cohort), cols = vars(delay)) + 
    theme(legend.title = element_blank(), legend.position = "none",
          text = element_text(size=20)) + 
    labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]),
                                          "_Discounting_U01_Mitchell", "\n", 
                                          "Assess Within Subject Variability"), width = 30),
         x = "Rep",
         y = discountingtraits_extract$var_graph[i])
  print(plot_by_sex)
  
}
dev.off()


## QCING THE AGE

discountingvalidtraits_graph %>%
  group_by(cohort, subject) %>%
  mutate(exp_age_diff = experimentage - lag(experimentage, default = first(experimentage))) %>% 
  select(cohort, subject, delay, exp_age_diff) %>% head(10)


discountingvalidtraits_graph$exp_age_diff <- ave(discountingvalidtraits_graph$experimentage, discountingvalidtraits_graph$subject, FUN=function(x) c(0, diff(x)))



discountingvalidtraits_graph %>% head(2)



## QCING THE DISTRIBUTION OF THE POINTS




## find out why avg_collection_time_free is NA
# avg_collection_time_free
# ./Ship1_Latin-square/2019-03-12_10h42m_Subject 46052.txt

## maybe why other ones are this way?
mitchell_raw_macro_expanded <- mitchell_raw_macro %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], ., by = c("rfid"= "subject")) %>% 
  left_join(metadata, by = c("cohort", "rfid"))

mitchell_raw_macro_expanded %>% 
  subset(!is.na(operantbox) & !is.na(delay)) %>%
  mutate(delay = factor(delay, levels = unique(sort(delay)))) %>% 
  ggplot(aes(x = delay, y = median, group = delay)) + 
  # geom_boxplot(aes(x = delay, y = median, group = delay)) + 
  geom_point(stat = 'summary', fun.y = function(x) median(x, na.rm = TRUE)) +
  geom_path(aes(group = 1), stat = 'summary', fun.y = function(x) median(x, na.rm = TRUE)) +
  facet_grid(~ operantbox) + 
  labs(title = paste0("Delay Medians by Operant Box"),
                                 y = "Median of Medians", x = "Delay") + 
  theme(axis.text.x = element_text(hjust = 1, size = 12, angle = 40),
        axis.text.y = element_text(hjust = 1, size = 12)) 





mitchell_raw_macro_expanded %>% 
  subset(!is.na(boxcolor) & !is.na(delay)) %>%
  mutate(delay = factor(delay, levels = unique(sort(delay))),
         boxcolor = toupper(boxcolor)) %>% 
  ggplot(aes(x = delay, y = median, group = delay)) + 
  # geom_boxplot(aes(x = delay, y = median, group = delay)) + 
  geom_point(stat = 'summary', fun.y = function(x) median(x, na.rm = TRUE)) +
  geom_path(aes(group = 1), stat = 'summary', fun.y = function(x) median(x, na.rm = TRUE)) +
  facet_grid(~ computer) + 
  labs(title = paste0("Delay Medians by Computer/Box Color"),
       y = "Median of Medians", x = "Delay") + 
  theme(axis.text.x = element_text(hjust = 1, size = 12, angle = 40),
        axis.text.y = element_text(hjust = 1, size = 12)) 
























# locomotor_avg[, rfid := paste0("9330003200", rfid)] # already done to locomotor_raw
locomotorvalidtraits_graph <- left_join(locomotor_avg, WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], by = "rfid")
# locomotorvalidtraits_graph[, time := str_match(experiment, "U01-(.*?)-.*")[,2]]   
# locomotortraits_extract <- data.frame(var_abv = grep(pattern = ".", names(locomotorvalidtraits_graph), perl = T, value = T) %>% as.character(),
#                                         var_graph = c("total number of trials", "number of free choice trials", "number of forced delay trials", "number of forced immediate trials",
#                                                       "number of forced trials", "number of free choice events before center nose pose", "number of forced delay events before center",
#                                                       "number of forced immediate events before center", "total events prior to center nose poke", "events prior to free choice", 
#                                                       "events prior to forced delay choice", "events prior to forced immediate choice", "total events prior to choice",
#                                                       "events during free choice timeout", "events during forced delay timeout", "events during forced immediate timeout", "total events during timeout",
#                                                       "average reaction time for free choice", "average choice reaction time for free choice", 
#                                                       "average timeout duration for free choice", "average collection time for free choice")) %>% 
#   # "average reaction times from start to center on the delayed side", "average reaction times from center to choice on the delayed side",
#   # "average choice reaction times from start to center on the immediate side", "average choice reaction times from center to choice on the immediate side", 
#   # "experiment age")) %>% 
#   mutate(var_abv = unlist(var_abv) %>% as.character,
#          var_graph = unlist(var_graph) %>% as.character)
locomotortraits_extract <- grep(pattern = "[.]", names(locomotorvalidtraits_graph), perl = T, value = T) %>% as.character()
                                      
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Mitchell_U01DA046077/QC")
pdf("mitchell_locomotor_raw.pdf", onefile = T)

## investigation of special cases
## cohort 4 (# from overview -- 47724, 47723, 47974 and 47530 were euthanized using CO2 chamber during study due to severe seizure phenotypes (onset post locomotor and during delay discounting sessions). Spleens, ceca, and tails collected.  Seizures were not witnessed by staff until weeks after the locomotor task had been completed; however, data may still not be viable due to complications associated with seizure phenotype.  )
seizure_rats_c04 <- locomotorvalidtraits_graph %>% subset(grepl("47724|47723|47974|47530", rfid))

for (i in seq_along(locomotortraits_extract)){
  
  # plot_by_cohort <- ggplot(discountingvalidtraits_graph, aes(x = cohort, group = cohort)) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = paste0(toupper(discountingtraits_extract$var_graph[i]), "_Discounting_U01_Mitchell", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_by_box <- locomotorvalidtraits_graph %>% 
    subset(!is.na(time)) %>% 
    ggplot(aes(x = time, color = cage)) +
    geom_boxplot(aes_string(y = locomotortraits_extract[i]), outlier.size = 0.5) + 
    facet_grid( ~ cohort) + 
    labs(title = stringr::str_wrap(paste0(toupper(locomotortraits_extract[i]), 
                                          "_Locomotor_U01_Mitchell_By_Cohort and Cage", "\n"), width = 60),
         y = locomotortraits_extract[i]) 
  # +
  #   geom_point(data = seizure_rats_c04, aes_string(x = "time", y =  locomotortraits_extract[i], color = "sex"), size = 2) +
  #   geom_text(data = seizure_rats_c04 %>% mutate(rfid = gsub("9330003200", "", rfid)), aes_string(x = "time", y = locomotortraits_extract[i], label = "rfid"), vjust=1)
  
  
  # plot_by_sex <- discountingvalidtraits_graph %>% 
  #   ggplot(aes(x = as.factor(sex), group = as.factor(sex))) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
  #                                "_Discounting_U01_Mitchell", "\n"), width = 100), 
  #                         x = "Sex", 
  #                         fill = "Sex") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_by_cohort_sex = locomotorvalidtraits_graph %>% 
    subset(!is.na(time)) %>% 
    ggplot(aes(x = time, color = sex)) +
    geom_boxplot(aes_string(y = locomotortraits_extract[i]), outlier.size = 0.5) + 
    facet_grid( ~ cohort) + 
    labs(title = stringr::str_wrap(paste0(toupper(locomotortraits_extract[i]), 
                                          "_Locomotor_U01_Mitchell_By_Cohort and Sex", "\n"), width = 60),
         y = locomotortraits_extract[i]) +
    geom_point(data = seizure_rats_c04, aes_string(x = "time", y =  locomotortraits_extract[i], color = "sex"), size = 2) +
    geom_text(data = seizure_rats_c04 %>% mutate(rfid = gsub("9330003200", "", rfid)), aes_string(x = "time", y = locomotortraits_extract[i], label = "rfid"), vjust=1)
  
  
  
  
  # print(plot_by_cohort)
  print(plot_by_box)
  # print(plot_by_sex)
  print(plot_by_cohort_sex)
  
}
dev.off()


locomotorvalidtraits_graph %>% 
  subset(!is.na(time)) %>% 
  ggplot(aes(x = time, color = sex)) +
  geom_boxplot(aes_string(y = locomotortraits_extract[i]), outlier.size = 0.5) + 
  facet_grid( ~ cohort) + 
  geom_point(data=seizure_rats_c04, aes(x = time, y = center_time_legacy_s.sum, color = sex), size = 2) +
  geom_text(data = seizure_rats_c04 %>% mutate(rfid = gsub("9330003200", "", rfid)), aes(x = time, y = center_time_legacy_s.sum, label = rfid), vjust=1)
