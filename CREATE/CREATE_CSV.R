## CREATE CSV FILES FOR DATABASE
setwd("~/Desktop/Database/csv files/u01_suzanne_mitchell")
write.csv(mitchell_wfu_metadata_c01_05, "mitchell_wfu_metadata_c01_05.csv", row.names = F)
write.csv(locomotor_gwas_metadata_c01_05, "locomotor_gwas_c01_05.csv", row.names = F)
write.csv(discounting_gwas_metadata_c01_03, "discounting_gwas_c01_03.csv", row.names = F)

View(discounting_gwas_metadata_c01_03 %>% 
  names %>% as.data.frame() %>% rename("exp" = ".") %>% 
  mutate(delay = str_extract(exp, "\\d+$") %>% as.numeric, exp_name = gsub("_\\d+", "", exp)) %>% 
  transform(id=match(exp_name, unique(exp_name))) %>% arrange(id, delay) %>% 
  mutate(description = "NA", 
         description = replace(description, exp_name == "tot_num_trials_mean", paste0("average of the total number of trials in all sessions for delay ", delay)),
         description = replace(description, exp_name == "tot_num_free_mean", paste0("average of the total number of free choice trials in all sessions for delay ", delay)),
         description = replace(description, exp_name == "tot_num_fd_mean", paste0("average of the total number of forced delay trials in all sessions for delay ", delay)),
         description = replace(description, exp_name == "tot_num_fi_mean", paste0("average of the total number of forced immediate trials in all sessions for delay ", delay)),
         description = replace(description, exp_name == "tot_forced_trials_mean", paste0("average of the total number of forced trials in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "events_before_center_mean", paste0("average of the number of events before center poke in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_center_fd_mean", paste0("average of the number of events before center poke in forced delay sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_center_fi_mean", paste0("average of the number of events before center poke in forced immediate sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_center_tot_mean", paste0("average of the number of events before center poke in all sessions for delay ", delay)),
     
         description = replace(description, exp_name == "events_before_choice_mean", paste0("average of the number of events before choice in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_choice_fd_mean", paste0("average of the number of events before choice in forced delay sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_choice_fi_mean", paste0("average of the number of events before choice in forced immediate sessions for delay ", delay)),
         description = replace(description, exp_name == "events_before_choice_tot_mean", paste0("average of the number of events before choice in all sessions for delay ", delay)),

         description = replace(description, exp_name == "events_during_to_mean", paste0("average of the number of events during time out in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "events_during_to_fd_mean", paste0("average of the number of events during time out in forced delay sessions for delay ", delay)),
         description = replace(description, exp_name == "events_during_to_fi_mean", paste0("average of the number of events during time out in forced immediate sessions for delay ", delay)),
         description = replace(description, exp_name == "events_during_to_tot_mean", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "avg_rxn_time_free_mean", paste0("average of the average reaction times in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "avg_choice_rxn_time_free_mean", paste0("average of the average reaction times in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "avg_timeout_dur_free_mean", paste0("average of the average time out duration in free choice sessions for delay ", delay)),
         description = replace(description, exp_name == "avg_collection_time_free_mean", paste0("average of the average collection time in free choice sessions for delay ", delay)),
         
         description = replace(description, exp_name == "avg_events_before_collect_imm_mean", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "avg_events_before_collect_del_mean", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "avg_events_before_collect_tot_mean", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "events_before_collect_tot_mean", paste0("average of the total number of events before reward collecting in  sessions for delay ", delay)),
         description = replace(description, exp_name == "percent_reward_collected_mean", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "mean_of_medians_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "n_analyzed_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "percent_choice_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "rxntime_delay_avg_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "choicerxntime_delay_avg_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "rxntime_imm_avg_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "choicerxntime_imm_avg_extremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "mean_of_medians_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "n_analyzed_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "percent_choice_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "rxntime_delay_avg_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "choicerxntime_delay_avg_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "rxntime_imm_avg_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         description = replace(description, exp_name == "choicerxntime_imm_avg_noextremetrials", paste0("average of the number of events during time out in all sessions for delay ", delay)),
         
         description = replace(description, exp_name == "age", paste0("age of animal at first session of delay ", delay)),
         
         
         
         mutate(events_before_collect_tot = avg_events_before_collect_imm + avg_events_before_collect_del) %<>% 
           
         
         )
)
