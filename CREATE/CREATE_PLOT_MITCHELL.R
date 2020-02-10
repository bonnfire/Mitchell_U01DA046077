## CREATE PLOTS

## merge on metadata
# add rep and delay information
# examples of 

rxn_time %>% arrange(subject, date) %>% bind_cols(., ship1_2_bind) %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(., WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], by = c("subject" = "rfid")) %>% 
  arrange(delay) %>% 
  mutate(delay = reorder(delay, sort(as.numeric(delay)))) %>% 
  ggplot(aes(x = delay, y = avg_rxn_time_free)) + ### XXX CHANGE THE FOR LOOP HERE ## NOTE THE OUTLIERS (POSITIVE)
  geom_boxplot(aes(color = cohort)) + 
  facet_grid( ~ sex)


rxn_time %>% arrange(subject, date) %>% bind_cols(., ship1_2_bind) %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(., WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], by = c("subject" = "rfid")) %>% 
  arrange(delay) %>% 
  mutate(delay = reorder(delay, sort(as.numeric(delay)))) %>% 
  ggplot(aes(x = delay, y = avg_choice_rxn_time_free)) + ### XXX NOTE THE NEGATIVE OUTLIERS HERE
  geom_boxplot(aes(color = cohort)) + 
  facet_grid( ~ sex)



discountingvalidtraits_graph <- discountingvalidtraits %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], ., by = c("rfid"= "subject")) %>% 
  mutate(experiment_age = round(as.numeric(date - dob)), 0) %>% 
  select(cohort, rfid, everything()) %>% 
  select(-c(filename, dob), filename) 
  
# WFU_Mitchell_test_df %>% 
#   select(cohort, sex, rfid, dob) %>% 
#   mutate(rfid = str_sub(rfid,-5,-1)) %>% 


discountingtraits_extract <- data.frame(var_abv = grep(pattern = "_", names(discountingvalidtraits_graph), perl = T, value = T) %>% as.character(),
                                    var_graph = c("total number of trials", "number of free choice trials", "number of forced delay trials", "number of forced immediate trials",
                                                  "number of forced trials", "number of free choice events before center nose pose", "number of forced delay events before center",
                                                  "average reaction times from start to center on the delayed side", "average reaction times from center to choice on the delayed side",
                                                  "average choice reaction times from start to center on the immediate side", "average choice reaction times from center to choice on the immediate side", 
                                                  "experiment age")) %>% 
  mutate(var_abv = unlist(var_abv) %>% as.character,
         var_graph = unlist(var_graph) %>% as.character)

discountingtraits <- grep(pattern = "_", names(discountingvalidtraits_graph), perl = T, value = T)

pdf("mitchell_discounting.pdf", onefile = T)
plot_list = list()
plot_compare_list = list()

for (i in seq_along(discountingtraits_extract$var_abv)){

  # plot_by_cohort <- ggplot(discountingvalidtraits_graph, aes(x = cohort, group = cohort)) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = paste0(toupper(discountingtraits_extract$var_graph[i]), "_Discounting_U01_Mitchell", "\n"), x = "Cohort", fill = "Cohort") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # plot_by_box <- ggplot(discountingvalidtraits_graph, aes(x = as.factor(box), group = as.factor(box))) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = paste0(toupper(discountingtraits_extract$var_graph[i]), "_Discounting_U01_Mitchell", "\n"), x = "Box", fill = "Box") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plot_by_sex <- ggplot(discountingvalidtraits_graph, aes(x = as.factor(sex), group = as.factor(sex))) +
    geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
    geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
    labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
                                 "_Discounting_U01_Mitchell", "\n"), width = 100), 
                          x = "Sex", 
                          fill = "Sex") +  # scale_fill_discrete(name = "New Legend Title")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  # print(plot_by_cohort)
  # print(plot_by_box)
  print(plot_by_sex)
  
  
}
dev.off()


## quick plots
timeout_duration %>% 
  ggplot(aes(avg_timeout_dur_free))
ggplot()
