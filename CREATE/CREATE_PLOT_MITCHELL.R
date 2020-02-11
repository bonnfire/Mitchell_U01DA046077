## CREATE PLOTS

## merge on metadata
# add rep and delay information


## positive outliers here: avg_rxn_time_free, negative outliers here: avg_choice_rxn_time_free

discountingvalidtraits_graph <- discountingvalidtraits %>% 
  arrange(subject, date) %>% bind_cols(., ship1_2_bind) %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], ., by = c("rfid"= "subject")) %>% 
  left_join(metadata, by = c("cohort", "rfid")) %>% 
  mutate(experimentage = round(as.numeric(date - dob)), 0) %>% 
  select(cohort, rfid, everything()) %>% 
  select(-c(filename, dob, rep, `0`), filename)
discountingvalidtraits_graph$delay <- factor(discountingvalidtraits_graph$delay, levels = sort(discountingvalidtraits_graph$delay %>% unique))
# WFU_Mitchell_test_df %>% 
#   select(cohort, sex, rfid, dob) %>% 
#   mutate(rfid = str_sub(rfid,-5,-1)) %>% 


## how much raw data do we have for spleen/ceca data 
mitchell_spleenceca_toprocess %>% left_join(.,
                                            discountingvalidtraits_graph %>% 
                                              distinct(rfid, assignedlever),
                                            by = c("rfid")) %>%
  subset(is.na(assignedlever))


discountingtraits_extract <- data.frame(var_abv = grep(pattern = "_", names(discountingvalidtraits_graph), perl = T, value = T) %>% as.character(),
                                    var_graph = c("total number of trials", "number of free choice trials", "number of forced delay trials", "number of forced immediate trials",
                                                  "number of forced trials", "number of free choice events before center nose pose", "number of forced delay events before center",
                                                  "number of forced immediate events before center", "total events prior to center nose poke", "events prior to free choice", 
                                                  "events prior to forced delay choice", "events prior to forced immediate choice", "total events prior to choice",
                                                  "events during free choice timeout", "events during forced delay timeout", "events during forced immediate timeout", "total events during timeout",
                                                  "average reaction time for free choice", "average choice reaction time for free choice", 
                                                  "average timeout duration for free choice", "average collection time for free choice")) %>% 
                                                  # "average reaction times from start to center on the delayed side", "average reaction times from center to choice on the delayed side",
                                                  # "average choice reaction times from start to center on the immediate side", "average choice reaction times from center to choice on the immediate side", 
                                                  # "experiment age")) %>% 
  mutate(var_abv = unlist(var_abv) %>% as.character,
         var_graph = unlist(var_graph) %>% as.character)


pdf("mitchell_discounting_raw.pdf", onefile = T)
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
  
  # plot_by_sex <- discountingvalidtraits_graph %>% 
  #   ggplot(aes(x = as.factor(sex), group = as.factor(sex))) +
  #   geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.75) +
  #   geom_jitter(aes_string(y = discountingtraits_extract$var_abv[i]), alpha = 0.3, position=position_jitter(0.2), size = 0.5) +
  #   labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
  #                                "_Discounting_U01_Mitchell", "\n"), width = 100), 
  #                         x = "Sex", 
  #                         fill = "Sex") +  # scale_fill_discrete(name = "New Legend Title")
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_by_cohort_sex = discountingvalidtraits_graph %>% 
    ggplot(aes(x = delay, color = cohort)) + ### XXX CHANGE THE FOR LOOP HERE ## NOTE THE OUTLIERS (POSITIVE)
    geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.5) + 
    facet_grid( ~ sex) + 
    labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
                                          "_Discounting_U01_Mitchell", "\n"), width = 60),
         y = discountingtraits_extract$var_graph[i])
  
  

  # print(plot_by_cohort)
  # print(plot_by_box)
  # print(plot_by_sex)
  print(plot_by_cohort_sex)
  
}
dev.off()














locomotor_avg[, subject_id := paste0("9330003200", subject_id)]
locomotorvalidtraits_graph <- locomotor_avg[setDT(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")]), on = c("subject_id == rfid")]
locomotorvalidtraits_graph[, time := str_match(experiment, "U01-(.*?)-.*")[,2]]   
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
                                      
pdf("mitchell_locomotor_raw.pdf", onefile = T)
for (i in seq_along(locomotortraits_extract)){
  
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
    ggplot(aes(x = delay, color = cohort)) + ### XXX CHANGE THE FOR LOOP HERE ## NOTE THE OUTLIERS (POSITIVE)
    geom_boxplot(aes_string(y = discountingtraits_extract$var_abv[i]), outlier.size = 0.5) + 
    facet_grid( ~ sex) + 
    labs(title = stringr::str_wrap(paste0(toupper(discountingtraits_extract$var_graph[i]), 
                                          "_Discounting_U01_Mitchell", "\n"), width = 60),
         y = discountingtraits_extract$var_graph[i])
  
  
  
  # print(plot_by_cohort)
  # print(plot_by_box)
  # print(plot_by_sex)
  print(plot_by_cohort_sex)
  
}
dev.off()

