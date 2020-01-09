## CREATE PLOTS

## merge on metadata
discountingvalidtraits %>% 
  mutate(subject = paste0("9330003200", subject)) %>% 
  left_join(WFU_Mitchell_test_df[,c("cohort", "sex", "rfid", "dob")], ., by = c("rfid"= "subject")) %>% 
  mutate(experiment_age = round(as.numeric(date - dob)), 0) %>% 
  select(cohort, rfid, everything()) %>% 
  select(-c(filename, dob), filename) %>% head
  

# WFU_Mitchell_test_df %>% 
#   select(cohort, sex, rfid, dob) %>% 
#   mutate(rfid = str_sub(rfid,-5,-1)) %>% 

Jhou_Locomotor_Excel_graph <- Jhou_Locomotor %>%  
  merge(Jhou_SummaryAll[,c("labanimalid", "rfid", "shipmentcohort", "dob")], ., by = "labanimalid") 
# %>%  # extract file information for preparation for appending to rfid
# mutate(labanimalid = gsub('(U)([[:digit:]]{1})$', '\\10\\2', labanimalid))

locomotormeasures <- grep(pattern = "^(?=min|bin)", names(Jhou_Locomotor_Raw_graph), perl = T, value = T)
onlymins <-  grep(pattern = "^(min)", names(Jhou_Locomotor_Raw_graph), perl = T, value = T)[-31]
onlymins_excel <- paste0(onlymins, "_excel")
onlymins_raw <- paste0(onlymins, "_raw")

joinrawtoexcel <- left_join(Jhou_Locomotor_Excel_graph,subset(Jhou_Locomotor_Raw_graph, select = c("labanimalid", "session", onlymins)),  by = c("labanimalid", "session"))
names(joinrawtoexcel) <- gsub(".x", "_excel", names(joinrawtoexcel))
names(joinrawtoexcel) <- gsub(".y", "_raw", names(joinrawtoexcel))
xlim <- lapply(joinrawtoexcel[onlymins_excel], range, na.rm=T)
ylim <- lapply(joinrawtoexcel[onlymins_raw], range, na.rm=T)


pdf("jhou_locomotor_compare.pdf", onefile = T)
plot_list = list()
plot_compare_list = list()



for (i in seq_along(onlymins)){
  # 
  # plot_list[[i]] <- ggplot(rawfiles_locomotor_wide_graph, aes(x=shipmentcohort))+ 
  #   geom_boxplot(aes_string(y = locomotormeasures[i])) + 
  #   labs(title = paste0(locomotor_dd$var_graphtext[i], "_locomotor_U01_Jhou"),
  #        y = locomotor_dd$var_graphtext[i], x = "Cohort") +
  #   theme(axis.text.x = element_text(angle = 45))
  
  plot_compare_list[[i]] <- ggplot(joinrawtoexcel, aes_string( onlymins_excel[i], onlymins_raw[i])) + 
    geom_point(aes(color = shipmentcohort)) +
    # geom_text(aes_string(label=ifelse(onlymins_excel[i] == onlymins_raw[i], '', "labanimalid")),hjust=0,vjust=0) + 
    # geom_text(aes(label = labanimalid), data = joinrawtoexcel[joinrawtoexcel$labanimalid %in% excelhasbutnotraw$labanimalid,]) + # get excelhasbutnotraw from raw and excel qc
    labs(title = paste0("Comparison of ", onlymins[i], "_locomotor_U01_Jhou"),
         y = onlymins_raw[i], x = onlymins_excel[i]) + 
    scale_x_continuous(limits = xlim[[i]]) + 
    scale_y_continuous(limits = xlim[[i]]) + 
    geom_abline(slope = 1, intercept = 0, size = 0.5, alpha = 0.5)
  
  
  # geom_text(aes(CPI, HDI, label = Country), data = dat[dat$Country %in% pointsToLabel,])
  
  
  #plot_compare_list[[i]] <- ggplot(data = test, aes(x=minute1.x, y = minute1.y)) + geom_point()
  
  #  print(plot_list[[i]])
  print(plot_compare_list[[i]])
  
  
}
dev.off()

