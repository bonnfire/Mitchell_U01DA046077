setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting") # use duplicate folder that contains the unzipped files
discounting_filenames_c03 <- list.files(path = ".", pattern = "*.txt", recursive = T, full.names = T) %>% grep("Ship3", ., value = T) # 3587 files 
# discounting_filenames_c03 %>% as.data.frame() %>% mutate(shipment = str_extract(`.`, "/Ship.*/")) %>% select(shipment) %>% table ## see which cohorts and how many each

extract_delays <- function(x){
  delays <- fread(paste0("grep -a2 \"30000\" ", "'", x, "'", " | tail -n 1"))
  delays$filename <- x
  return(delays)
}

delays_c03 <- lapply(discounting_filenames_c03, extract_delays) %>% rbindlist() %>% 
  rename("delay" = "V1") %>% 
  mutate(delay = delay/1000)
delays_c03 <- delays_c03 %>% 
  mutate(delay = replace(delay, delay == 0.001, 0))


######################################################
############ USE FOR EXTRACTING DISCOUNT
######################################################


# extract event and codes data
readdiscounting <- function(x){
  
  
  discounting <- fread(paste0("grep -Pzo \'(?s)-100.*\\r\\n.*30000\\r\\n1\' ", "'", x, "'"))
  end <- which(discounting$V1 == 1) %>% tail(1)
  discounting <- discounting[-c((end-1):nrow(discounting)),] # 1/10 "The session should end at the 360000-ish mark so no problem having things in the 300000s" - Suzanne Mitchell
  indices <- which(discounting$V1 < 0)
  discounting_split <- split(discounting, cumsum(1:nrow(discounting) %in% indices))
  
  discounting_split_newcols <- lapply(discounting_split, function(x){
    names(x) <- "codes"
    
    if(nrow(x) == 2){
      x$timefromstart <- tail(x$codes, 1)
      x <- x[-nrow(x),]
    }
    
    
    if(nrow(x) > 2){
      x$timefromstart <- x$codes[4]
      x$reward <- x[2, 1]
      x$adjustingamt <- x[3, 1]
      x <- x[1,]
    }
    
    
    return(x)
  }) %>% rbindlist(fill = T)
  discounting_split_newcols$file <- x
  
  return(discounting_split_newcols)
  
}

discounting_df_c03 <- lapply(discounting_filenames_c03, readdiscounting) %>% rbindlist(fill = T) # 7162 files
# summary looks good, no positive numbers in codes and almost all positive timestamps (only one na in file == "./Ship1_Latin-square/2019-03-22_15h05m_Subject 45883.txt")
# discounting_df %>% add_count(file) %>% distinct(file, n) %>% mutate(n = as.numeric(n)) %>% select(n) %>% summary() MIN SHOULD NOT BE LESS THAN 10


discounting_df_expanded_c03 <- discounting_df_c03 %>% 
  distinct(codes, timefromstart, file) %>% 
  dplyr::rename('filename' = 'file') %>% 
  dplyr::mutate(subject = str_match(filename, "Subject (.*?)\\.txt")[,2],
                date = str_extract(filename, "\\d{4}-\\d{2}-\\d{2}"),
                time = gsub("h", ":", str_extract(filename, "\\d{2}h\\d{2}")),
                date = as.POSIXct(date)) %>% 
  dplyr::arrange(subject, date) %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(event_order = dplyr::row_number()) #ensure that events are in order


## 
# add the median codes 
## 45751 (delay 16)
# 45877
## break down by cohort

# extract squad (?) and box information


### COME BACK TO THIS #### XX 
# readsquadbox <- function(x){
#   squadbox <- fread(paste0("sed -n '9p; 10p' ", "'", x, "'"))
#   # squadbox$box <- fread(paste0("sed -n '10p' ", "'", x, "'"))
#   # squadbox %<>% rename("squad" = "V1")
#   squadbox$filename <- x
#   return(squadbox)
# }
# discounting_squadbox <- lapply(discounting_filenames_c03, readsquadbox) %>% rbindlist(fill = T)
# discounting_squadbox %<>%
#   mutate(ind = rep(c(1, 2),length.out = n())) %<>%
#   group_by(ind) %<>%
#   mutate(id = row_number()) %<>%
#   spread(ind, V1) %<>%
#   select(-id) %<>%
#   rename("box" = "1",
#          "squad" = "2")
# 
# discounting_df_expanded <- left_join(discounting_df_expanded, discounting_squadbox, by = "filename")

####################### XXX 

## check if the within file date information matches with filename information

## extract other variables

### EVENTS PRIOR TO CENTER NOSE POKE, CHOICE, AND DURING TIMEOUT
discountingevents_c03 <- discounting_df_expanded_c03 %>% 
  # group_by(filename, subject, date, time, box, squad) %>%
  group_by(filename, subject, date, time) %>%
  summarize(
    tot_num_trials = length(timefromstart[codes %in% c("-100", "-200", "-300")]),
    tot_num_free = length(timefromstart[codes %in% c("-100")]),
    tot_num_fd = length(timefromstart[codes %in% c("-200")]),
    tot_num_fi = length(timefromstart[codes %in% c("-300")]),
    tot_forced_trials = length(timefromstart[codes %in% c("-200", "-300")]),
    
    events_before_center = length(timefromstart[codes %in% c("-1", "-3", "-5", "-7")]),
    events_before_center_fd = length(timefromstart[codes %in% c("-101", "-103", "-105", "-107")]),
    events_before_center_fi = length(timefromstart[codes %in% c("-201", "-203", "-205", "-207")]),
    events_before_center_tot = sum(
      events_before_center,
      events_before_center_fd,
      events_before_center_fi
    ),
    
    events_before_choice = length(timefromstart[codes %in% c("-15", "-17", "-16")]),
    events_before_choice_fd = length(timefromstart[codes %in% c("-116", "-115", "-117")]),
    events_before_choice_fi = length(timefromstart[codes %in% c("-215", "-217", "-216")]),
    events_before_choice_tot = sum(
      events_before_choice,
      events_before_choice_fd,
      events_before_choice_fi
    ),
    
    events_during_to = length(timefromstart[codes %in%  c("-25", "-27", "-26", "-21", "-23")]),
    events_during_to_fd = length(timefromstart[codes %in% c("-125", "-127", "-126", "-121", "-123")]),
    events_during_to_fi = length(timefromstart[codes %in% c("-225", "-227", "-226", "-221", "-223")]),
    events_during_to_tot = sum(events_during_to, 
                               events_during_to_fd, 
                               events_during_to_fi),
    
  ) %>%
  as.data.frame() %>% 
  ungroup()



### AVERAGE REACTION TIME AND CHOICE REACTION TIME 
rxn_time_c03 <- lapply(discounting_df_expanded_c03 %>% select(-event_order) %>% subset(codes %in% c(-100, -13, -6, -11)) %>% split(., .$filename), function(x){
  y <- split(x, cumsum(1:nrow(x) %in% which(x$codes == -100)))
  reactiontimes <- lapply(y, function(x){
    x <- x %>% 
      mutate(
        rxn_time_free = ifelse(x$codes[2] == -6, x$timefromstart[2] - x$timefromstart[1], NA),
        choice_rxn_time_free = ifelse(x$codes[2] == -6, x$timefromstart[3] - x$timefromstart[2], NA)) %>%
      slice(1) %>%
      dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
  }) %>% rbindlist()
  return(reactiontimes)
}) %>% rbindlist() %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(avg_rxn_time_free = mean(rxn_time_free, na.rm = T),
                avg_choice_rxn_time_free = mean(choice_rxn_time_free, na.rm = T)) %>%
  slice(1) %>% 
  select(-c("rxn_time_free", "choice_rxn_time_free")) %>% ungroup()

### AVERAGE TIMEOUT DURATION (FREE)
timeout_duration_c03 <- lapply(discounting_df_expanded_c03 %>% select(-event_order) %>% subset(codes %in% c(-20, -100, -200, -300)) %>% split(., .$filename) , function(x){
  y <- split(x, cumsum(1:nrow(x) %in% which(x$codes == -20)))
  timeout <- lapply(y, function(x){
    x <- x %>% mutate(
      timeout_duration_free = x$timefromstart[2] - x$timefromstart[1]) %>%
      slice(1) %>%
      dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
  }) %>% rbindlist()
  return(timeout)
}) %>% rbindlist() %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(avg_timeout_dur_free = mean(timeout_duration_free, na.rm = T)) %>%
  dplyr::select(-c(timeout_duration_free)) %>% 
  slice(1) %>% ungroup()


### AVERAGE COLLECTION TIME (FREE)
collection_time_c03 <- lapply( discounting_df_expanded_c03 %>% select(-event_order) %>% subset(codes %in% c(-100, -51, -53, as.numeric(grep("(5|7)$", eventchoices_full$key, value = T)))) %>% split(., .$filename) , function(x){
  y <- split(x, cumsum(1:nrow(x) %in% which(x$codes %in% c(-51, -53))))
  collection <- lapply(y, function(x){
    x <- x %>% 
      mutate(
        collection_time = case_when(
          x$codes[1] == -51 & any(grepl("5$", x$codes)) ~ as.character(min(x$timefromstart[match(as.numeric(grep("5$", eventchoices_full$key, value = T)), x$codes)], na.rm = T) - x$timefromstart[1]),
          x$codes[1] == -53 & any(grepl("7$", x$codes)) ~ as.character(min(x$timefromstart[match(as.numeric(grep("7$", eventchoices_full$key, value = T)), x$codes)], na.rm = T) - x$timefromstart[1]),
          x$codes[1] == -51 & any(grepl("5$", x$codes)) == F ~ "NA",
          x$codes[1] == -53 & any(grepl("7$", x$codes)) == F ~ "NA",
          TRUE ~ "NA"),
        collection_time = as.numeric(collection_time)
      ) %>%
      slice(1) %>%
      dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
  }) %>% rbindlist()
  return(collection)
}) %>% rbindlist() %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(avg_collection_time_free = mean(collection_time, na.rm = T)) %>%
  dplyr::select(-c(collection_time)) %>% 
  slice(1) %>% ungroup()
# add timeout_time_avg <- timeout_time_bytrial after the rbindlist() call to get the vectors by trial


# EVENTS PRIOR TO IMMEDIATE REWARD COLLECTION
## getting events prior to immediate reward collection 
events_imm_c03 <- lapply(discounting_df_expanded_c03 %>% select(-event_order) %>% 
                       subset(codes %in% c(-51, -53, -1, -3, -7, -6, 
                                           -11, -13,
                                           -101, -103, -107,
                                           -201, -203, -207, 
                                           -17, -16, 
                                           -117, -116, 
                                           -217, -216,
                                           -27, -26,
                                           -21, -23,
                                           -127, -126, -121, -123, 
                                           -227, -226, -221, -223,
                                           -5, -15, -105,-205, -115, -215, -25, -125, -225)) %>% 
                       split(., cumsum(1:nrow(.) %in% which(.$codes %in% c(-51, -53,-5, -15, -105,-205, -115, -215, -25, -125, -225)))), function(x){
                         x <- x %>% 
                           mutate(
                             events_before_collect_imm = ifelse(x$codes[1] == -51, length(timefromstart[codes %in% c(-1, -3, -7, -6, 
                                                                                                                     -11, -13,
                                                                                                                     -101, -103, -107,
                                                                                                                     -201, -203, -207, 
                                                                                                                     -17, -16, 
                                                                                                                     -117, -116, 
                                                                                                                     -217, -216,
                                                                                                                     -27, -26,
                                                                                                                     -21, -23,
                                                                                                                     -127, -126, -121, -123, 
                                                                                                                     -227, -226, -221, -223)]), NA)) %>%  
                           slice(1) %>%
                           dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
                         return(x)
                       }) %>% rbindlist() %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(avg_events_before_collect_imm = sum(events_before_collect_imm, na.rm = T)) %>%
  dplyr::select(-c(events_before_collect_imm)) %>% 
  slice(1) %>% ungroup() ## remove -125, -225 ## remove -125, -225

# EVENTS PRIOR TO DELAYED REWARD COLLECTION
## getting events prior to delayed reward collection 
events_del_c03 <- lapply(discounting_df_expanded_c03 %>% 
                       select(-event_order) %>% 
                       subset(codes %in% c(-51, -53, -1, -3, -5, -6, 
                                           -11, -13,
                                           -101, -103, -105,
                                           -201, -203, -205, 
                                           -15, -16, 
                                           -115, -116, 
                                           -215, -216,
                                           -25, -26,
                                           -21, -23,
                                           -125, -126, -121, -123, 
                                           -225, -226, -221, -223,
                                           -7, -107, -207, -17, -117, -217, -27, -127, -227)) %>% 
                       split(., cumsum(1:nrow(.) %in% which(.$codes %in% c(-51, -53, -7, -107, -207, -17, -117, -217, -27, -127, -227)))), function(x){
                         x <- x %>% 
                           mutate(
                             events_before_collect_del = ifelse(x$codes[1] == -53, length(timefromstart[codes %in% c(-51, -1, -3, -5, -6,
                                                                                                                     -11, -13,
                                                                                                                     -101, -103, -105,
                                                                                                                     -201, -203, -205,
                                                                                                                     -15, -16,
                                                                                                                     -115, -116,
                                                                                                                     -215, -216,
                                                                                                                     -25, -26,
                                                                                                                     -21, -23,
                                                                                                                     -125, -126, -121, -123,
                                                                                                                     -225, -226, -221, -223)]), NA)) %>%
                           slice(1) %>%
                           dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
                         return(x)
                       }) %>% rbindlist() %>% 
  dplyr::group_by(filename) %>% 
  dplyr::mutate(avg_events_before_collect_del = sum(events_before_collect_del, na.rm = T)) %>%
  dplyr::select(-c(events_before_collect_del)) %>% 
  slice(1) %>% ungroup() ## remove -125, -225


# REWARDS COLLECTED
## getting events prior to immediate reward collection 
rewards_collected_c03 <- lapply(discounting_df_expanded_c03 %>%
                              select(-event_order) %>% 
                              subset(codes %in% c(-51, -53, 
                                                  -5,-105, -205, -15, -115, -215, -25, -125, -225,
                                                  -7, -107, -207, -17, -117, -217, -27, -127, -227)) %>% 
                              split(., cumsum(1:nrow(.) %in% which(.$codes %in% c(-51, -53)))), function(x){
                                x <- x %>% 
                                  mutate(
                                    collect = 
                                      case_when(
                                        x$codes[1] == -53 & any(grepl("7$", x$codes)) | x$codes[1] == -51 & any(grepl("5$", x$codes)) ~ 1,
                                        # TRUE ~ 0
                                        x$codes[1] == -53 & length(x$codes[which(x$codes == "-53")])>= 1 | x$codes[1] == -51 & length(x$codes[which(x$codes == "-51")])>= 1 ~ 0
                                      )) %>%
                                  slice(1) %>%
                                  subset(!is.na(collect)) %>% 
                                  dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
                                return(x)
                              }) %>% rbindlist() %>% 
  group_by(filename) %>% 
  dplyr::summarize(percent_reward_collected = sum(collect, na.rm = T)/length(collect) * 100) %>% ungroup() ## remove -125, -225




## join the df's together to create discounting master table
discountingvalidtraits_c03 <- list("discountingevents" = discountingevents_c03, 
                               "rxn_time" = rxn_time_c03,
                               "timeout_duration" = timeout_duration_c03,
                               "collection_time" = collection_time_c03,
                               "events_imm" = events_imm_c03,
                               "events_del" = events_del_c03,
                               "rewards_collected" = rewards_collected_c03
) %>% do.call(cbind, .)
discountingvalidtraits_c03 <- discountingvalidtraits_c03[!duplicated(as.list(discountingvalidtraits_c03))] ## remove duplicated columns, like subject, filename, etc
names(discountingvalidtraits_c03) <- sub(".*[.]", "", names(discountingvalidtraits_c03))
discountingvalidtraits_c03 %<>% 
  mutate(events_before_collect_tot = avg_events_before_collect_imm + avg_events_before_collect_del) %<>% 
  mutate(date = lubridate::ymd(as.character(date))) %<>%  
  left_join(., delays_c03, by = "filename") %<>% 
  select(filename, subject, date, time, delay, everything())
discountingvalidtraits_c03 <- discountingvalidtraits_c03[, c( setdiff(names(discountingvalidtraits_c03), c("events_before_collect_tot", "percent_reward_collected")), c("events_before_collect_tot", "percent_reward_collected"))] ## more flexible way of reordering the columns without using references
