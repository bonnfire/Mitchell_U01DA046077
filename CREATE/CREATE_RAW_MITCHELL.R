### Extract and process RAW TEXT and CSV files
# remove all but one object from the global environment
# rm(list=setdiff(ls(), "WFU_Mitchell_test_df"))


library(tidyr)
library(lubridate)

#########################################
########## DISCOUNTING ##################
#########################################
# Shipment1 Latin Square (From U01_Shipment1_LS_Data_Files_Overview.pdf)
# Exceptions: 
# 46260ismissing a delay 2 sessionintended for 2/11/19,due to amacro error theanimal was run on delay4 instead of 2and the session was not made up prior tothe animalbeing euthanized
# 46266 is missing a delay 2 sessionintended for2/22/19. Due to a macro error,the animal was run on delay8instead ofdelay2and the session was not made up prior tothe animalbeing euthanized
# Expectations: 89 subjects; provided 3202/3204 sessions for analysis

# Shipment2 Latin Square (From U01_Shipment2_LS_Data_Files_Overview.pdf)
# Expectations: 110 subjects; provided 3960/3960 sessions for analysis


# unzip files if you haven't already
mitchell_uploaded_cohorts_disc <- data.frame(dirs = list.dirs("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Data-discounting", recursive = F)) %>%  # rec = F,don't include the path itself
  mutate(cohort = str_extract(dirs, "Ship\\d"))
  
mitchell_unzipped_cohorts_disc <- data.frame(dirs = list.dirs("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting", recursive = F)) %>%  # rec = F,don't include the path itself
  mutate(cohort = str_extract(dirs, "Ship\\d"))


setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting")


# extract DELAY from the files (two lines before the end time)


setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting") # use duplicate folder that contains the unzipped files
discounting_filenames <- list.files(path = ".", pattern = "*.txt", recursive = T, full.names = T) # 10749 files 
# discounting_filenames %>% as.data.frame() %>% mutate(shipment = str_extract(`.`, "/Ship.*/")) %>% select(shipment) %>% table ## see which cohorts and how many each

extract_delays<- function(x){
  delays <- fread(paste0("grep -a2 \"30000\" ", "'", x, "'", " | tail -n 1"))
  delays$filename <- x
  return(delays)
  }

delays <- lapply(discounting_filenames, extract_delays) %>% rbindlist() %>% 
  rename("delay" = "V1") %>% 
  mutate(delay = delay/1000)
delays <- delays %>% 
  mutate(delay = replace(delay, delay == 0.001, 0))

delays %>% 
  mutate(delay = replace(delay, delay == 0.001, 0)) %>% mutate(delay = as.character(delay)) %>% select(delay) %>% table()


######################################################
############ USE FOR EVENT CODES (NEED TO VERIFY)
######################################################

eventchoices <- data.frame("key" = c(-100, -1, -3, -5, -6, -7), 
                           "meaning" = c("begin_trial", "leverpress_imm", "leverpress_del", "nosepoke_imm", "nosepoke_center", "nosepoke_del"))
eventchoices_full <- eventchoices %>% 
  rbind(data.frame("key" = eventchoices$key[2:6] - 10, 
                   "meaning" = paste0(eventchoices$meaning[2:6], "_choice"))) %>% 
  rbind(data.frame("key" = -20, "meaning" = "begin_to")) %>% 
  rbind(data.frame("key" = eventchoices$key[2:6] - 20, 
                   "meaning" = paste0(eventchoices$meaning[2:6], "_to"))) %>% 
  rbind(data.frame("key" = c(-51, -53), "meaning" = c("reinforcer_imm", "reinforcer_del"))) %>% 
  rbind(data.frame("key" = .$key[2:11] - 100, 
                   "meaning" = paste0(.$meaning[2:11], "_fd"))) %>% 
  rbind(data.frame("key" = .$key[2:11] - 200, 
                   "meaning" = paste0(.$meaning[2:11], "_fi")))



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

discounting_df <- lapply(discounting_filenames, readdiscounting) %>% rbindlist(fill = T) # 7162 files
  # summary looks good, no positive numbers in codes and almost all positive timestamps (only one na in file == "./Ship1_Latin-square/2019-03-22_15h05m_Subject 45883.txt")
# discounting_df %>% add_count(file) %>% distinct(file, n) %>% mutate(n = as.numeric(n)) %>% select(n) %>% summary() MIN SHOULD NOT BE LESS THAN 10


discounting_df_expanded <- discounting_df %>% 
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
# discounting_squadbox <- lapply(discounting_filenames, readsquadbox) %>% rbindlist(fill = T)
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
discountingevents <- discounting_df_expanded %>% 
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
rxn_time <- lapply(discounting_df_expanded %>% select(-event_order) %>% subset(codes %in% c(-100, -13, -6, -11)) %>% split(., .$filename), function(x){
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

# add timeout_time_avg <- timeout_time_bytrial after the rbindlist() call to get the vectors by trial

### AVERAGE TIMES (FROM SHIPMENT MACROS) ** CHECK IN TO SEE IF WE NEED THESE
reactiontimes <- discounting_df_expanded %>% subset(codes %in% c(-100, -13, -6, -11)) %>% subset(filename %in% c("./Ship1_Latin-square/2019-02-07_09h42m_Subject 46259.txt", 
                                                                                                                 "./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt",
                                                                                                                 "./Ship1_Latin-square/2019-02-07_10h48m_Subject 46260.txt"))
rxn_time_bytrial <- lapply(split(reactiontimes, cumsum(1:nrow(reactiontimes) %in% which(reactiontimes$codes == -100))), function(x){
  x <- x %>% 
    mutate(
      rxn_time_del = ifelse(x$codes[3] == -13, x$timefromstart[2] - x$timefromstart[1], NA),
      choice_rxn_time_del = ifelse(x$codes[3] == -13, x$timefromstart[3] - x$timefromstart[2], NA),
      rxn_time_imm = ifelse(x$codes[3] == -11, x$timefromstart[2] - x$timefromstart[1], NA),
      choice_rxn_imm = ifelse(x$codes[3] == -11, x$timefromstart[3] - x$timefromstart[2], NA)) %>% 
    slice(1) %>% 
    dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
  return(x) 
}) %>% rbindlist() %>% 
  group_by(filename) %>% 
  dplyr::mutate(trialid = dplyr::row_number(),
                rxn_time_del = rxn_time_del/100,
                rxn_time_imm = rxn_time_imm/100) %>% 
  ungroup() # create individual trial information

# aggregate above individual trial information to get group values
rxntime_crt_avg <- rxn_time_bytrial %>% 
  dplyr::group_by(filename) %>% 
  slice(31:n()) %>% 
  dplyr::mutate(avg_rxn_time_del = mean(rxn_time_del, na.rm = T),
                avg_choice_rxn_time_del = mean(choice_rxn_time_del, na.rm = T),
                avg_rxn_time_imm = mean(rxn_time_imm, na.rm = T), 
                avg_choice_rxn_imm = mean(choice_rxn_imm, na.rm = T)) %>%
  dplyr::select(-c(rxn_time_del, choice_rxn_time_del, rxn_time_imm, choice_rxn_imm)) %>% 
  dplyr::filter(trialid == max(trialid)) ## won't be correct if you load plyr after dplyr

### AVERAGE TIMES (FROM SHIPMENT MACROS) ** CHECK IN TO SEE IF WE NEED THESE
# ********************** ABOVE ************************************************************************************

### AVERAGE TIMEOUT DURATION (FREE)
timeout_duration <- lapply(discounting_df_expanded %>% select(-event_order) %>% subset(codes %in% c(-20, -100, -200, -300)) %>% split(., .$filename) , function(x){
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
collection_time <- lapply( discounting_df_expanded %>% select(-event_order) %>% subset(codes %in% c(-100, -51, -53, as.numeric(grep("(5|7)$", eventchoices_full$key, value = T)))) %>% split(., .$filename) , function(x){
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
events_imm <- lapply(discounting_df_expanded %>% select(-event_order) %>% 
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
events_del <- lapply(discounting_df_expanded %>% 
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
rewards_collected <- lapply(discounting_df_expanded %>%
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
discountingvalidtraits <- list("discountingevents" = discountingevents, 
                               "rxn_time" = rxn_time,
                               "timeout_duration" = timeout_duration,
                               "collection_time" = collection_time,
                               "events_imm" = events_imm,
                               "events_del" = events_del,
                               "rewards_collected" = rewards_collected
                               ) %>% do.call(cbind, .)
discountingvalidtraits <- discountingvalidtraits[!duplicated(as.list(discountingvalidtraits))] ## remove duplicated columns, like subject, filename, etc
names(discountingvalidtraits) <- sub(".*[.]", "", names(discountingvalidtraits))
discountingvalidtraits %<>% 
  mutate(events_before_collect_tot = avg_events_before_collect_imm + avg_events_before_collect_del) %<>% 
  mutate(date = lubridate::ymd(as.character(date))) %<>%  
  left_join(., delays, by = "filename") %<>% 
  select(filename, subject, date, time, delay, everything())
discountingvalidtraits <- discountingvalidtraits[, c( setdiff(names(discountingvalidtraits), c("events_before_collect_tot", "percent_reward_collected")), c("events_before_collect_tot", "percent_reward_collected"))] ## more flexible way of reordering the columns without using references

## XX figure out how to remove the rep variable
## 3 traits that we should be cautious about rn are: events_before_center_fi and fd; and avg_collection_time_free 



#############################################################################################################################################################################################3

rxn_time_subset_time %>% 
  dplyr::group_by(filename) %>% 
  # slice(31:n()) %>% 
  dplyr::mutate(avg_rxn_time_free = mean(rxn_time_free, na.rm = T),
                avg_choice_rxn_time_free = mean(choice_rxn_time_free, na.rm = T),
                avg_rxn_time_del = mean(rxn_time_del, na.rm = T),
                avg_choice_rxn_time_del = mean(choice_rxn_time_del, na.rm = T),
                avg_rxn_time_imm = mean(rxn_time_imm, na.rm = T), 
                avg_choice_rxn_imm = mean(choice_rxn_imm, na.rm = T)) %>%
  # mutate_at(vars(contains('avg')), as.numeric) %>% 
  dplyr::select(-c(rxn_time_del, choice_rxn_time_del, rxn_time_imm, choice_rxn_imm)) %>% 
  dplyr::filter(trialid == max(trialid)) ## won't be correct if you load plyr after dplyr
# trialid == head(trialid)
 
## getting the timeout information 



# lapply(discounting_df_expanded %>% select(-event_order) %>% 
#           subset(filename=="./Ship1_Latin-square/2019-02-07_09h42m_Subject 46259.txt") %>% subset(codes %in% c(-100, -51, as.numeric(grep("(5|7)$", eventchoices_full$key, value = T)))) %>% split(., .$filename) 





## FOR PLOTTING, NEED THE DELAY AND REP INFORMATION
# timeout_duration %>% subset(grepl("Ship1", filename)&subject=="45877") %>% arrange(subject, date) %>% 
  




## testing on three files, each for diff subject
# subset_disc_onlyinterestedcodes <- subset_disc %>% subset(codes %in% c(-100, -13, -6, -11))
# test_rxntime_crt <- lapply(split(subset_disc_onlyinterestedcodes, cumsum(1:nrow(subset_disc_onlyinterestedcodes) %in% which(subset_disc_onlyinterestedcodes$codes == -100))), function(x){
#   x <- x %>% 
#     mutate(rxn_time_del = ifelse(x$codes[3] == -13, x$timefromstart[2] - x$timefromstart[1], NA),
#            choice_rxn_time_del = ifelse(x$codes[3] == -13, x$timefromstart[3] - x$timefromstart[2], NA),
#            rxn_time_imm = ifelse(x$codes[3] == -11, x$timefromstart[2] - x$timefromstart[1], NA),
#            choice_rxn_imm = ifelse(x$codes[3] == -11, x$timefromstart[3] - x$timefromstart[2], NA)) %>% 
#     slice(1) %>% 
#     dplyr::select(-one_of(c("codes", "timefromstart", "reward","adjustingamt")))
#   return(x) 
# }) %>% rbindlist() %>% 
#   dplyr::group_by(filename) %>% 
#   # dplyr::mutate(trial_id = paste0("trial_", dplyr::row_number()),
#   dplyr::mutate(trial_id = dplyr::row_number(),
#                 rxn_time_del = rxn_time_del/100,
#                 # choice_rxn_time_del = choice_rxn_time_del/100,
#                 rxn_time_imm = rxn_time_imm/100) %>% 
# # , choice_rxn_imm = choice_rxn_imm/100) %>% 
#   ungroup() # create individual trial information
# 
# # aggregate above individual trial information to get group values
# test_rxntime_crt_avg <- test_rxntime_crt %>% 
#   dplyr::group_by(filename) %>% 
#   slice(31:n()) %>% 
#   dplyr::mutate(avg_rxn_time_del = mean(rxn_time_del, na.rm = T),
#             avg_choice_rxn_time_del = mean(choice_rxn_time_del, na.rm = T),
#             avg_rxn_time_imm = mean(rxn_time_imm, na.rm = T), 
#             avg_choice_rxn_imm = mean(choice_rxn_imm, na.rm = T)) %>%
#   # mutate_at(vars(contains('avg')), as.numeric) %>% 
#   dplyr::select(-c(rxn_time_del, choice_rxn_time_del, rxn_time_imm, choice_rxn_imm)) %>% 
#   dplyr::filter(trial_id == max(trial_id)) ## won't be correct if you load plyr after dplyr
# 

# helpful if sapply, but changed to lapply
# %>% 
#   dplyr::rename(rxn_time = ".") %>% 
#   cbind(trial = paste0("trial_", 1:length(which(reactiontimes$codes == -100)))) %>% 
#   mutate(rxn_time = rxn_time/100)
# 
# rxn_time <- rxn_time %>% rbindlist()
#   
#   do.call("rbind",.)


##  Test sessions last: 10 minutes or cumulative total 5 ml of water consumption

# The first and last patch changes of the test session were excluded 




## INDIFFERENCE 
### trying to find the indifference points? 
# median adjusting amounts over trials 31-60 -> average to create composite value for each delay
indifference <- discounting_df %>% distinct(file) %>% 
  dplyr::rename('filename' = 'file') %>% 
  dplyr::mutate(subject = str_match(filename, "Subject (.*?)\\.txt")[,2],
                date = str_extract(filename, "\\d{4}-\\d{2}-\\d{2}"),
                time = gsub("h", ":", str_extract(filename, "\\d{2}h\\d{2}")),
                date = as.POSIXct(date)) %>% 
  dplyr::arrange(subject, date) %>% cbind(., ship1_2_bind) %>% 
  left_join(discounting_df, ., by = c("file" = "filename")) %>% 
  subset(!is.na(adjustingamt)) %>% dplyr::group_by(file) %>% 
  dplyr::mutate(trial = dplyr::row_number()) %>% 
  subset(trial > 30) %>% group_by(file) %>% dplyr::mutate(indifference_point = median(adjustingamt)) %>% select(file, indifference_point, subject, delay, date, time) %>% distinct() %>% 
  ungroup() %>% ################# pick up here to check if the delay is being assigned correctly
  group_by(subject, delay) %>% 
  summarize(composite_value = mean(indifference_point))



indifference %>% 
  ungroup() %>% 
  mutate(delay = as.integer(delay), 
         composite_value = as.integer(composite_value)) %>% 
  slice(1:150) %>% 
  ggplot() +
  geom_path(aes(x = delay, y = composite_value)) +
  facet_grid( ~subject) + 
  labs(y = "Average of indifference points") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, size = 10))

### 


## looking at distribution of indifference

## for each subject on each tral, the size of the reinforcer from the adjusting alternative (adjusting amount) 
# is recorded, even when the delayed alternative is selected 
# INDEX OF PREFERENCE 
# increases in the adjusting amount indicate choice of the delayed alternative while decreases suggest choice in adjusting alternative
# variability between 0s delaysesssions is usually larger than variability bw sessions w particular delay

pdf("mitchell_discounting_adjustingamounts_free.pdf", onefile = T)
for(i in 1:(discounting_df %>% select(file) %>% dplyr::mutate(subject = str_match(file, "Subject (.*?)\\.txt")[,2]) %>% distinct(subject) %>% nrow)){
# for(i in 1:2){
  
  
  # consider adding reps and making the median line really thick? 
  g <- discounting_df %>% subset(!is.na(reward)&codes %in% c(-11, -13)) %>% 
    dplyr::rename('filename' = 'file') %>% 
    left_join(., delays, by = "filename") %>% 
    dplyr::mutate(subject = str_match(filename, "Subject (.*?)\\.txt")[,2],
                  date = str_extract(filename, "\\d{4}-\\d{2}-\\d{2}"),
                  time = gsub("h", ":", str_extract(filename, "\\d{2}h\\d{2}")),
                  date = as.POSIXct(date)) %>% 
    dplyr::arrange(subject, date) %>%
    # subset(subject %in% c("46047")) %>%
    # subset(subject %in% c("46047", "46259")) %>%
    dplyr::group_by(filename) %>% 
    dplyr::mutate(trial = dplyr::row_number()) %>% 
    ungroup() %>% 
    group_by(subject, delay) %>%
    mutate(rep = dense_rank(date) %>% as.character()) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = trial, y = adjustingamt, color = rep)) +
    geom_point(aes(x = trial, y = adjustingamt), size = 0.2) + 
    stat_summary(aes(x = trial, y = adjustingamt), fun.y = "median", geom = "line", color = "black", size = 1.2) +
    ggforce::facet_grid_paginate(delay ~ subject, nrow = 6, ncol = 1, page = i) +
    labs(title = "Adjusting Amounts at All Free Trials (By Rep and Delay)")
  
  print(g)
  
}
dev.off()

## recreating macros values 
mitchell_raw_macro <- discounting_df %>% subset(!is.na(reward)&codes %in% c(-11, -13)) %>% 
  dplyr::rename('filename' = 'file') %>% 
  left_join(., delays, by = "filename") %>% 
  dplyr::mutate(subject = str_match(filename, "Subject (.*?)\\.txt")[,2],
                date = str_extract(filename, "\\d{4}-\\d{2}-\\d{2}"),
                time = gsub("h", ":", str_extract(filename, "\\d{2}h\\d{2}")),
                date = as.POSIXct(date)) %>% 
  dplyr::arrange(subject, date) %>%
  # subset(subject %in% c("46047")) %>%
  # subset(subject %in% c("46047", "46259")) %>%
  dplyr::group_by(filename) %>% 
  dplyr::mutate(trial = dplyr::row_number()) %>% 
  ungroup() %>% 
  group_by(subject, delay) %>%
  mutate(rep = dense_rank(date) %>% as.character()) %>% 
  ungroup() %>% 
  # subset(grepl("Ship1", filename)) %>%
  # group_by(filename) %>% 
  # dplyr::filter(max(trial) > 45) %>% 
  # ungroup() %>% 
  mutate(adjustingamt = trunc(adjustingamt * 10/10)) %>% # truncate after the first DAD as macro does
  dplyr::filter(trial > 30) %>% 
  group_by(filename, delay, subject, date, time, rep) %>% 
  summarize(median = median(adjustingamt),
            numoftrials = max(trial)) %>% 
  ungroup() %>% 
  mutate(filename = gsub("./Ship\\d_Latin-square/", "", filename))

  


  
  
  
  
  
  
  

#########################################
############ LOCOMOTOR ##################
#########################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-locomotor")
dates <- data.frame(V1=system(paste0("grep -ira1 \"creation\" | grep -irEo \"[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}\""), intern = T)) %>%  
  separate(V1, into=c("experiment", "date"), sep = ":") %>% 
  mutate(cohort = sub("\\D*(\\d{1}).*", "\\1", experiment) %>% str_pad(width = 2, side = "left", pad = "0"),
         experiment = str_match(experiment, "Shipment\\d_locomotor/(.*?)(U.*?)(comp|com)?.csv")[,3]) # 303 observations from shipments 1-5
  
  
locomotorfilenames <- list.files(pattern = "*.csv", recursive = T)
locomotor_raw <- lapply(locomotorfilenames, read.csv, skip = 58, header = T, sep = ',', stringsAsFactors = F)
names(locomotor_raw) <- locomotorfilenames
# names(locomotor_raw) <- str_match(locomotorfilenames, "Shipment\\d_locomotor/(.*?)(U.*?)(comp|com)?.csv")[,3]
locomotor_raw_df <- locomotor_raw %>%
  rbindlist(idcol = "filename", fill = T) %>% 
  clean_names %>% select_if(~sum(!is.na(.)) > 0) %>% 
  mutate(cohort = paste0("C", sub("\\D*(\\d{1}).*", "\\1", filename) %>% str_pad(width = 2, side = "left", pad = "0")),
         group = str_extract(filename, "gp\\d+")) %>% 
  left_join(dates, by = c("experiment", "cohort")) %>%
  mutate(subject_id = paste0("9330003200", subject_id)) %>% 
  rename("rfid" = "subject_id") %>% 
  left_join(WFU_Mitchell_test_df[,c("sex", "rfid", "dob")], by = c("rfid")) %>% 
  mutate(date = lubridate::mdy(date), 
         dob = lubridate::ymd(as.character(dob)),
         experimentage = as.numeric(difftime(date, dob, units = "days")),
         time = str_match(experiment, "U01-(.*?)-.*")[,2]) 
# %>% 
#   select(-one_of("date", "dob"))

locomotor_raw_df %>% ggplot(aes(x = cohort, y = experimentage, fill = sex, linetype =time )) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                         axis.text.y = element_text(size = 15),
                         legend.text = element_text(size = 15) ) +
  labs(title = "Boxplots of Experiment age at locomotor time 1 and 2 by cohort")
locomotor_raw_df %>% subset(is.na(date)) ## dropped animals
# NOT NA locomotor_raw_df %>% select(cohort, sex, rfid, experiment) %>% distinct(rfid, experiment) %>% group_by(rfid) %>% add_count(rfid) %>% ungroup() %>% subset(n!=1)


locomotors_vars <- c("total_distance_cm", "rest_time_s", "rest_episode_count", "movement_time_s", "movement_episode_count", "vertical_activity_count", 
                     "margin_time_legacy_s", "center_time_legacy_s")
my.summary = function(x) list(mean = mean(x), 
                              sd = sd(x),
                              skew = moments::skewness(x), 
                              kurt = moments::kurtosis(x), 
                              sum = sum(x))
locomotor_avg = setDT(locomotor_raw_df)[, as.list(unlist(lapply(.SD, my.summary))),  
                     by = .(cohort, experiment, cage, rfid, batch, time), 
                     .SDcols = locomotors_vars]

locomotor_avg <- locomotor_avg %>% 
  mutate(experiment = replace(experiment, experiment == "U01-t2-gp13-B"&rfid == "933000320046763", "U01-t2-gp13")) 

## note that batch is batch 1 for all, not the same as cohort
# locomotorvalidtraits_graph %>% 
#   mutate(batch = readr::parse_number(batch), 
#          cohort = readr::parse_number(cohort)) %>% 
#   subset(batch != cohort) %>% dim()

## how much raw data do we have for spleen/ceca data 
mitchell_spleenceca_toprocess %>% left_join(., locomotor_avg, by = c("rfid" = "subject_id")) %>% subset(is.na(total_distance_cm.mean))
  
  