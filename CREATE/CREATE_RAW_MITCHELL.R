### Extract and process RAW TEXT and CSV files

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

# Each subject performed 6 iterations of each time delay 0s,2s, 4s, 8s, 16s, and 24sin the delay discounting task(89X36 sessions)
discount_latinsquare_order <- c(4, 8, 2, 16, 0, 24,
                                2, 4, 0, 8, 24, 16,
                                8, 16, 4, 24, 2, 0,
                                24, 0 ,16, 2, 8, 4,
                                16, 24, 8, 0, 4, 2,
                                0, 2, 24, 4, 16, 8) 
# include code to unzip the files and duplicate the files from the original directory to the new one
#### XXXXXXXXXXXXx

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Suzanne_Mitchell_U01DA046077/Suzanne_Mitchell_U01/Data-discounting") # use duplicate folder that contains the unzipped files
discounting_filenames <- list.files(path = ".", pattern = "*.txt", recursive = T, full.names = T)

# extract critical information 
# readsubjectnum <- 

## sed -n '7p;22p;61p;67p;79p' '2019-02-07_09h41m_Subject 46067.txt' # works for highlighted portions 

## grep -A 3 -m1 '.3' '2019-02-07_09h41m_Subject 46067.txt'|tail -1


## currently they are writing python script to extract the following
## EXAMPLE USING THE ANNOTATED VERSION
# Subject#  
# sed -n '7p' assuming same location
# 
# Session  
# group by animals and order by date and time and assign session numbers? 
# OR 
# use the latin shipment information to find where the file falls in the square??
# 
# Number of trials (total) 
# sed -n '22p' (number of elements in C array) - 
# OR 
# 20 +23 +9+8=immediate SS choices+delay LL choices+immediate SS forced+delay LL forced
# 
# Number of free choice trials 
# 
# 
# Number of forced choice trials (code for Choice, Forced Choice, or Free Choice) 
# count the number of codes that exceed 100 or 200? 
# 
# Number of events before center nose poke 
# ? which center nose poke (\-6), the number of sessions between each center nose poke or just the first one?
# 
# 
# RT: Reaction Times (time from start to center nose poke) 
# 3803 is on line 104; the time after the first -6 code apppearance # assuming just first center nose poke
# 
# Number of events before “choice” 
# is this value 4454 - 3803 # assuming its the time after lever press 
# OR is this value 4116 - 3803 # assuming its the time after nosepoke
# OR Is this value 4116 - 3803 # assuming its any code after the center nose poke
# 
# CHRT: Choice Reaction Time (time from center to choice) 
# 
# Number of events before collection (delivered side head entry) 
# 
# Collection time (time from choice to delivered side head entry)  
# 
# Number of events in time out periods 
# number of timestamps/codes between Begin TO (\-20) line 117 and Begin Session (\-100) line  (since there isn't a code for end TO)
# 4 excluding the start and end? -27, -26, -25, -25
# ?? what to do with more than one timeout?
# 
# Duration of time out periods  
# 7579-4579 = times after those two codes (begin to and begin session) 
# 

# ideas: get the start of the session + number in c array
# 
# 
readCelems<- function(x){
  C_elems <- fread(paste0("sed -n '22p' ", "'", x, "'")) 
  C_elems$filename <- x
  return(C_elems)
}
readCelems_df <- lapply(discounting_filenames[1:10],readCelems) %>% rbindlist() %>% rename("C_elem" = "V1")

readdiscounting <- function(x){
  
  # C_elems <- fread(paste0("sed -n '22p' ", "'", x, "'")) 
  
  discounting <- fread(paste0("sed -n /-100/,/30000/p ", "'", x, "'"))
  discounting <- discounting[-nrow(discounting),]
  indices <- which(discounting$V1 < 0)
  discounting_split <- split(discounting, cumsum(1:nrow(discounting) %in% indices))
  
  discounting_split_newcols <- lapply(discounting_split, function(x){
    names(x) <- "codes"
    x$timefromstart <- tail(x$codes, 1)
    x <- x[-nrow(x),]
    
    if(nrow(x) > 1){
      x$reward <- x[2, 1]
      x$adjustingamt <- x[3, 1]
      x <- x[1,]
    }
    
    return(x)
  }) %>% rbindlist(fill = T)
  discounting_split_newcols$file <- x
  
  return(discounting_split_newcols)
  # return(C_elems)
  # return(discounting)
}

discounting_df <- lapply(discounting_filenames, readdiscounting) %>% rbindlist(fill = T) # 7162 files
# summary looks good, no positive numbers in codes and all positive times

discounting_df_expanded <- discounting_df %>% 
  rename('filename' = 'file') %>% 
  mutate(subject = str_match(file, "Subject (.*?)\\.txt")[,2],
         date = str_extract(file, "\\d{4}-\\d{2}-\\d{2}"),
         time = gsub("h", ":", str_extract(file, "\\d{2}h\\d{2}")),
         date = as.POSIXct(date))

# extract squad (?) and box information
readsquadbox <- function(x){
  squadbox <- fread(paste0("sed -n '9p; 10p' ", "'", x, "'"))
  # squadbox$box <- fread(paste0("sed -n '10p' ", "'", x, "'"))
  # squadbox %<>% rename("squad" = "V1")
  squadbox$filename <- x
  return(squadbox)
}
discounting_squadbox <- lapply(discounting_filenames, readsquadbox) %>% rbindlist(fill = T)
discounting_squadbox %<>%
  mutate(ind = rep(c(1, 2),length.out = n())) %<>%
  group_by(ind) %<>%
  mutate(id = row_number()) %<>%
  spread(ind, V1) %<>%
  select(-id) %<>% 
  rename("box" = "1",
         "squad" = "2")

discounting_df_expanded <- left_join(discounting_df_expanded, discounting_squadbox, by = "filename")
## check if the within file date information matches with filename information

## extract other variables

# total number of trials is free choice + forced delay + forced immediate
# won't work, psuedocode discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% sum(#below)
# free choice trials
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes == "-100") %>% nrow()
# forced delay trials
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes == "-200") %>% nrow()
# forced immediate trials 
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes == "-300") %>% nrow()
####################################
# events prior to center nose poke (free choice)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-1", "-3", "-5", "-7")) %>% nrow()
# events prior to center nose poke (forced delay)
### NOT WORKING discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-101", "-103", "-105 )) %>% nrow()
# events prior to center nose poke (forced immediate)
### NOT WORKING discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-201", "-203", "-205", "-207")) %>% nrow()
# total events prior to center nose poke
sum(priorcenter_free, priorcenter_fdel, priorcenter_fimm)

# events prior to choice (free choice)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-15", "-17", "-16", "-7")) %>% nrow()
# events prior to choice (forced delayed)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-101", "-103", "-105", "-107")) %>% nrow()
# events prior to choice (formed imm)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-201", "-203", "-205", "-207")) %>% nrow()
# total events prior to choice (all trials)
sum(priorchoice_free, priorchoice_fdel, priorchoice_fimm)

# events during timeout (free choice)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-25", "-27", "-26", "-21", "-23")) %>% nrow()
# events during timeout (forced delay)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-125", "-127", "-126", "-121", "-123")) %>% nrow()
# events during timeout (forced immediate)
discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% subset(codes %in% c("-225", "-227", "-226", "-221", "-223")) %>% nrow()
# total events prior to timeout (all trials)
sum(priorto_free, priorto_fdel, priorto_fimm)
####################################
# average reaction time (free choice)

# average choice reaction time (free choice)

# average timeout duration (free choice)

# average collection time (free choice)



# events prior immediate reward collection (free choice)

# events prior delayed reward collection (free choice)

# total events prior to reward collection

discounting_df_expanded %>% dplyr::filter(filename=="./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt") %>% count(codes)

# discounting_df <- discounting_df_expanded %>% 
#   group_by(filename) %>%   
#   mutate(free_choice = )
subset_disc <- discounting_df_expanded %>% subset(filename %in% c("./Ship1_Latin-square/2019-02-07_09h42m_Subject 46259.txt", 
                                                                  "./Ship1_Latin-square/2019-02-07_09h41m_Subject 46067.txt",
                                                                  "./Ship1_Latin-square/2019-02-07_10h48m_Subject 46260.txt"))


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
  
  
# subset_disc %>% 
#     group_by(filename) %>% 
#     summarize(subject = head(subject, 1),
#               date = head(date, 1),
#               time = head(time, 1),
#               box = head(box, 1),
#               squad = head(squad, 1),
#       
#               tot_num_trials = length(timefromstart[codes %in% c("-100", "-200", "-300")]),
#               tot_num_free = length(timefromstart[codes %in% c("-100")]), #potentialy subtract one
#               tot_num_fd = length(timefromstart[codes %in% c("-200")]),
#               tot_num_fi = length(timefromstart[codes %in% c("-300")]),
#               tot_forced_trials = length(timefromstart[codes %in% c("-200", "-300")]),
#                 
#               events_before_center = length(timefromstart[codes %in% c("-1", "-3", "-5", "-7")]),
#               events_before_center_fd = length(timefromstart[codes %in% c("-101", "-103", "-105", "-107")]),
#               events_before_center_fi = length(timefromstart[codes %in% c("-201", "-203", "-205", "-207")]),
#               events_before_center_tot = sum(events_before_center, events_before_center_fd, events_before_center_fi),
#               
#               events_before_choice = length(timefromstart[codes %in% c("-15", "-17", "-16", "-7")]),
#               events_before_choice_fd = length(timefromstart[codes %in% c("-101", "-103", "-105", "-107")]),
#               events_before_choice_fi = length(timefromstart[codes %in% c("-201", "-203", "-205", "-207")]),
#               events_before_choice_tot = sum(events_before_choice, events_before_choice_fd, events_before_choice_fi),
#               
#               events_during_to = length(timefromstart[codes %in%  c("-25", "-27", "-26", "-21", "-23")]),
#               events_during_to_fd = length(timefromstart[codes %in% c("-125", "-127", "-126", "-121", "-123")]),
#               events_during_to_fi = length(timefromstart[codes %in% c("-225", "-227", "-226", "-221", "-223")]),
#               events_during_to_tot = sum(events_during_to, events_during_to_fd, events_during_to_fi),
#               
#               events_before_collect_d = length(timefromstart[codes %in% c("-1", "-3", "-7", "-6", "-13", "-101", "-103", "-107", "-201", "-203", "-207", "-17","-16", "-117", "-116", "-217", "-216")] & | codes %in% c("-5") % ),
#               events_before_collect_i = length(timefromstart[codes %in% c("-225", "-227", "-226", "-221", "-223")]))

discountingvalidtraits <- discounting_df_expanded %>% 
  group_by(filename) %>% 
  summarize(subject = head(subject, 1),
            date = head(date, 1),
            time = head(time, 1),
            box = head(box, 1),
            squad = head(squad, 1),
            
            tot_num_trials = length(timefromstart[codes %in% c("-100", "-200", "-300")]),
            tot_num_free = length(timefromstart[codes %in% c("-100")]), #potentialy subtract one
            tot_num_fd = length(timefromstart[codes %in% c("-200")]),
            tot_num_fi = length(timefromstart[codes %in% c("-300")]),
            tot_forced_trials = length(timefromstart[codes %in% c("-200", "-300")]),
            
            events_before_center = length(timefromstart[codes %in% c("-1", "-3", "-5", "-7")]),
            
            avg_rxn_time = mean(timefromstart[codes %in% c("-6")])
            )
            
subset_disc%>% 
  group_by(filename) %>% 
  
  summarize(subject = head(subject, 1),
            date = head(date, 1),
            time = head(time, 1),
            box = head(box, 1),
            squad = head(squad, 1),
            
            tot_num_trials = length(timefromstart[codes %in% c("-100", "-200", "-300")]),
            tot_num_free = length(timefromstart[codes %in% c("-100")]), #potentialy subtract one
            tot_num_fd = length(timefromstart[codes %in% c("-200")]),
            tot_num_fi = length(timefromstart[codes %in% c("-300")]),
            tot_forced_trials = length(timefromstart[codes %in% c("-200", "-300")]),
            
            events_before_center = length(timefromstart[codes %in% c("-1", "-3", "-5", "-7")]),
            
            avg_rxn_time = mean(timefromstart[codes %in% c("-6")]),
            
  )

## finding choice_reaction_time
choice_reaction_time <- ddply(subset_disc, .(filename), transform, choicereactiontime=timefromstart-timefromstart[codes == -100])
subset_disc %>% dplyr::filter(codes == -6|!is.na(reward))
  
##  Test sessions last: 10 minutes or cumulative total 5 ml of water consumption

# The first and last patch changes of the test session were excluded 


#########################################
############ LOCOMOTOR ##################
#########################################