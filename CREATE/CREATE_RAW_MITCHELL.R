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
  
  # for(i in 1:length(x)){
  C_elems <- fread(paste0("sed -n '22p' ", "'", x, "'")) 
  
  discounting <- fread(paste0("grep -m1 -A", C_elems$V1 -32, " '\\-100' ", "'", x, "'"))
# # }
#   indices <- which(discounting$V1 < 0)
#   discounting_split <- split(discounting, cumsum(1:nrow(discounting) %in% indices))
# 
#   discounting_split_newcols <- lapply(discounting_split, function(x){
#     names(x) <- "codes"
#     x$timefromstart <- tail(x$codes, 1)
#     x <- x[-nrow(x),]
# 
#     if(nrow(x) > 1){
#       x$reward <- x[2, 1]
#       x$adjustingamt <- x[3, 1]
#       x <- x[1,]
#     }
# 
#     return(x)
#   }) %>% rbindlist(fill = T)
#   discounting_split_newcols$file <- x
# 
#  return(discounting_split_newcols)
# return(C_elems)
return(discounting)
}

discounting_df <- lapply(discounting_filenames[1:2], readdiscounting) 
%>% rbindlist(fill = T)

discounting_df_expanded <- discounting_df %>% 
  mutate(subject = str_match(file, "Subject (.*?)\\.txt")[,2],
         date = str_extract(file, "\\d{4}-\\d{2}-\\d{2}"),
         time = gsub("h", ":", str_extract(file, "\\d{2}h\\d{2}")),
         date = as.POSIXct(date))


## check if the within file date information matches with filename information

## extract other variables


##  Test sessions last: 10 minutes or cumulative total 5 ml of water consumption

# The first and last patch changes of the test session were excluded 


#########################################
############ LOCOMOTOR ##################
#########################################