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
# Subject#  
# 
# Session  
# 
# Number of trials (total) 
# 
# Number of free choice trials 
# 
# Number of forced choice trials (code for Choice, Forced Choice, or Free Choice) 
# 
# Number of events before center nose poke 
# 
# RT: Reaction Times (time from start to center nose poke) 
# 
# Number of events before “choice” 
# 
# CHRT: Choice Reaction Time (time from center to choice) 
# 
# Number of events before collection (delivered side head entry) 
# 
# Collection time (time from choice to delivered side head entry)  
# 
# Number of events in time out periods 
# 
# Duration of time out periods  





##  Test sessions last: 10 minutes or cumulative total 5 ml of water consumption

# The first and last patch changes of the test session were excluded 


#########################################
############ LOCOMOTOR ##################
#########################################