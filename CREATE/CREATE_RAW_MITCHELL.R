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




##  Test sessions last: 10 minutes or cumulative total 5 ml of water consumption

# The first and last patch changes of the test session were excluded 


#########################################
############ LOCOMOTOR ##################
#########################################