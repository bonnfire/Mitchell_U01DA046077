## extract excel information to compare 

setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")

# extract the shipment files from this directory
shipmentfiles <- list.files(path = ".", pattern = "Shipment\\d.xlsm") # 3 files 



## extract the excel metadata about boxes/squads
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_")
metadatafilesc01_05 <- list.files(path = ".", pattern = "*.xlsx") 
metadata_c01_05 <- lapply(metadatafilesc01_05, read_excel, sheet = "Import-Info")
names(metadata) = str_pad(regmatches(metadatafiles, gregexpr("[[:digit:]]+", metadatafiles)) %>% unlist(), 2, "left", "0")
metadata <- lapply(metadata, function(x){
  x <- x %>% clean_names %>% 
    rename("rfid" = "transponder_id",
           "subject" = "last_5") %>% 
    mutate(box_color = toupper(box_color)) %>% 
    select(one_of("rfid", "subject", "box_color", "computer", "operant_box", "assigned_lever"))
  names(x) <- gsub("_", "", names(x))
  return(x)
})
metadata <- metadata %>% rbindlist(idcol = "cohort")



### extract the data created from mitchell's lab (katie)
# setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information/KG Python Script Materials")
# read_excel_values_discounting <- function(xlname){
#   path_sheetnames <- excel_sheets(xlname)
#   df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, range = cell_cols("B")) # only extract the second column bc this one has values, will assign the column names later
#   df <- lapply(df, function(x){
#     x <- x %>% t() %>% 
#       as.data.frame()
#     rownames(x) <- NULL
#     return(x)
#   })
#   names(df) <- path_sheetnames
#   return(df)
# }
# mitchell_discounting_excel_original <- read_excel_values_discounting("MergedExcelFiles2.xlsm") %>% rbindlist() %>% 
#   mutate(time = sub(".*_(\\d+h\\d+m)_.*", "\\1", V1) %>% strptime(format = "%Hh%Mm") %>% format("%H:%M")) %>% 
#   select(V1, V2, V3, time, everything())
# 
# names(mitchell_discounting_excel_original) <- names(discountingvalidtraits)
# 
# mitchell_discounting_excel <- mitchell_discounting_excel_original %>% 
#   mutate_all(as.character) %>% 
#   mutate_at(vars(-one_of("filename", "subject", "date", "time")), as.numeric) %>%
#   mutate_at(vars(one_of("date")), lubridate::ymd)
  






### extract the data created from mitchell's lab  (macros)
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")
mitchell_xl <- list.files(path = ".", pattern = "AA_Processing_Macro_Shipment\\d+?.xlsm")
extract_mitchell_macro_xl <- function(x){
  
  u01.importxlsx.colname <- function(xlname){
    path_sheetnames <- excel_sheets(xlname)
    df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
    names(df) <- path_sheetnames
    return(df)
  }
  
  
  df <- u01.importxlsx.colname(x) %>% 
    lapply(., function(x){
      x <- x[c(1, 76:99), ] %>% 
        t() %>% 
        as.data.frame() %>% 
        separate(V1, into = c("delay", "rep"), sep = "\r\n") %>%
        select(-matches("V(3|4|5|1[1238]|2[34])")) %>% 
        rename("filename" = "V2",
               "conversion" = "V6",
               "median" = "V7",
               "microliter" = "V8",
               "numoftrials" = "V9",
               "trials45_bin" = "V10",
               "rxntime_delay_avg" = "V14",
               "choicerxntime_delay_avg" = "V15", 
               "rxntime_imm_avg" = "V16", 
               "choicerxntime_imm_avg" = "V17",
               "choicerxntime_delay_count" = "V19", 
               "choicerxntime_imm_count" = "V20", 
               "choicerxntime_count_sum" = "V21",
               "testsums_bin" = "V22",
               "percent_choice" = "V25") %>%
        slice(-1) %>%
        dplyr::filter(!is.na(filename)) %>% 
        mutate_all(as.character) %>% 
        mutate(delay = as.numeric(str_extract_all(delay, "[0-9]+")), 
               rep = as.numeric(str_extract_all(rep, "[0-9]+")), 
               conversion = dplyr::first(conversion),
               conversion = as.numeric(as.character(conversion))) %>%     
        mutate_at(vars(c("median", "microliter", "numoftrials", "trials45_bin", "filename")), as.character) %>% 
        mutate_at(vars(c("median", "microliter", "numoftrials", "trials45_bin")), funs(na_if(., "A"))) %>%
        mutate(testsums_bin = ifelse(testsums_bin == "yes", 1, 
                                     ifelse(testsums_bin == "no", 0, NA))) %>% 
        mutate_at(vars(-c("filename")), as.numeric)
      return(x)
    }) %>% rbindlist(idcol = "tab") %>% 
    dplyr::filter(grepl("Sub\\d+", tab)) %>% 
    select(-tab)
  return(df)
} 
mitchell_macro_xl <- lapply(mitchell_xl, extract_mitchell_macro_xl) 
names(mitchell_macro_xl) <- mitchell_xl
mitchell_macro_xl_df <- mitchell_macro_xl %>% 
  rbindlist(idcol = "cohort") %>% 
  mutate(cohort = str_extract(cohort, "Shipment\\d+") %>% parse_number() %>% str_pad(2, "left", "0"))
mitchell_macro_xl_df %>% mutate(subject = str_extract(filename, "Subject \\d+")) %>% distinct(cohort, subject) %>% select(cohort) %>% table()


### extract the summary data created from mitchell's lab  (macros)
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")
mitchell_xl <- list.files(path = ".", pattern = "AA_Processing_Macro_Shipment\\d+?.xlsm")
extract_mitchell_macro_summary_xl <- function(x){
  
  u01.importxlsx.colname <- function(xlname){
    path_sheetnames <- excel_sheets(xlname)
    df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
    names(df) <- path_sheetnames
    return(df)
  }
  
  
  df <- u01.importxlsx.colname(x) 
  
  if(any(grepl("Instructions", names(df)))){
    df <- df[-grep("Instructions", names(df))] # remove the sheet that is throwing an error becasue of [c(105:113), c(1:8)] 
  }
  
  df <- df %>% 
    lapply(., function(x){
      x <- x[c(105:113), c(1:8)] %>% 
        t() %>% 
        as.data.frame() %>%
        rename("delay" = "V1",
               "trials45_bin" = "V2",
               "mean_of_medians" = "V3",
               "n_analyzed" = "V4",
               "percent_choice" = "V5",
               "rxntime_delay_avg" = "V6",
               "choicerxntime_delay_avg" = "V7",
               "rxntime_imm_avg" = "V8",
               "choicerxntime_imm_avg" = "V9") %>%
        slice(-1) %>%
        mutate_all(as.character)
      return(x)
    }) %>% rbindlist(idcol = "tab") %>%
  dplyr::filter(grepl("Sub\\d+", tab))
  return(df)
} 
mitchell_macro_summary_extremes_xl <- lapply(mitchell_xl, extract_mitchell_macro_summary_xl) 
mitchell_macro_summary_extremes_xl_df <- mitchell_macro_summary_extremes_xl %>% 
  rbindlist() %>% 
  rowwise() %>%
  mutate(subject = gsub("Sub", "", tab)) %>% 
         # rfid = replace(rfid, parse_number(cohort) < 5, paste0("9330003200", subject_id)),
         # rfid = replace(rfid, parse_number(cohort) >= 5, paste0("9330003201", subject_id))) %>% 
  ungroup() %>% 
  left_join(mitchell_wfu_metadata_c01_05 %>%
              mutate(subject = str_extract(rfid, "\\d{5}$")) %>% 
              select(cohort, rfid, subject, dob, sex), 
            by = "subject") %>% 
  mutate_at(vars(-matches("tab|subject|cohort|rfid|dob|sex")), as.numeric) %>% 
  subset(!is.na(delay)) %>% 
  select(-tab,-subject,-trials45_bin) # subject and tab are redundant with rfid, trials45_bin was removed after verifying that it is 100% empty  
# check that names_from = delay will not have invalid values
mitchell_macro_summary_extremes_xl_df %>% select(delay) %>% table()

# turn into wide
mitchell_macro_summary_extremes_xl_df_wide <- mitchell_macro_summary_extremes_xl_df %>% 
  rename_at(vars(one_of("mean_of_medians", "n_analyzed", "percent_choice", "rxntime_delay_avg", 
                        "choicerxntime_delay_avg", "rxntime_imm_avg", "choicerxntime_imm_avg")), ~ paste0(., "_extremetrials")) %>% 
   pivot_wider(names_from = delay, 
              values_from = c(mean_of_medians_extremetrials, n_analyzed_extremetrials, percent_choice_extremetrials, rxntime_delay_avg_extremetrials, 
                              choicerxntime_delay_avg_extremetrials, rxntime_imm_avg_extremetrials, choicerxntime_imm_avg_extremetrials)) 


### extract the summary data created from mitchell's lab  (macros)
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")
mitchell_xl <- list.files(path = ".", pattern = "AA_Processing_Macro_Shipment\\d+?.xlsm")
extract_mitchell_macro_summary_xl_noextremes <- function(x){
  
  u01.importxlsx.colname <- function(xlname){
    path_sheetnames <- excel_sheets(xlname)
    df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
    names(df) <- path_sheetnames
    return(df)
  }
  
  
  df <- u01.importxlsx.colname(x) 
  
  if(any(grepl("Instructions", names(df)))){
    df <- df[-grep("Instructions", names(df))] # remove the sheet that is throwing an error because of [c(105, 117:123), c(1:8)] 
  }
  
  df <- df %>% 
    lapply(., function(x){
      x <- x[c(105, 117:123), c(1:8)] %>% 
        t() %>% 
        as.data.frame() %>%
        rename("delay" = "V1",
               "mean_of_medians" = "V2",
               "n_analyzed" = "V3",
               "percent_choice" = "V4",
               "rxntime_delay_avg" = "V5",
               "choicerxntime_delay_avg" = "V6",
               "rxntime_imm_avg" = "V7",
               "choicerxntime_imm_avg" = "V8") %>%
        slice(-1) %>%
        mutate_all(as.character)
      return(x)
    }) %>% rbindlist(idcol = "tab") %>%
    dplyr::filter(grepl("Sub\\d+", tab))
  return(df)
} 
mitchell_macro_summary_noextremes_xl <- lapply(mitchell_xl, extract_mitchell_macro_summary_xl_noextremes) 
mitchell_macro_summary_noextremes_xl_df <- mitchell_macro_summary_noextremes_xl %>% 
  rbindlist() %>% 
  rowwise() %>%
  mutate(subject = gsub("Sub", "", tab)) %>% 
  # rfid = replace(rfid, parse_number(cohort) < 5, paste0("9330003200", subject_id)),
  # rfid = replace(rfid, parse_number(cohort) >= 5, paste0("9330003201", subject_id))) %>% 
  ungroup() %>% 
  left_join(mitchell_wfu_metadata_c01_05 %>%
              mutate(subject = str_extract(rfid, "\\d{5}$")) %>% 
              select(cohort, rfid, subject, dob, sex), 
            by = "subject") %>% 
  mutate_at(vars(-matches("tab|subject|cohort|rfid|dob|sex")), as.numeric) %>% 
  subset(!is.na(delay)) %>% 
  select(-tab,-subject) # subject and tab are redundant with rfid, trials45_bin was removed after verifying that it is 100% empty  
# check that names_from = delay will not have invalid values
mitchell_macro_summary_noextremes_xl_df %>% select(delay) %>% table()

# turn into wide
mitchell_macro_summary_noextremes_xl_df_wide <- mitchell_macro_summary_noextremes_xl_df %>% 
  rename_at(vars(one_of("mean_of_medians", "n_analyzed", "percent_choice", "rxntime_delay_avg", 
                        "choicerxntime_delay_avg", "rxntime_imm_avg", "choicerxntime_imm_avg")), ~ paste0(., "_noextremetrials")) %>% 
  pivot_wider(names_from = delay, 
              values_from = c(mean_of_medians_noextremetrials, n_analyzed_noextremetrials, percent_choice_noextremetrials, rxntime_delay_avg_noextremetrials, 
                              choicerxntime_delay_avg_noextremetrials, rxntime_imm_avg_noextremetrials, choicerxntime_imm_avg_noextremetrials)) 



### extract metadata for experiments
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_")
# C01 locomotor 
c01_metadata_locomotor <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/MITCHELL #1 SHIPPING SHEET with notes.xlsx")$`Metadata-Locomotor` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>% 
  select(rfid, matches("locomotor|d\\d")) %>% 
  mutate(d1_time_in = sub("(\\d+)(\\d{2})", "\\1:\\2", d1_time_in),
         d2_time_in = sub("(\\d+)(\\d{2})", "\\1:\\2", d2_time_in)) # XX eventually should change the times, but not essential for these GWAS deadline

# C01 delay discounting
c01_metadata_dd <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/MITCHELL #1 SHIPPING SHEET with notes.xlsx")$`Metadata-DD` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>%
  rename_at(vars(one_of("squad_number", 
                        # "box_color", 
                        "computer", "operant_box", "assigned_lever")), ~ paste0("dd_", .)) %>% 
  select(rfid, starts_with("dd_"))


# C02 locomotor 
c02_metadata_locomotor <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #2 Shipping Sheet with notes.xlsx")$`Metadata-Locomotor` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>% 
  select(rfid, matches("locomotor|d\\d")) %>% 
  mutate_at(vars(matches("time")), ~format(., format = "%H:%M")) # extract the time from the datetime object, by default the date is incorrect


# C02 delay discounting
c02_metadata_dd <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #2 Shipping Sheet with notes.xlsx")$`Metadata-DD` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>%
  rename_at(vars(one_of("squad_number", 
                        # "box_color", 
                        "computer", "operant_box", "assigned_lever")), ~ paste0("dd_", .)) %>% 
  select(rfid, starts_with("dd_"))


# C03 locomotor 
c03_metadata_locomotor <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #3 Shipping Sheet_with cage order.xlsx")$`Metadata-Locomotor` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>% 
  select(rfid, matches("locomotor|d\\d")) %>% 
  mutate_at(vars(matches("time")), ~format(., format = "%H:%M"))

# C03 delay discounting
c03_metadata_dd <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #3 Shipping Sheet_with cage order.xlsx")$`Metadata-DD` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>%
  rename_at(vars(one_of("squad_number", 
                        # "box_color", 
                        "computer", "operant_box", "assigned_lever")), ~ paste0("dd_", .)) %>% 
  select(rfid, starts_with("dd_"))


# C04 locomotor 
c04_metadata_locomotor <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #4 Shipping sheet_Cage-ID-Info.xlsx")$`Metadata-Locomotor` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>% 
  select(rfid, matches("locomotor|d\\d")) %>% 
  mutate_at(vars(matches("time")), ~format(., format = "%H:%M"))

# C04 delay discounting
c04_metadata_dd <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/Mitchell #4 Shipping sheet_Cage-ID-Info.xlsx")$`Metadata-DD` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id",
         "squad_number" = "squad") %>%
  rename_at(vars(one_of("squad_number", 
                        # "box_color", 
                        "computer", "operant_box", "assigned_lever")), ~ paste0("dd_", .)) %>% 
  select(rfid, starts_with("dd_"))




# C05 locomotor 
c05_metadata_locomotor <- u01.importxlsx("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_/MITCHELL #5 Shipping Sheet_with_notes.xlsx")$`Metadata-Locomotor` %>% 
  clean_names() %>% 
  rename("rfid" = "transponder_id") %>% 
  select(rfid, matches("locomotor|d\\d")) %>% 
  mutate_at(vars(matches("time")), ~format(., format = "%H:%M"))


# C05 delay discounting 
## DD WILL NOT BE DONE FOR COHORT 5



# =============================================
# =============================================

# bind all locomotor 
locomotor_metadata_c01_05 <- bind_rows(c01_metadata_locomotor, c02_metadata_locomotor) %>% 
  bind_rows(c03_metadata_locomotor) %>% 
  bind_rows(c04_metadata_locomotor) %>% 
  bind_rows(c05_metadata_locomotor)

# bind all delay discounting 
dd_metadata_c01_04 <- bind_rows(c01_metadata_dd, c02_metadata_dd) %>% 
  bind_rows(c03_metadata_dd) %>% 
  bind_rows(c04_metadata_dd) 
# %>% 
  # bind_rows(c05_metadata_dd)

