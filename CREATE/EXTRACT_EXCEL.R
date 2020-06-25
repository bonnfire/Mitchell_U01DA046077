## extract excel information to compare 

setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")

# extract the shipment files from this directory
shipmentfiles <- list.files(path = ".", pattern = "Shipment\\d.xlsm") # 3 files 



## extract the excel metadata about boxes/squads
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/U01_Shipment_Details_")
metadatafiles <- list.files(path = ".", pattern = "*.xlsx") 
metadata <- lapply(metadatafiles, read_excel, sheet = "Sheet1")
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
setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information/KG Python Script Materials")
read_excel_values_discounting <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, range = cell_cols("B")) # only extract the second column bc this one has values, will assign the column names later
  df <- lapply(df, function(x){
    x <- x %>% t() %>% 
      as.data.frame()
    rownames(x) <- NULL
    return(x)
  })
  names(df) <- path_sheetnames
  return(df)
}
mitchell_discounting_excel_original <- read_excel_values_discounting("MergedExcelFiles2.xlsm") %>% rbindlist() %>% 
  mutate(time = sub(".*_(\\d+h\\d+m)_.*", "\\1", V1) %>% strptime(format = "%Hh%Mm") %>% format("%H:%M")) %>% 
  select(V1, V2, V3, time, everything())

names(mitchell_discounting_excel_original) <- names(discountingvalidtraits)

mitchell_discounting_excel <- mitchell_discounting_excel_original %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(-one_of("filename", "subject", "date", "time")), as.numeric) %>%
  mutate_at(vars(one_of("date")), lubridate::ymd)
  






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
mitchell_macro_xl <- lapply(mitchell_xl, extract_mitchell_macro_xl) %>% rbindlist() 
mitchell_macro_xl %>% mutate(subject = str_extract(filename, "Subject \\d+")) %>% distinct(subject) %>% dim

