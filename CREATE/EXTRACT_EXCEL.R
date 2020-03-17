## extract excel information to compare 

setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")

# extract the shipment files from this directory
shipmentfiles <- list.files(path = ".", pattern = "Shipment\\d.xlsm")



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
u01.importxlsx.colname <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F)
  names(df) <- path_sheetnames
  return(df)
}

mitchell_c01_xl <- u01.importxlsx.colname("AA_Processing_Macro_Shipment1.xlsm") %>% 
  lapply(., function(x){
    x <- x[c(1, 76:84), ] %>% 
      t() %>% 
      as.data.frame() %>% 
      separate(V1, into = c("delay", "rep"), sep = "\r\n") %>%
      select(-matches("V(3|4|5)")) %>% 
      rename("filename" = "V2",
             "conversion" = "V6",
             "median" = "V7",
             "microliter" = "V8",
             "numoftrials" = "V9",
             "trials45_bin" = "V10") %>%
      slice(-1) %>%
      dplyr::filter(!is.na(filename)) %>% 
      mutate(delay = as.numeric(str_extract_all(delay, "[0-9]+")), 
             rep = as.numeric(str_extract_all(rep, "[0-9]+")), 
             conversion = dplyr::first(conversion),
             conversion = as.numeric(as.character(conversion))) %>%     
      mutate_at(vars(c("median", "microliter", "numoftrials", "trials45_bin", "filename")), as.character) %>% 
      mutate_at(vars(c("median", "microliter", "numoftrials", "trials45_bin")), funs(na_if(., "A")))
    return(x)
  })












