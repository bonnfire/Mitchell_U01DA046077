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
    select(one_of("rfid", "subject", "box_color", "computer", "operant_box", "assigned_lever"))
  names(x) <- gsub("_", "", names(x))
  return(x)
})
metadata <- metadata %>% rbindlist(idcol = "cohort")



### extract the data created from michelle's lab
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
mitchell_discounting_excel_original <- read_excel_values_discounting("MergedExcelOutput.xlsm") %>% rbindlist() %>% 
  mutate(time = sub(".*_(\\d+h\\d+m)_.*", "\\1", V1) %>% strptime(format = "%Hh%Mm") %>% format("%H:%M")) %>% 
  select(V1, V2, V3, time, everything())



names(mitchell_discounting_excel_original) <- names(discountingvalidtraits)[names(discountingvalidtraits) != "time"] %>% 
  append(., "percent_reward_collected")
  mutate_at(vars(one_of), as.character) %>% 
  mutate_at(, as.numeric) %>%
  mutate_at( , lubridate::ymd)
  
  
  
