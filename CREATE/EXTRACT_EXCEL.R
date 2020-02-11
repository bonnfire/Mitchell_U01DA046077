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
