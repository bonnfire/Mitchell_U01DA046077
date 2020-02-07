## extract excel information to compare 

setwd("~/Dropbox (Palmer Lab)/Suzanne_Mitchell_U01/Protocol-materials/DD-programs/Data-Analysis-Information")

# extract the shipment files from this directory
shipmentfiles <- list.files(path = ".", pattern = "Shipment\\d.xlsm")
