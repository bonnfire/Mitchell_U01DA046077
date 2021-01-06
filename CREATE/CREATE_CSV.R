## CREATE CSV FILES FOR DATABASE
setwd("~/Desktop/Database/csv files/u01_suzanne_mitchell")

# create csv's for master tables 
write.csv(mitchell_wfu_metadata_c01_05, "mitchell_wfu_metadata_c01_05.csv", row.names = F)

# create csv's for phenotypes 
write.csv(locomotor_gwas_metadata_c01_05, "locomotor_gwas_c01_05.csv", row.names = F)
# write.csv(discounting_gwas_metadata_c01_03, "discounting_gwas_c01_03.csv", row.names = F)
write.csv(mitchell_c01_04_gwas_dd, "~/Desktop/Database/csv files/u01_suzanne_mitchell/dd_gwas_c01_04.csv", row.names = F)

