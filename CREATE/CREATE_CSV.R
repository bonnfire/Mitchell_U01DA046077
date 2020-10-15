## CREATE CSV FILES FOR DATABASE
setwd("~/Desktop/Database/csv files/u01_suzanne_mitchell")
write.csv(mitchell_wfu_metadata_c01_05, "mitchell_wfu_metadata_c01_05.csv", row.names = F)
write.csv(locomotor_gwas_metadata_c01_05, "locomotor_gwas_c01_05.csv", row.names = F)
write.csv(discounting_gwas_metadata_c01_03, "discounting_gwas_c01_03.csv", row.names = F)

