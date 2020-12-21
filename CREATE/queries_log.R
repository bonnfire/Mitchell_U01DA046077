## queries log
## generate the sample sheets for the 8 larva
# read in sample barcode 
larva_samplebarcodelib <- read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/8 Larva Deep Sequenced Flowcell Sample-Barcode list .xlsx") %>% 
  clean_names()
# read in fastq filenames 
larva_samplebarcodelib %>% 
  rename("rfid" = "sample_id") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/fastq_deepseqlarva_filenames_run0179.csv") %>% mutate_all(as.character) %>% # write.csv(data.frame(fastq_filename = list.files(path = "201030_A00953_0176_BHLGGYDSXY", full.names=T)), row.names = F) in tscc
              separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>% 
              mutate(rfid = str_extract(fastq_filename, "^\\d+_Plate\\d+_\\D\\d+")) %>% 
              group_by(rfid) %>% summarise_all(~ toString(na.omit(.))) %>% mutate(runid = gsub(",.*", "", runid)),
            by = "rfid") %>% 
  mutate(rfid = gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid)) %>% 
  mutate(project_name = "r01_su_guo") %>% 
  mutate(filename = "8 Larva Deep Sequenced Flowcell Sample-Barcode list .xlsx") %>% 
  mutate(sex = NA,
         coatcolor = NA, 
         organism = "zebrafish", 
         strain = "Ekkwill fish") %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_run0179_n8larva.csv", row.names = F)

larva_samplebarcodelib %>% 
  rename("rfid" = "sample_id") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/fastq_deepseqlarva_filenames_run0176.csv") %>% mutate_all(as.character) %>% 
              separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>% 
              mutate(rfid = str_extract(fastq_filename, "^\\d+_Plate\\d+_\\D\\d+")) %>% 
              group_by(rfid) %>% summarise_all(~ toString(na.omit(.))) %>% mutate(runid = gsub(",.*", "", runid)),
            by = "rfid") %>% 
  mutate(rfid = gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid))%>% 
  mutate(project_name = "r01_su_guo") %>% 
  mutate(filename = "8 Larva Deep Sequenced Flowcell Sample-Barcode list .xlsx") %>% 
  mutate(sex = NA,
         coatcolor = NA, 
         organism = "zebrafish", 
         strain = "Ekkwill fish") %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_run0176_n8larva.csv", row.names = F)




hs_rats_n1536 <- read.csv("~/Desktop/Database/csv files/sample_tracking/hs_rats_1536_11042020_genotype_log.csv") %>% mutate_all(as.character)
hs_rats_n1536_2 <- hs_rats_n1536 %>% 
  select(-unique) %>% 
  mutate(file_location = gsub("_1536", "_n1536", file_location))
setwd("~/Desktop/Database/csv files/sample_tracking")
write.csv(hs_rats_n1536_2, "genotyping_log_n1536_11052020.csv", row.names = F)


# 10/30/2020
## regenerate the sample sheets in den's format 
# for 200221_A00953_0069_BH5T5LDSXY
sample_barcode_lib_10262020_flowcell1 %>% 
  left_join(read.csv("sample_metadata10272020.csv") %>% mutate_all(as.character) %>% select(-comments), by = c("rfid", "project_name") ) %>% 
  select(runid, fastq_files, library_name, pcr_barcode, rfid, project_name, barcode, filename, comments, flag, sex, coatcolor, organism, strain) %>%
  write.csv("200221_A00953_0069_BH5T5LDSXY_samplesheet.csv", row.names = F)

# for 200326_A00953_0086_BHC2FMDSXY
sample_barcode_lib_10262020_flowcell2 %>% 
  left_join(read.csv("sample_metadata10272020.csv") %>% mutate_all(as.character) %>% select(-comments), by = c("rfid", "project_name") ) %>% 
  left_join(pcal_df, by = "rfid") %>% 
  mutate(sex = coalesce(sex.x, sex.y)) %>% 
  select(-sex.y, -sex.x) %>% 
  mutate(organism = replace(organism, grepl("Plate", rfid), "zebrafish"),
         strain = replace(strain, grepl("Plate", rfid), "Ekkwill fish")) %>% 
  select(runid, fastq_files, library_name, pcr_barcode, rfid, project_name, barcode, filename, comments, flag, sex, coatcolor, organism, strain) %>% 
  write.csv("200326_A00953_0086_BHC2FMDSXY_samplesheet.csv", row.names = F)


# 10/28/2020 
## add family id for den
# third flowcell
sample_barcode_lib_10262020_flowcell3 %>% subset(organism == "rat") %>% left_join(
  rbind(WFU_Jhou_test_df[, c("rfid", "dames", "sires")], WFU_Mitchell_test_df[, c("rfid", "dames", "sires")]) %>% rbind(riptide_control_pedigree %>% rename("sires" = "sire", "dames" = "dam") %>% select(rfid, dames, sires)), # load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/P50/before_renewal/Riptide_control_Pedigree.RData") 
  by = "rfid") %>% 
  select(rfid, project_name, dames, sires) %>% 
  write.csv("flowcell3_familyid_n384.csv", row.names = F)

# fourth flowcell
sample_barcode_lib_10262020_flowcell4 %>% subset(organism == "rat") %>% left_join(
  rbind(WFU_Jhou_test_df[, c("rfid", "dames", "sires")], WFU_Mitchell_test_df[, c("rfid", "dames", "sires")]) %>% 
    rbind(WFU_Jerry_excel_orig_test_df[, c("rfid", "dames", "sires")]) %>% 
    rbind(WFU_Kalivas_test_df[, c("rfid", "dames", "sires")]), 
  by = "rfid") %>% 
  select(rfid, project_name, dames, sires) %>% 
  write.csv("flowcell4_familyid_n480.csv", row.names = F)



# fixing the sample barcode lib for all organisms
sample_barcode_lib_10262020 <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_10262020_n3451.csv") %>% 
  mutate_all(as.character)

# first batch # add metadata (sex and coat color, organism)
sample_barcode_lib_10262020_flowcell1 <- sample_barcode_lib_10262020 %>% subset(grepl("2020-01-16", filename)) %>%
  rowwise() %>% 
  mutate(rfid_genotype = rfid, 
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("-", "_", rfid_genotype)),
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid_genotype))) %>% 
  ungroup() %>% 
  left_join(seq01_fastq, by = c("pcr_barcode" = "plate"))

seq01_fastq <- read.csv("fastq_seq_01_filenames.csv") %>% 
  select(-X) %>% 
  separate(filename, into = c("runid", "filename"), sep = "/") %>% 
  subset(runid == "200221_A00953_0069_BH5T5LDSXY") %>% 
  mutate(plate = str_extract(filename, "Plate_\\d+") %>% parse_number) %>% 
  mutate(Rnum = str_extract(filename, "R\\d"), file = gsub("R\\d", "", filename)) %>% 
  select(-filename) %>% 
  mutate(runid = paste0(runid, ";", file)) %>% spread(Rnum, file) %>% 
  separate(runid, c("runid", "file"), ";") %>% 
  mutate(file = paste0(gsub("__", "_R1_", file), ";", gsub("__", "_R2_", file))) %>% 
  rename("fastq_files" = "file") %>% 
  select(-R1, -R2) %>% 
  distinct() %>% 
  mutate(library = str_extract(fastq_files, "Riptide\\d+"),
         plate = as.character(plate)) %>% 
  mutate(plate = coalesce(plate, library))

## add metadata to first flowcell
sample_barcode_lib_10262020_flowcell1 %>% 
  left_join(read.csv("sample_metadata10272020.csv") %>% mutate_all(as.character) %>% select(-comments), by = c("rfid", "project_name") ) %>% 
  write.csv("sample_barcode_lib_idconversion_flowcell1_n960.csv", row.names = F)


# second batch 
sample_barcode_lib_10262020_flowcell2 <- sample_barcode_lib_10262020 %>% subset(grepl("2020-03-16", filename)) %>%
  rowwise() %>% 
  mutate(rfid_genotype = rfid, 
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("-", "_", rfid_genotype)),
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid_genotype))) %>% 
  ungroup() %>% 
  left_join(seq02_fastq, by = c("library_name" = "library"))


seq02_fastq <- read.csv("fastq_seq_01_filenames.csv") %>% 
  select(-X) %>% 
  separate(filename, into = c("runid", "filename"), sep = "/") %>% 
  subset(runid == "200326_A00953_0086_BHC2FMDSXY") %>% 
  mutate(Rnum = str_extract(filename, "R\\d"), file = gsub("R\\d", "", filename)) %>% 
  select(-filename) %>% 
  mutate(runid = paste0(runid, ";", file)) %>% spread(Rnum, file) %>% 
  separate(runid, c("runid", "file"), ";") %>% 
  mutate(file = paste0(gsub("__", "_R1_", file), ";", gsub("__", "_R2_", file))) %>% 
  rename("fastq_files" = "file") %>% 
  select(-R1, -R2) %>% 
  distinct() %>% 
  mutate(library = str_extract(fastq_files, "Riptide\\d+"))

pcal <- read.xlsx("Sample_list_for_DNA_samples.xlsx", colNames = F) 
names(pcal) <- c("rfid", "X2", "sex", "comments") 
pcal_df <- pcal %>% 
  select(-X2, -comments) %>% 
  mutate(rfid = paste0("p.cal", str_pad(rfid,  2, "left", "0")),
         sex = toupper(sex))
     
## add metadata to second flowcell
sample_barcode_lib_10262020_flowcell2 %>% 
  left_join(read.csv("sample_metadata10272020.csv") %>% mutate_all(as.character) %>% select(-comments), by = c("rfid", "project_name") ) %>% 
  left_join(pcal_df, by = "rfid") %>% 
  mutate(sex = coalesce(sex.x, sex.y)) %>% 
  select(-sex.y, -sex.x) %>% 
  mutate(organism = replace(organism, grepl("Plate", rfid), "zebrafish"),
         strain = replace(strain, grepl("Plate", rfid), "Ekkwill fish")) %>% 
  write.csv("sample_barcode_lib_idconversion_flowcell2_n571.csv", row.names = F)

# third batch 
sample_barcode_lib_10262020_flowcell3 <- sample_barcode_lib_10262020 %>% subset(grepl("KN02", filename)) %>%
  rowwise() %>% 
  mutate(rfid_genotype = rfid, 
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("-", "_", rfid_genotype)),
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid_genotype))) %>% 
  ungroup() %>% 
  left_join(read.csv("kn02_fastq_sample_metadata_n960.csv") %>% mutate_all(as.character), 
            by = c("rfid", "project_name", "barcode", "library_name", "pcr_barcode", "filename", "comments", "flag"))
sample_barcode_lib_10262020_flowcell3 %>% write.csv("sample_barcode_lib_idconversion_kn02_n960.csv", row.names = F)

# fourth batch 
sample_barcode_lib_10262020_flowcell4 <- sample_barcode_lib_10262020 %>% subset(grepl("KN03", filename)) %>%
  rowwise() %>% 
  mutate(rfid_genotype = rfid, 
         rfid_genotype = gsub("-", "_", rfid_genotype), # to all, not just the larvae
         rfid_genotype = replace(rfid_genotype, grepl("Plate", rfid_genotype), gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid_genotype))) %>% 
  ungroup() %>% 
  left_join(read.csv("kn03_sample_sheet_n960.csv") %>% mutate_all(as.character) %>% select(-project_name, -barcode, -library_name, -pcr_barcode, -filename, -comments, -flag) %>% mutate(rfid = gsub("_", "-", rfid)), by = "rfid")
sample_barcode_lib_10262020_flowcell4 %>% write.csv("sample_barcode_lib_idconversion_kn03_n960.csv", row.names = F)


## XX pick up for milad's founders
## second batch - continued 
# read.csv("fastq_seq02_filenames.csv") %>% 
#   separate(fastq_filename, into = c("runid", "filename"), sep = "/") %>% 
#   mutate(plate = str_extract(filename, "Plate_\\d+") %>% parse_number) %>% 
#   mutate(fastq_files = paste0(filename, ";", gsub("R1", "R2", filename))) %>% 
#   subset(!grepl("R2", filename)) %>% select(-filename) %>% distinct() %>% 
#   mutate(library_name = gsub("_.*", "", fastq_files)) # Milad's founders 


# end of sample sheet recreation




read.csv("kn02_fastq_sample_metadata_n960.csv") %>% 
  mutate_all(as.character) %>% 
  left_join(read.csv("samplemetadata_10192020.csv") %>% 
              mutate_all(as.character), by = "rfid") %>% 
  write.csv("kn02_fastq_sample_metadata_n960.csv",row.names = F)


setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
read.csv("data-1600111650217.csv") %>% mutate(rfid = as.character(rfid)) %>% # csv generated from pgadmin
  left_join(flowcell_df[,c("rfid", "library")], by = "rfid") %>% 
  left_join(genotype_df[, c("sample_name", "library","genotype_id")], by = c("rfid" = "sample_name")) %>% select(genotype_id) %>% 
  unlist() %>% as.character() %>% write(file = "genotype_id_n64_mitchell.txt") # 64


read.csv("fastq_seq03_filenames.csv") %>%
  separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
  mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
         file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
  distinct(runid, file) %>% 
  mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% 
  subset(grepl("^R\\d+|^Fish_Breeders", fastq_files)) %>% 
  mutate(library_name = ifelse(grepl("^Fish_Breeders", fastq_files), "Fish Breeders", paste0("Riptide", parse_number(gsub("(R\\d+)_.*", "\\1", file)))),
         pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character)  %>% 
  select(-file) %>%  
  right_join(read.csv("kn03_sample_barcode_lib.csv") %>%
               mutate_all(as.character),
             by = c("library_name", "pcr_barcode")) %>%
  # right_join(read.csv("kn03_sample_barcode_lib.csv") %>% 
  #              mutate_all(as.character), 
  #            by = c("library_name", "pcr_barcode")) %>% 
  select(-ends_with(".1")) %>% 
  mutate(organism = ifelse(!grepl("guo", project_name), "rat", "zebrafish"), strain = ifelse(organism != "zebrafish", "Heterogenous stock", "Ekkwill fish")) %>% 
  mutate(rfid = gsub(" ", "", rfid)) %>% 
  mutate(rfid = gsub("-", "_", rfid)) %>% # for the demultiplexing
  naniar::replace_with_na_all(~ .x %in% c("NULL")) %>% 
  rowwise() %>% 
  mutate(sex = replace(sex , library_name == "Fish Breeders", str_extract(rfid, "[MF]"))) %>% 
  ungroup() %>% 
  write.csv("kn03_sample_sheet_n960.csv", row.names = F)



