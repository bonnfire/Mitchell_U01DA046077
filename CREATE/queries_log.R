## queries log
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Mitchell_U01DA046077/CREATE")
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
