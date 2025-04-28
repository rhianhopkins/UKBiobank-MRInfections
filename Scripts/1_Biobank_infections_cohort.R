
###SETUP
rm(list=ls())

##File IDs for required files
#Note: file IDs removed for sharing

ICD10_CODES_FILEID = "file-#" # ICD10 codes for infections
READ_CODES_FILEID = "file-#" # Read codes for infections

GRS_2HG_FILEID = "file-#" # GRS scores by participant ID (EID)
GRS_FG_FILEID = "file-#"
GRS_GLYHBA1C_FILEID = "file-#"
GRS_NGLYHBA1C_FILEID = "file-#"

GCK_FILEID = "file-#" # GCK classification by EID

GRS_BMI_FILEID = "file-#" # BMI GRS

GP_COUNTS_CLINICAL_FILDID = "file-#" #IDs in gp clinical table and observation count
GP_COUNTS_SCRIPTS_FILDID = "file-#" #IDs in gp scripts table and prescription count

WITHDRAWN_IDS_FILDID = "file-#" #IDs that have withdrawn from Biobank

##Packages
install.packages('tidyverse')
library(tidyverse)

library(devtools)
source_url("https://raw.githubusercontent.com/hdg204/UKBB/main/UKBB_Health_Records_New_Project.R") #to use functions from github to work with Biobank health records

################################################################################

###Add infection hospitalisations

#Download code lists
system(paste0('dx download ', ICD10_CODES_FILEID))

# Load code lists into memory as dataframe
ICD10_codes <- read_delim("infections_ICD10_codes.txt", "\t")

### Bacterial
# Pull all participants with bacterial infection hospitalisations
bacterial_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_group == "bacterial") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Pneumonia
pneumonia_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "pneumonia") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Skin infection
bacterial_skininfection_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "skininfection" & infection_group == "bacterial") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# UTI
UTI_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "UTI") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

## Subgrouping UTI into cystitis and pyelonephritis

#Cystitis
cystitis_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "UTI" & infection_subgroup == "cystitis") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

#Pyelonephritis
pyelonephritis_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "UTI" & infection_subgroup == "pyelonephritis") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

###Viral
# Pull all participants with viral infection hospitalisations
viral_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_group == "viral") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Influenza
influenza_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "influenza") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Other respiratory 
otherrespiratoryinfection_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "otherrespiratoryinfection") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

## Subgroup splitting respiratory infection into upper and lower

#Lower
lower_resp_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "otherrespiratoryinfection" & infection_subgroup == "lower respiratory") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

#Upper
upper_resp_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "otherrespiratoryinfection" & infection_subgroup == "upper respiratory") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()


###Fungal
# Pull all participants with fungal infection hospitalisations
fungal_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_group == "fungal") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Genital infection
genitalinfection_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "genitalinfection") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()

# Skin infection
fungal_skininfection_eids <- read_ICD10(
  ICD10_codes %>% filter(infection_type == "skininfection" & infection_group == "fungal") %>% select(ICD10) %>%
    pull()) %>% select(eid) %>% pull()


# Pull baseline_table into a new infections_df dataframe and add infection variables
infections_df <- baseline_table %>% mutate(eid = as.character(eid)) %>%
  #Classify bacterial
  mutate(bacterial = as.logical(eid %in% bacterial_eids)) %>%
  mutate(pneumonia = as.logical(eid %in% pneumonia_eids)) %>%
  mutate(bacterial_skininfection = as.logical(eid %in% bacterial_skininfection_eids)) %>%
  mutate(UTI = as.logical(eid %in% UTI_eids)) %>%
  mutate(cystitis = as.logical(eid %in% cystitis_eids)) %>%
  mutate(pyelonephritis = as.logical(eid %in% pyelonephritis_eids)) %>%
  #Classify viral
  mutate(viral = as.logical(eid %in% viral_eids)) %>%
  mutate(influenza = as.logical(eid %in% influenza_eids)) %>%
  mutate(otherrespiratoryinfection = as.logical(eid %in% otherrespiratoryinfection_eids)) %>%
  mutate(lower_resp = as.logical(eid %in% lower_resp_eids)) %>%
  mutate(upper_resp = as.logical(eid %in% upper_resp_eids)) %>%
  #Classify fungal
  mutate(fungal = as.logical(eid %in% fungal_eids)) %>%
  mutate(genitalinfection = as.logical(eid %in% genitalinfection_eids)) %>%
  mutate(fungal_skininfection = as.logical(eid %in% fungal_skininfection_eids)) %>%
  #Identify infection free controls
  mutate(control = ifelse(!viral & !bacterial & !fungal, TRUE, FALSE))


################################################################################

###Add infections primary care

#Download code lists
system(paste0('dx download ', READ_CODES_FILEID))

# Load code lists into memory as dataframe
Read_codes <- read_delim("infections_Read_codes.txt", "\t")

### Bacterial
# Pull all participants with bacterial infection in GP record
bacterial_GP_eids <- read_GP(
  Read_codes %>% filter(infection_group == "bacterial") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Pneumonia
pneumonia_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "pneumonia") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Skin infection
bacterial_skininfection_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "skininfection" & infection_group == "bacterial") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# UTI
UTI_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "UTI") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

## Subgrouping UTI into cystitis and pyelonephritis

#Cystitis
cystitis_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "UTI" & infection_subgroup == "cystitis") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

#Pyelonephritis
pyelonephritis_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "UTI" & infection_subgroup == "pyelonephritis") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()


###Viral
# Pull all participants with viral infection in GP record
viral_GP_eids <- read_GP(
  Read_codes %>% filter(infection_group == "viral") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Influenza
influenza_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "influenza") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Other respiratory 
otherrespiratoryinfection_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "otherrespiratoryinfection") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

## Subgroup splitting respiratory infection into upper and lower

#Lower
lower_resp_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "otherrespiratoryinfection" & infection_subgroup == "lower respiratory") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

#Upper
upper_resp_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "otherrespiratoryinfection" & infection_subgroup == "upper respiratory") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()



###Fungal
# Pull all participants with fungal infection in GP record
fungal_GP_eids <- read_GP(
  Read_codes %>% filter(infection_group == "fungal") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Genital infection
genitalinfection_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "genitalinfection") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()

# Skin infection
fungal_skininfection_GP_eids <- read_GP(
  Read_codes %>% filter(infection_type == "skininfection" & infection_group == "fungal") %>% select(readcode) %>%
    pull()) %>% select(eid) %>% pull()


# Add GP infection variables to infections_df
infections_df <- infections_df %>%
  #Classify bacterial
  mutate(bacterial_GP = as.logical(eid %in% bacterial_GP_eids)) %>%
  mutate(pneumonia_GP = as.logical(eid %in% pneumonia_GP_eids)) %>%
  mutate(bacterial_skininfection_GP = as.logical(eid %in% bacterial_skininfection_GP_eids)) %>%
  mutate(UTI_GP = as.logical(eid %in% UTI_GP_eids)) %>%
  mutate(cystitis_GP = as.logical(eid %in% cystitis_GP_eids)) %>%
  mutate(pyelonephritis_GP = as.logical(eid %in% pyelonephritis_GP_eids)) %>%
  #Classify viral
  mutate(viral_GP = as.logical(eid %in% viral_GP_eids)) %>%
  mutate(influenza_GP = as.logical(eid %in% influenza_GP_eids)) %>%
  mutate(otherrespiratoryinfection_GP = as.logical(eid %in% otherrespiratoryinfection_GP_eids)) %>%
  mutate(lower_resp_GP = as.logical(eid %in% lower_resp_GP_eids)) %>%
  mutate(upper_resp_GP = as.logical(eid %in% upper_resp_GP_eids)) %>%
  #Classify fungal
  mutate(fungal_GP = as.logical(eid %in% fungal_GP_eids)) %>%
  mutate(genitalinfection_GP = as.logical(eid %in% genitalinfection_GP_eids)) %>%
  mutate(fungal_skininfection_GP = as.logical(eid %in% fungal_skininfection_GP_eids)) %>%
  #Identify infection free controls
  mutate(control_GP = ifelse(!viral_GP & !bacterial_GP & !fungal_GP, TRUE, FALSE))


################################################################################

###Add GRS data

##Download GRS data

system(paste0('dx download ', GRS_2HG_FILEID))
system(paste0('dx download ', GRS_FG_FILEID))
system(paste0('dx download ', GRS_GLYHBA1C_FILEID))
system(paste0('dx download ', GRS_NGLYHBA1C_FILEID))
system(paste0('dx download ', GRS_BMI_FILEID))

##GRS to dataframe
th_grs = read_csv("2hGlu_grs.csv")
fg_grs = read_csv("FG_grs.csv")
glyhba1c_grs = read_csv("HbA1c_gly_grs.csv")
nonhba1c_grs = read_csv("HbA1c_non_gly_grs.csv")
bmi_grs = read_tsv('lifted_GRSs.tsv')


##Join GRS info to cohort

infections_df <- infections_df %>%
  left_join(th_grs %>% rename(th_grs = grs) %>% mutate(eid = as.character(eid)), by = 'eid') %>%
  left_join(fg_grs %>% rename(fg_grs = grs) %>% mutate(eid = as.character(eid)), by = 'eid') %>%
  left_join(glyhba1c_grs %>% rename(hba1c_glyc_grs = grs) %>% mutate(eid = as.character(eid)), by = 'eid') %>%
  left_join(nonhba1c_grs %>% rename(hba1c_nonglyc_grs = grs) %>% mutate(eid = as.character(eid)), by = 'eid') %>%
  left_join(bmi_grs %>% dplyr::select(eid, bmi_73_grs) %>% mutate(eid = as.character(eid)), by = "eid") 

################################################################################

### Add GCK data

##Download
system(paste0('dx download ', GCK_FILEID))

## GCK to dataframe
gck_df = read.table("gck_patho_ids_450k.tsv", sep="\t", header=TRUE)

###Join GCK data to cohort

# Get EID array of all UKbb participants with pathogenic GCK mutations
gck_IDlist <- gck_df %>%
  filter(as.logical(pathogenic)) %>%
  select(eid) %>% pull()

# Load onto infections_df
infections_df <- infections_df %>%
  mutate(GCK = ifelse(eid %in% gck_IDlist, TRUE, FALSE))

################################################################################

### Flag which participants have primary care data

system(paste0('dx download ', GP_COUNTS_CLINICAL_FILDID))
system(paste0('dx download ', GP_COUNTS_SCRIPTS_FILDID))

gp_counts_clinical = read.table("unique_counts_clinical.csv")
gp_counts_scripts = read.table("unique_counts_scripts.csv")

with_GP_data <- gp_counts_clinical %>% select(eid = V2) %>% full_join(gp_counts_scripts %>% select(eid = V2)) %>% pull()

# Add to infections_df
infections_df <- infections_df %>%
  mutate(with_GP = ifelse(eid %in% with_GP_data, TRUE, FALSE))

################################################################################

#Remove participants that have withdrawn

system(paste0('dx download ', WITHDRAWN_IDS_FILDID))

excl_IDs = read.table("withdraw_IDs.csv")

excl_IDs <- excl_IDs %>% select(eid = V1) %>% pull()

infections_df <- infections_df %>%
  mutate(exclude = ifelse(eid %in% excl_IDs, TRUE, FALSE)) %>%
  filter(exclude == FALSE)

################################################################################

###Checks
summary(infections_df)

#Recode GP controls
infections_df <- infections_df %>% mutate(control_GP = ifelse(control_GP ==TRUE & with_GP == FALSE, FALSE, control_GP))

###Upload

# Generating time-stamped name for DF (identification and versioning purposes)
filename = paste0("infections_df_",Sys.Date(),".csv")

# Create .csv on UKbb session
write_csv(infections_df,file = filename)

# Upload file to UK-bb
system(paste0('dx upload ', filename))
