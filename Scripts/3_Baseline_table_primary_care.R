
###SETUP
rm(list=ls())

##File IDs for required files
INFECTIONS_COHORT_FILEID = "file-#" #file IDs removed for sharing

##Packages
install.packages('tidyverse')
library(tidyverse)
install.packages('tableone')
library(tableone)

# Creating directory for downloadable content in ZIP
dir.create('saveables')

# Download cohort data file
system(paste('dx download', INFECTIONS_COHORT_FILEID))
# Load in cohort data
infections_df <- read_csv('infections_df_2025-01-13.csv')

#Filter just with GP data
infections_df <- infections_df %>% filter(with_GP ==TRUE)

#Make variable that is infection vs controls (for hospitalisation outcome)
infections_df <- infections_df %>% mutate(infection_control = ifelse(control_GP ==TRUE, "Control", "Infection"))

#Code female sex
infections_df$sex <- factor(infections_df$sex)
infections_df$sex <- relevel(infections_df$sex, ref="Female")
infections_df$femalesex <- relevel(factor(infections_df$sex), ref = "Male")

#Recode diabetes and smoking - combine NA and prefer not to answer 
infections_df <- infections_df %>% 
  mutate(diabetes_diagnosed = ifelse(diabetes_diagnosed == "Prefer not to answer" | is.na(diabetes_diagnosed), "Unknown diabetes", diabetes_diagnosed)) %>%
  mutate(smoking_status = ifelse(smoking_status == "Prefer not to answer" | is.na(smoking_status), "Unknown smoking", smoking_status))

#Specify variables
all_vars <- c("sex", "femalesex", "assess_age", "bmi", "hba1c", "glucose", "whr", "diabetes_diagnosed", "tdi", "smoking_status", 
              "bacterial_skininfection_GP", "fungal_skininfection_GP", "pneumonia_GP", "influenza_GP", "otherrespiratoryinfection_GP",
              "lower_resp_GP", "upper_resp_GP", "UTI_GP", "cystitis_GP", "pyelonephritis_GP", "genitalinfection_GP")
categorical_vars <-c("sex", "femalesex", "diabetes_diagnosed", "smoking_status")

tableone_all <- CreateTableOne(vars=all_vars,data= infections_df, strata = 'infection_control', factorVars=categorical_vars, test=FALSE)

tab_all <-as_tibble(print(tableone_all)) %>%
  add_column(measure=row.names(print(tableone_all))) %>%
  mutate(measure = trimws(measure)) %>%
  select(measure, Infection, Control)

# Save table as CSV
write.csv(tab_all, file = paste0("saveables/", "baseline_table_GP_all.csv"), row.names=FALSE)


##Also output baseline table by infection type to see if any differences

#Redo variables wihtout infections
all_vars <- c("sex", "femalesex", "assess_age", "bmi", "hba1c", "glucose", "whr", "diabetes_diagnosed", "tdi", "smoking_status")
categorical_vars <-c("sex", "femalesex", "diabetes_diagnosed", "smoking_status")

#Controls
tableone_controls <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(control_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_controls <-as_tibble(print(tableone_controls)) %>%
  add_column(measure=row.names(print(tableone_controls))) %>%
  mutate(measure = trimws(measure)) %>%
  rename(Control = Overall)

#Skin infection bacterial
tableone_skin_bact <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(bacterial_skininfection_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_skin_bact <-as_tibble(print(tableone_skin_bact)) %>%
  add_column(measure=row.names(print(tableone_skin_bact))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Bacterial skin infection' = Overall)

#Skin infection fungal
tableone_skin_fun <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(fungal_skininfection_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_skin_fun <-as_tibble(print(tableone_skin_fun)) %>%
  add_column(measure=row.names(print(tableone_skin_fun))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Fungal skin infection' = Overall)

#Pneumonia
tableone_pneumo <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(pneumonia_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_pneumo <-as_tibble(print(tableone_pneumo)) %>%
  add_column(measure=row.names(print(tableone_pneumo))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Bacterial pneumonia' = Overall)

#Influenza
tableone_flu <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(influenza_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_flu <-as_tibble(print(tableone_flu)) %>%
  add_column(measure=row.names(print(tableone_flu))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Influenza' = Overall)

#Other viral respiratory
tableone_resp <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(otherrespiratoryinfection_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_resp <-as_tibble(print(tableone_resp)) %>%
  add_column(measure=row.names(print(tableone_resp))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Other viral respiratory infection' = Overall)

#Lower
tableone_lowerresp <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(lower_resp_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_lowerresp <-as_tibble(print(tableone_lowerresp)) %>%
  add_column(measure=row.names(print(tableone_lowerresp))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Lower respiratory infection' = Overall)

#Upper
tableone_upperresp <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(upper_resp_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_upperresp <-as_tibble(print(tableone_upperresp)) %>%
  add_column(measure=row.names(print(tableone_upperresp))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Upper respiratory infection' = Overall)

#UTI
tableone_UTI <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(UTI_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_UTI <-as_tibble(print(tableone_UTI)) %>%
  add_column(measure=row.names(print(tableone_UTI))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Bacterial urinary tract infection' = Overall)

#Cystitis
tableone_cystitis <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(cystitis_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_cystitis <-as_tibble(print(tableone_cystitis)) %>%
  add_column(measure=row.names(print(tableone_cystitis))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Cystitis' = Overall)

#Pyelonephritis
tableone_pyelonephritis <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(pyelonephritis_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_pyelonephritis <-as_tibble(print(tableone_pyelonephritis)) %>%
  add_column(measure=row.names(print(tableone_pyelonephritis))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Pyelonephritis' = Overall)

#Genital infection
tableone_genitalinfection <- CreateTableOne(vars=all_vars,data= (infections_df %>% filter(genitalinfection_GP == TRUE)), factorVars=categorical_vars, test=FALSE)
tab_genitalinfection <-as_tibble(print(tableone_genitalinfection)) %>%
  add_column(measure=row.names(print(tableone_genitalinfection))) %>%
  mutate(measure = trimws(measure)) %>%
  rename('Fungal genital infection' = Overall)


#Combine
tab_all_infections <- tab_controls %>% select(measure, Control) %>% left_join(tab_skin_bact) %>% left_join(tab_skin_fun) %>% left_join(tab_pneumo) %>% left_join(tab_flu) %>%
  left_join(tab_resp) %>% left_join(tab_lowerresp) %>% left_join(tab_upperresp) %>% left_join(tab_UTI) %>% left_join(tab_cystitis) %>% left_join(tab_pyelonephritis) %>% 
  left_join(tab_genitalinfection)


# Save table as CSV
write.csv(tab_all_infections, file = paste0("saveables/", "baseline_table_GP_by_type.csv"), row.names=FALSE)
