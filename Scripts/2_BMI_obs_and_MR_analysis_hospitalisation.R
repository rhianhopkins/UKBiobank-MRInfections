
###SETUP
rm(list=ls())

##File IDs for required files
INFECTIONS_COHORT_FILEID = "file-#" #file IDs removed for sharing

##Packages
install.packages('tidyverse')
library(tidyverse)
install.packages('MASS')
library(MASS)

# Creating directory for downloadable content in ZIP
dir.create('saveables')

# Download cohort data file
system(paste('dx download', INFECTIONS_COHORT_FILEID))
# Load in cohort data
infections_df <- read_csv('infections_df_2025-01-13.csv')

################################################################################

### Observational associations (logistic regression)

bmi_obs_df <- infections_df %>% filter(!is.na(bmi)) # exclude those without BMI

# Run models

##Bacterial
#Pneumonia
pneumonia_lr <- glm(pneumonia ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(pneumonia == 1 | control == 1)), family = "binomial")
#Skin infection
bacterial_skininfection_lr <- glm(bacterial_skininfection ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(bacterial_skininfection == 1 | control == 1)), family = "binomial")
#UTI
UTI_lr <- glm(UTI ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(UTI == 1 | control == 1)), family = "binomial")
#Cystitis
cystitis_lr <- glm(cystitis ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(cystitis == 1 | control == 1)), family = "binomial")
#Pyelonephritis
pyelonephritis_lr <- glm(pyelonephritis ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(pyelonephritis == 1 | control == 1)), family = "binomial")

##Viral
#Influenza
influenza_lr <- glm(influenza ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(influenza == 1 | control == 1)), family = "binomial")
#Other respiratory infection
otherrespiratoryinfection_lr <- glm(otherrespiratoryinfection ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(otherrespiratoryinfection == 1 | control == 1)), family = "binomial")
#Lower respiratory infection
lower_resp_lr <- glm(lower_resp ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(lower_resp == 1 | control == 1)), family = "binomial")
#Upper respiratory infection
upper_resp_lr <- glm(upper_resp ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(upper_resp == 1 | control == 1)), family = "binomial")

##Fungal
#Skin infection
fungal_skininfection_lr <- glm(fungal_skininfection ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(fungal_skininfection == 1 | control == 1)), family = "binomial")
#Genital infection
genitalinfection_lr <- glm(genitalinfection ~ bmi + sex + recruit_age, data = (bmi_obs_df %>% filter(genitalinfection == 1 | control == 1)), family = "binomial")


# Pull coefficients, and p values into table
Obs_BMI <- data.frame(
  Infection = c("Pneumonia", "Bacterial skin infection", "UTI", "Cystitis", "Pyelonephritis", "Influenza", "Other viral respiratory", "Lower respiratory", "Upper respiratory", "Fungal skin infection", "Genital infection"),
  Coef = c(
    summary(pneumonia_lr)$coefficients["bmi", 1],
    summary(bacterial_skininfection_lr)$coefficients["bmi", 1],
    summary(UTI_lr)$coefficients["bmi", 1],
    summary(cystitis_lr)$coefficients["bmi", 1],
    summary(pyelonephritis_lr)$coefficients["bmi", 1],
    summary(influenza_lr)$coefficients["bmi", 1],
    summary(otherrespiratoryinfection_lr)$coefficients["bmi", 1],
    summary(lower_resp_lr)$coefficients["bmi", 1],
    summary(upper_resp_lr)$coefficients["bmi", 1],
    summary(fungal_skininfection_lr)$coefficients["bmi", 1],
    summary(genitalinfection_lr)$coefficients["bmi", 1]
  ),
  P_value = c(
    summary(pneumonia_lr)$coefficients["bmi", 4],
    summary(bacterial_skininfection_lr)$coefficients["bmi", 4],
    summary(UTI_lr)$coefficients["bmi", 4],
    summary(cystitis_lr)$coefficients["bmi", 4],
    summary(pyelonephritis_lr)$coefficients["bmi", 4],
    summary(influenza_lr)$coefficients["bmi", 4],
    summary(otherrespiratoryinfection_lr)$coefficients["bmi", 4],
    summary(lower_resp_lr)$coefficients["bmi", 4],
    summary(upper_resp_lr)$coefficients["bmi", 4],
    summary(fungal_skininfection_lr)$coefficients["bmi", 4],
    summary(genitalinfection_lr)$coefficients["bmi", 4]
  )
)

# Add confidence intervals
Obs_BMI <- Obs_BMI %>%
  mutate(
    Lower_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_lr, "bmi")[1],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_lr, "bmi")[1],
      Infection == "UTI" ~ confint(UTI_lr, "bmi")[1],
      Infection == "Cystitis" ~ confint(cystitis_lr, "bmi")[1],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_lr, "bmi")[1],
      Infection == "Influenza" ~ confint(influenza_lr, "bmi")[1],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_lr, "bmi")[1],
      Infection == "Lower respiratory" ~ confint(lower_resp_lr, "bmi")[1],
      Infection == "Upper respiratory" ~ confint(upper_resp_lr, "bmi")[1],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_lr, "bmi")[1],
      Infection == "Genital infection" ~ confint(genitalinfection_lr, "bmi")[1]
    ),
    Upper_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_lr, "bmi")[2],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_lr, "bmi")[2],
      Infection == "UTI" ~ confint(UTI_lr, "bmi")[2],
      Infection == "Cystitis" ~ confint(cystitis_lr, "bmi")[2],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_lr, "bmi")[2],
      Infection == "Influenza" ~ confint(influenza_lr, "bmi")[2],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_lr, "bmi")[2],
      Infection == "Lower respiratory" ~ confint(lower_resp_lr, "bmi")[2],
      Infection == "Upper respiratory" ~ confint(upper_resp_lr, "bmi")[2],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_lr, "bmi")[2],
      Infection == "Genital infection" ~ confint(genitalinfection_lr, "bmi")[2]
    ),
  )

# Add infection Ns
Obs_BMI <- Obs_BMI %>%
  mutate(case_when(
    Infection == "Pneumonia" ~ count(bmi_obs_df %>% filter(pneumonia ==1)),
    Infection == "Bacterial skin infection" ~ count(bmi_obs_df %>% filter(bacterial_skininfection ==1)),
    Infection == "UTI" ~ count(bmi_obs_df %>% filter(UTI ==1)),
    Infection == "Cystitis" ~  count(bmi_obs_df %>% filter(cystitis ==1)),
    Infection == "Pyelonephritis" ~  count(bmi_obs_df %>% filter(pyelonephritis ==1)),
    Infection == "Influenza" ~  count(bmi_obs_df %>% filter(influenza ==1)),
    Infection == "Other viral respiratory" ~  count(bmi_obs_df %>% filter(otherrespiratoryinfection ==1)),
    Infection == "Lower respiratory" ~  count(bmi_obs_df %>% filter(lower_resp ==1)),
    Infection == "Upper respiratory" ~  count(bmi_obs_df %>% filter(upper_resp ==1)),
    Infection == "Fungal skin infection" ~  count(bmi_obs_df %>% filter(fungal_skininfection ==1)),
    Infection == "Genital infection" ~  count(bmi_obs_df %>% filter(genitalinfection ==1))
  )
  )

# Set p <0.001
Obs_BMI <- Obs_BMI %>%  dplyr::select(Infection, N_cases = n, Coef, Lower_CI, Upper_CI, P_value) %>%
  mutate(P_value = ifelse(P_value <0.001, "<0.001", P_value), OR = exp(Coef), OR_lower = exp(Lower_CI), OR_upper = exp(Upper_CI)) %>%
  mutate(OR_5 = OR^5, OR_lower_5 = OR_lower^5, OR_upper_5 = OR_upper^5)

# Save table as CSV
write.csv(Obs_BMI, file = paste0("saveables/", "Obs_BMI_results_by_type.csv"), row.names=FALSE)

################################################################################

###MR

# Create linear model to predict genetic BMI from glycaemic BMI GRS
bmi_mr_assoc <- lm(data = infections_df, formula = bmi ~ bmi_73_grs, 
                   na.action="na.exclude") # remove anyone without GRS data

#Predict BMI for each person from GRS, plus residuals
bmi_mr_df <- infections_df %>%
  mutate(bmi_pred = predict(bmi_mr_assoc, infections_df),
         bmi_resid = residuals(bmi_mr_assoc,na.action=na.exclude)) %>% 
  filter(!is.na(bmi_pred) & !is.na(bmi)) # exclude those without BMI and genetically predicted BMI


# Run models

##Bacterial
#Pneumonia
pneumonia_mr <- glm(pneumonia ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(pneumonia == 1 | control == 1)), family = "binomial")
#Skin infection
bacterial_skininfection_mr <- glm(bacterial_skininfection ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(bacterial_skininfection == 1 | control == 1)), family = "binomial")
#UTI
UTI_mr <- glm(UTI ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(UTI == 1 | control == 1)), family = "binomial")
#Cystitis
cystitis_mr <- glm(cystitis ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(cystitis == 1 | control == 1)), family = "binomial")
#Pyelonephritis
pyelonephritis_mr <- glm(pyelonephritis ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(pyelonephritis == 1 | control == 1)), family = "binomial")

##Viral
#Influenza
influenza_mr <- glm(influenza ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(influenza == 1 | control == 1)), family = "binomial")
#Other respiratory infection
otherrespiratoryinfection_mr <- glm(otherrespiratoryinfection ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(otherrespiratoryinfection == 1 | control == 1)), family = "binomial")
#Lower respiratory infection
lower_resp_mr <- glm(lower_resp ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(lower_resp == 1 | control == 1)), family = "binomial")
#Upper respiratory infection
upper_resp_mr <- glm(upper_resp ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(upper_resp == 1 | control == 1)), family = "binomial")

##Fungal
#Skin infection
fungal_skininfection_mr <- glm(fungal_skininfection ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(fungal_skininfection == 1 | control == 1)), family = "binomial")
#Genital infection
genitalinfection_mr <- glm(genitalinfection ~ bmi_pred + sex + recruit_age + bmi_resid, data = (bmi_mr_df %>% filter(genitalinfection == 1 | control == 1)), family = "binomial")


# Pull coefficients, and p values into table
MR_BMI <- data.frame(
  Infection = c("Pneumonia", "Bacterial skin infection", "UTI", "Cystitis", "Pyelonephritis", "Influenza", "Other viral respiratory", "Lower respiratory", "Upper respiratory", "Fungal skin infection", "Genital infection"),
  Coef = c(
    summary(pneumonia_mr)$coefficients["bmi_pred", 1],
    summary(bacterial_skininfection_mr)$coefficients["bmi_pred", 1],
    summary(UTI_mr)$coefficients["bmi_pred", 1],
    summary(cystitis_mr)$coefficients["bmi_pred", 1],
    summary(pyelonephritis_mr)$coefficients["bmi_pred", 1],
    summary(influenza_mr)$coefficients["bmi_pred", 1],
    summary(otherrespiratoryinfection_mr)$coefficients["bmi_pred", 1],
    summary(lower_resp_mr)$coefficients["bmi_pred", 1],
    summary(upper_resp_mr)$coefficients["bmi_pred", 1],
    summary(fungal_skininfection_mr)$coefficients["bmi_pred", 1],
    summary(genitalinfection_mr)$coefficients["bmi_pred", 1]
  ),
  P_value = c(
    summary(pneumonia_mr)$coefficients["bmi_pred", 4],
    summary(bacterial_skininfection_mr)$coefficients["bmi_pred", 4],
    summary(UTI_mr)$coefficients["bmi_pred", 4],
    summary(cystitis_mr)$coefficients["bmi_pred", 4],
    summary(pyelonephritis_mr)$coefficients["bmi_pred", 4],
    summary(influenza_mr)$coefficients["bmi_pred", 4],
    summary(otherrespiratoryinfection_mr)$coefficients["bmi_pred", 4],
    summary(lower_resp_mr)$coefficients["bmi_pred", 4],
    summary(upper_resp_mr)$coefficients["bmi_pred", 4],
    summary(fungal_skininfection_mr)$coefficients["bmi_pred", 4],
    summary(genitalinfection_mr)$coefficients["bmi_pred", 4]
  )
)

# Add confidence intervals
MR_BMI <- MR_BMI %>%
  mutate(
    Lower_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_mr, "bmi_pred")[1],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_mr, "bmi_pred")[1],
      Infection == "UTI" ~ confint(UTI_mr, "bmi_pred")[1],
      Infection == "Cystitis" ~ confint(cystitis_mr, "bmi_pred")[1],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_mr, "bmi_pred")[1],
      Infection == "Influenza" ~ confint(influenza_mr, "bmi_pred")[1],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_mr, "bmi_pred")[1],
      Infection == "Lower respiratory" ~ confint(lower_resp_mr, "bmi_pred")[1],
      Infection == "Upper respiratory" ~ confint(upper_resp_mr, "bmi_pred")[1],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_mr, "bmi_pred")[1],
      Infection == "Genital infection" ~ confint(genitalinfection_mr, "bmi_pred")[1]
    ),
    Upper_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_mr, "bmi_pred")[2],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_mr, "bmi_pred")[2],
      Infection == "UTI" ~ confint(UTI_mr, "bmi_pred")[2],
      Infection == "Cystitis" ~ confint(cystitis_mr, "bmi_pred")[2],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_mr, "bmi_pred")[2],
      Infection == "Influenza" ~ confint(influenza_mr, "bmi_pred")[2],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_mr, "bmi_pred")[2],
      Infection == "Lower respiratory" ~ confint(lower_resp_mr, "bmi_pred")[2],
      Infection == "Upper respiratory" ~ confint(upper_resp_mr, "bmi_pred")[2],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_mr, "bmi_pred")[2],
      Infection == "Genital infection" ~ confint(genitalinfection_mr, "bmi_pred")[2]
    ),
  )

# Add infection Ns
MR_BMI <- MR_BMI %>%
  mutate(case_when(
    Infection == "Pneumonia" ~ count(bmi_mr_df %>% filter(pneumonia ==1)),
    Infection == "Bacterial skin infection" ~ count(bmi_mr_df %>% filter(bacterial_skininfection ==1)),
    Infection == "UTI" ~ count(bmi_mr_df %>% filter(UTI ==1)),
    Infection == "Cystitis" ~  count(bmi_mr_df %>% filter(cystitis ==1)),
    Infection == "Pyelonephritis" ~  count(bmi_mr_df %>% filter(pyelonephritis ==1)),
    Infection == "Influenza" ~  count(bmi_mr_df %>% filter(influenza ==1)),
    Infection == "Other viral respiratory" ~  count(bmi_mr_df %>% filter(otherrespiratoryinfection ==1)),
    Infection == "Lower respiratory" ~  count(bmi_mr_df %>% filter(lower_resp ==1)),
    Infection == "Upper respiratory" ~  count(bmi_mr_df %>% filter(upper_resp ==1)),
    Infection == "Fungal skin infection" ~  count(bmi_mr_df %>% filter(fungal_skininfection ==1)),
    Infection == "Genital infection" ~  count(bmi_mr_df %>% filter(genitalinfection ==1))
  )
  )

# Set p <0.001
MR_BMI <- MR_BMI %>% dplyr::select(Infection, N_cases = n, Coef, Lower_CI, Upper_CI, P_value) %>%
  mutate(P_value = ifelse(P_value <0.001, "<0.001", P_value), OR = exp(Coef), OR_lower = exp(Lower_CI), OR_upper = exp(Upper_CI)) %>%
  mutate(OR_5 = OR^5, OR_lower_5 = OR_lower^5, OR_upper_5 = OR_upper^5)

# Save table as CSV
write.csv(MR_BMI, file = paste0("saveables/", "MR_BMI_results_by_type.csv"), row.names=FALSE)

