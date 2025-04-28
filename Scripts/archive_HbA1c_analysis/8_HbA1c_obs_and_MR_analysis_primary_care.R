
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

#Drop people without GP data
infections_df <- infections_df %>% filter(with_GP ==TRUE)

################################################################################

### Observational associations (logistic regression)

hba1c_obs_df <- infections_df %>% filter(!is.na(hba1c)) # exclude those without hba1c


# Run models

##Bacterial
#Pneumonia
pneumonia_lr <- glm(pneumonia_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(pneumonia_GP == 1 | control_GP == 1)), family = "binomial")
#Skin infection
bacterial_skininfection_lr <- glm(bacterial_skininfection_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(bacterial_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#UTI
UTI_lr <- glm(UTI_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(UTI_GP == 1 | control_GP == 1)), family = "binomial")
#Cystitis
cystitis_lr <- glm(cystitis_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(cystitis_GP == 1 | control_GP == 1)), family = "binomial")
#Pyelonephritis
pyelonephritis_lr <- glm(pyelonephritis_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(pyelonephritis_GP == 1 | control_GP == 1)), family = "binomial")

##Viral
#Influenza
influenza_lr <- glm(influenza_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(influenza_GP == 1 | control_GP == 1)), family = "binomial")
#Other respiratory infection
otherrespiratoryinfection_lr <- glm(otherrespiratoryinfection_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(otherrespiratoryinfection_GP == 1 | control_GP == 1)), family = "binomial")
#Lower respiratory infection
lower_resp_lr <- glm(lower_resp_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(lower_resp_GP == 1 | control_GP == 1)), family = "binomial")
#Upper respiratory infection
upper_resp_lr <- glm(upper_resp_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(upper_resp_GP == 1 | control_GP == 1)), family = "binomial")

##Fungal
#Skin infection
fungal_skininfection_lr <- glm(fungal_skininfection_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(fungal_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#Genital infection
genitalinfection_lr <- glm(genitalinfection_GP ~ hba1c + sex + recruit_age, data = (hba1c_obs_df %>% filter(genitalinfection_GP == 1 | control_GP == 1)), family = "binomial")


# Pull coefficients, and p values into table
Obs_HbA1c <- data.frame(
  Infection = c("Pneumonia", "Bacterial skin infection", "UTI", "Cystitis", "Pyelonephritis", "Influenza", "Other viral respiratory", "Lower respiratory", "Upper respiratory", "Fungal skin infection", "Genital infection"),
  Coef = c(
    summary(pneumonia_lr)$coefficients["hba1c", 1],
    summary(bacterial_skininfection_lr)$coefficients["hba1c", 1],
    summary(UTI_lr)$coefficients["hba1c", 1],
    summary(cystitis_lr)$coefficients["hba1c", 1],
    summary(pyelonephritis_lr)$coefficients["hba1c", 1],
    summary(influenza_lr)$coefficients["hba1c", 1],
    summary(otherrespiratoryinfection_lr)$coefficients["hba1c", 1],
    summary(lower_resp_lr)$coefficients["hba1c", 1],
    summary(upper_resp_lr)$coefficients["hba1c", 1],
    summary(fungal_skininfection_lr)$coefficients["hba1c", 1],
    summary(genitalinfection_lr)$coefficients["hba1c", 1]
  ),
  P_value = c(
    summary(pneumonia_lr)$coefficients["hba1c", 4],
    summary(bacterial_skininfection_lr)$coefficients["hba1c", 4],
    summary(UTI_lr)$coefficients["hba1c", 4],
    summary(cystitis_lr)$coefficients["hba1c", 4],
    summary(pyelonephritis_lr)$coefficients["hba1c", 4],
    summary(influenza_lr)$coefficients["hba1c", 4],
    summary(otherrespiratoryinfection_lr)$coefficients["hba1c", 4],
    summary(lower_resp_lr)$coefficients["hba1c", 4],
    summary(upper_resp_lr)$coefficients["hba1c", 4],
    summary(fungal_skininfection_lr)$coefficients["hba1c", 4],
    summary(genitalinfection_lr)$coefficients["hba1c", 4]
  )
)

# Add confidence intervals
Obs_HbA1c <- Obs_HbA1c %>%
  mutate(
    Lower_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_lr, "hba1c")[1],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_lr, "hba1c")[1],
      Infection == "UTI" ~ confint(UTI_lr, "hba1c")[1],
      Infection == "Cystitis" ~ confint(cystitis_lr, "hba1c")[1],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_lr, "hba1c")[1],
      Infection == "Influenza" ~ confint(influenza_lr, "hba1c")[1],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_lr, "hba1c")[1],
      Infection == "Lower respiratory" ~ confint(lower_resp_lr, "hba1c")[1],
      Infection == "Upper respiratory" ~ confint(upper_resp_lr, "hba1c")[1],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_lr, "hba1c")[1],
      Infection == "Genital infection" ~ confint(genitalinfection_lr, "hba1c")[1]
    ),
    Upper_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_lr, "hba1c")[2],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_lr, "hba1c")[2],
      Infection == "UTI" ~ confint(UTI_lr, "hba1c")[2],
      Infection == "Cystitis" ~ confint(cystitis_lr, "hba1c")[2],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_lr, "hba1c")[2],
      Infection == "Influenza" ~ confint(influenza_lr, "hba1c")[2],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_lr, "hba1c")[2],
      Infection == "Lower respiratory" ~ confint(lower_resp_lr, "hba1c")[2],
      Infection == "Upper respiratory" ~ confint(upper_resp_lr, "hba1c")[2],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_lr, "hba1c")[2],
      Infection == "Genital infection" ~ confint(genitalinfection_lr, "hba1c")[2]
    ),
  )

# Add infection Ns
Obs_HbA1c <- Obs_HbA1c %>%
  mutate(case_when(
    Infection == "Pneumonia" ~ count(hba1c_obs_df %>% filter(pneumonia_GP ==1)),
    Infection == "Bacterial skin infection" ~ count(hba1c_obs_df %>% filter(bacterial_skininfection_GP ==1)),
    Infection == "UTI" ~ count(hba1c_obs_df %>% filter(UTI_GP ==1)),
    Infection == "Cystitis" ~  count(hba1c_obs_df %>% filter(cystitis_GP ==1)),
    Infection == "Pyelonephritis" ~  count(hba1c_obs_df %>% filter(pyelonephritis_GP ==1)),
    Infection == "Influenza" ~  count(hba1c_obs_df %>% filter(influenza_GP ==1)),
    Infection == "Other viral respiratory" ~  count(hba1c_obs_df %>% filter(otherrespiratoryinfection_GP ==1)),
    Infection == "Lower respiratory" ~  count(hba1c_obs_df %>% filter(lower_resp_GP ==1)),
    Infection == "Upper respiratory" ~  count(hba1c_obs_df %>% filter(upper_resp_GP ==1)),
    Infection == "Fungal skin infection" ~  count(hba1c_obs_df %>% filter(fungal_skininfection_GP ==1)),
    Infection == "Genital infection" ~  count(hba1c_obs_df %>% filter(genitalinfection_GP ==1))
  )
  )

# Set p <0.001
Obs_HbA1c <- Obs_HbA1c %>% dplyr::select(Infection, N_cases = n, Coef, Lower_CI, Upper_CI, P_value) %>%
  mutate(P_value = ifelse(P_value <0.001, "<0.001", P_value), OR = exp(Coef), OR_lower = exp(Lower_CI), OR_upper = exp(Upper_CI)) %>%
  mutate(OR_10 = OR^10, OR_lower_10 = OR_lower^10, OR_upper_10 = OR_upper^10)

# Save table as CSV
write.csv(Obs_HbA1c, file = paste0("saveables/", "Obs_HbA1c_results_primary_care_by_type.csv"), row.names=FALSE)

################################################################################

###MR (Glycaemic HbA1c)

# Create linear model to predict genetic HbA1c from glycaemic HbA1c GRS
hba1c_gly_assoc <- lm(data = infections_df, formula = hba1c ~ hba1c_glyc_grs, 
                      na.action="na.exclude") # remove anyone without GRS data

#Predict HbA1c for each person from GRS, plus residuals
hba1c_gly <- infections_df %>%
  mutate(gly_pred = predict(hba1c_gly_assoc, infections_df),
         gly_resid = residuals(hba1c_gly_assoc,na.action=na.exclude)) %>% 
  filter(!is.na(gly_pred) & !is.na(hba1c)) # exclude those without hba1c or genetically predicted HbA1c


# Run models

##Bacterial
#Pneumonia
pneumonia_gly <- glm(pneumonia_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(pneumonia_GP == 1 | control_GP == 1)), family = "binomial")
#Skin infection
bacterial_skininfection_gly <- glm(bacterial_skininfection_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(bacterial_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#UTI
UTI_gly <- glm(UTI_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(UTI_GP == 1 | control_GP == 1)), family = "binomial")
#Cystitis
cystitis_gly <- glm(cystitis_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(cystitis_GP == 1 | control_GP == 1)), family = "binomial")
#Pyelonephritis
pyelonephritis_gly <- glm(pyelonephritis_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(pyelonephritis_GP == 1 | control_GP == 1)), family = "binomial")

##Viral
#Influenza
influenza_gly <- glm(influenza_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(influenza_GP == 1 | control_GP == 1)), family = "binomial")
#Other respiratory infection
otherrespiratoryinfection_gly <- glm(otherrespiratoryinfection_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(otherrespiratoryinfection_GP == 1 | control_GP == 1)), family = "binomial")
#Lower respiratory infection
lower_resp_gly <- glm(lower_resp_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(lower_resp_GP == 1 | control_GP == 1)), family = "binomial")
#Upper respiratory infection
upper_resp_gly <- glm(upper_resp_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(upper_resp_GP == 1 | control_GP == 1)), family = "binomial")

##Fungal
#Skin infection
fungal_skininfection_gly <- glm(fungal_skininfection_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(fungal_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#Genital infection
genitalinfection_gly <- glm(genitalinfection_GP ~ gly_pred + sex + recruit_age + gly_resid, data = (hba1c_gly %>% filter(genitalinfection_GP == 1 | control_GP == 1)), family = "binomial")


# Pull coefficients, and p values into table
MR_gly_HbA1c <- data.frame(
  Infection = c("Pneumonia", "Bacterial skin infection", "UTI", "Cystitis", "Pyelonephritis", "Influenza", "Other viral respiratory", "Lower respiratory", "Upper respiratory", "Fungal skin infection", "Genital infection"),
  Coef = c(
    summary(pneumonia_gly)$coefficients["gly_pred", 1],
    summary(bacterial_skininfection_gly)$coefficients["gly_pred", 1],
    summary(UTI_gly)$coefficients["gly_pred", 1],
    summary(cystitis_gly)$coefficients["gly_pred", 1],
    summary(pyelonephritis_gly)$coefficients["gly_pred", 1],
    summary(influenza_gly)$coefficients["gly_pred", 1],
    summary(otherrespiratoryinfection_gly)$coefficients["gly_pred", 1],
    summary(lower_resp_gly)$coefficients["gly_pred", 1],
    summary(upper_resp_gly)$coefficients["gly_pred", 1],
    summary(fungal_skininfection_gly)$coefficients["gly_pred", 1],
    summary(genitalinfection_gly)$coefficients["gly_pred", 1]
  ),
  P_value = c(
    summary(pneumonia_gly)$coefficients["gly_pred", 4],
    summary(bacterial_skininfection_gly)$coefficients["gly_pred", 4],
    summary(UTI_gly)$coefficients["gly_pred", 4],
    summary(cystitis_gly)$coefficients["gly_pred", 4],
    summary(pyelonephritis_gly)$coefficients["gly_pred", 4],
    summary(influenza_gly)$coefficients["gly_pred", 4],
    summary(otherrespiratoryinfection_gly)$coefficients["gly_pred", 4],
    summary(lower_resp_gly)$coefficients["gly_pred", 4],
    summary(upper_resp_gly)$coefficients["gly_pred", 4],
    summary(fungal_skininfection_gly)$coefficients["gly_pred", 4],
    summary(genitalinfection_gly)$coefficients["gly_pred", 4]
  )
)

# Add confidence intervals
MR_gly_HbA1c <- MR_gly_HbA1c %>%
  mutate(
    Lower_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_gly, "gly_pred")[1],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_gly, "gly_pred")[1],
      Infection == "UTI" ~ confint(UTI_gly, "gly_pred")[1],
      Infection == "Cystitis" ~ confint(cystitis_gly, "gly_pred")[1],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_gly, "gly_pred")[1],
      Infection == "Influenza" ~ confint(influenza_gly, "gly_pred")[1],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_gly, "gly_pred")[1],
      Infection == "Lower respiratory" ~ confint(lower_resp_gly, "gly_pred")[1],
      Infection == "Upper respiratory" ~ confint(upper_resp_gly, "gly_pred")[1],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_gly, "gly_pred")[1],
      Infection == "Genital infection" ~ confint(genitalinfection_gly, "gly_pred")[1]
    ),
    Upper_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_gly, "gly_pred")[2],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_gly, "gly_pred")[2],
      Infection == "UTI" ~ confint(UTI_gly, "gly_pred")[2],
      Infection == "Cystitis" ~ confint(cystitis_gly, "gly_pred")[2],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_gly, "gly_pred")[2],
      Infection == "Influenza" ~ confint(influenza_gly, "gly_pred")[2],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_gly, "gly_pred")[2],
      Infection == "Lower respiratory" ~ confint(lower_resp_gly, "gly_pred")[2],
      Infection == "Upper respiratory" ~ confint(upper_resp_gly, "gly_pred")[2],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_gly, "gly_pred")[2],
      Infection == "Genital infection" ~ confint(genitalinfection_gly, "gly_pred")[2]
    ),
  )

# Add infection Ns
MR_gly_HbA1c <- MR_gly_HbA1c %>%
  mutate(case_when(
    Infection == "Pneumonia" ~ count(hba1c_gly %>% filter(pneumonia_GP ==1)),
    Infection == "Bacterial skin infection" ~ count(hba1c_gly %>% filter(bacterial_skininfection_GP ==1)),
    Infection == "UTI" ~ count(hba1c_gly %>% filter(UTI_GP ==1)),
    Infection == "Cystitis" ~  count(hba1c_gly %>% filter(cystitis_GP ==1)),
    Infection == "Pyelonephritis" ~  count(hba1c_gly %>% filter(pyelonephritis_GP ==1)),
    Infection == "Influenza" ~  count(hba1c_gly %>% filter(influenza_GP ==1)),
    Infection == "Other viral respiratory" ~  count(hba1c_gly %>% filter(otherrespiratoryinfection_GP ==1)),
    Infection == "Lower respiratory" ~  count(hba1c_gly %>% filter(lower_resp_GP ==1)),
    Infection == "Upper respiratory" ~  count(hba1c_gly %>% filter(upper_resp_GP ==1)),
    Infection == "Fungal skin infection" ~  count(hba1c_gly %>% filter(fungal_skininfection_GP ==1)),
    Infection == "Genital infection" ~  count(hba1c_gly %>% filter(genitalinfection_GP ==1))
  )
  )

# Set p <0.001
MR_gly_HbA1c <- MR_gly_HbA1c %>% dplyr::select(Infection, N_cases = n, Coef, Lower_CI, Upper_CI, P_value) %>%
  mutate(P_value = ifelse(P_value <0.001, "<0.001", P_value), OR = exp(Coef), OR_lower = exp(Lower_CI), OR_upper = exp(Upper_CI)) %>%
  mutate(OR_10 = OR^10, OR_lower_10 = OR_lower^10, OR_upper_10 = OR_upper^10)

# Save table as CSV
write.csv(MR_gly_HbA1c, file = paste0("saveables/", "MR_gly_HbA1c_results_primary_care_by_type.csv"), row.names=FALSE)

################################################################################

###MR (Non-glycaemic HbA1c)

# Create linear model to predict genetic HbA1c from non-glycaemic HbA1c GRS
hba1c_non_gly_assoc <- lm(data = infections_df, formula = hba1c ~ hba1c_nonglyc_grs,
                          na.action="na.exclude") # remove anyone without GRS data

#Predict HbA1c for each person from GRS, plus residuals
hba1c_non_gly <- infections_df %>%
  mutate(non_gly_pred = predict(hba1c_non_gly_assoc, infections_df),
         non_gly_resid = residuals(hba1c_non_gly_assoc,na.action=na.exclude)) %>% 
  filter(!is.na(non_gly_pred) & !is.na(hba1c)) # exclude those without hba1c or genetically predicted HbA1c


# Run models
##Bacterial
#Pneumonia
pneumonia_non_gly <- glm(pneumonia_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(pneumonia_GP == 1 | control_GP == 1)), family = "binomial")
#Skin infection
bacterial_skininfection_non_gly <- glm(bacterial_skininfection_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(bacterial_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#UTI
UTI_non_gly <- glm(UTI_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(UTI_GP == 1 | control_GP == 1)), family = "binomial")
#Cystitis
cystitis_non_gly <- glm(cystitis_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(cystitis_GP == 1 | control_GP == 1)), family = "binomial")
#Pyelonephritis
pyelonephritis_non_gly <- glm(pyelonephritis_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(pyelonephritis_GP == 1 | control_GP == 1)), family = "binomial")

##Viral
#Influenza
influenza_non_gly <- glm(influenza_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(influenza_GP == 1 | control_GP == 1)), family = "binomial")
#Other respiratory infection
otherrespiratoryinfection_non_gly <- glm(otherrespiratoryinfection_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(otherrespiratoryinfection_GP == 1 | control_GP == 1)), family = "binomial")
#Lower respiratory infection
lower_resp_non_gly <- glm(lower_resp_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(lower_resp_GP == 1 | control_GP == 1)), family = "binomial")
#Upper respiratory infection
upper_resp_non_gly <- glm(upper_resp_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(upper_resp_GP == 1 | control_GP == 1)), family = "binomial")

##Fungal
#Skin infection
fungal_skininfection_non_gly <- glm(fungal_skininfection_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(fungal_skininfection_GP == 1 | control_GP == 1)), family = "binomial")
#Genital infection
genitalinfection_non_gly <- glm(genitalinfection_GP ~ non_gly_pred + sex + recruit_age + non_gly_resid, data = (hba1c_non_gly %>% filter(genitalinfection_GP == 1 | control_GP == 1)), family = "binomial")


# Pull coefficients, and p values into table
MR_non_gly_HbA1c <- data.frame(
  Infection = c("Pneumonia", "Bacterial skin infection", "UTI", "Cystitis", "Pyelonephritis", "Influenza", "Other viral respiratory", "Lower respiratory", "Upper respiratory", "Fungal skin infection", "Genital infection"),
  Coef = c(
    summary(pneumonia_non_gly)$coefficients["non_gly_pred", 1],
    summary(bacterial_skininfection_non_gly)$coefficients["non_gly_pred", 1],
    summary(UTI_non_gly)$coefficients["non_gly_pred", 1],
    summary(cystitis_non_gly)$coefficients["non_gly_pred", 1],
    summary(pyelonephritis_non_gly)$coefficients["non_gly_pred", 1],
    summary(influenza_non_gly)$coefficients["non_gly_pred", 1],
    summary(otherrespiratoryinfection_non_gly)$coefficients["non_gly_pred", 1],
    summary(lower_resp_non_gly)$coefficients["non_gly_pred", 1],
    summary(upper_resp_non_gly)$coefficients["non_gly_pred", 1],
    summary(fungal_skininfection_non_gly)$coefficients["non_gly_pred", 1],
    summary(genitalinfection_non_gly)$coefficients["non_gly_pred", 1]
  ),
  P_value = c(
    summary(pneumonia_non_gly)$coefficients["non_gly_pred", 4],
    summary(bacterial_skininfection_non_gly)$coefficients["non_gly_pred", 4],
    summary(UTI_non_gly)$coefficients["non_gly_pred", 4],
    summary(cystitis_non_gly)$coefficients["non_gly_pred", 4],
    summary(pyelonephritis_non_gly)$coefficients["non_gly_pred", 4],
    summary(influenza_non_gly)$coefficients["non_gly_pred", 4],
    summary(otherrespiratoryinfection_non_gly)$coefficients["non_gly_pred", 4],
    summary(lower_resp_non_gly)$coefficients["non_gly_pred", 4],
    summary(upper_resp_non_gly)$coefficients["non_gly_pred", 4],
    summary(fungal_skininfection_non_gly)$coefficients["non_gly_pred", 4],
    summary(genitalinfection_non_gly)$coefficients["non_gly_pred", 4]
  )
)

# Add confidence intervals
MR_non_gly_HbA1c <- MR_non_gly_HbA1c %>%
  mutate(
    Lower_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_non_gly, "non_gly_pred")[1],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_non_gly, "non_gly_pred")[1],
      Infection == "UTI" ~ confint(UTI_non_gly, "non_gly_pred")[1],
      Infection == "Cystitis" ~ confint(cystitis_non_gly, "non_gly_pred")[1],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_non_gly, "non_gly_pred")[1],
      Infection == "Influenza" ~ confint(influenza_non_gly, "non_gly_pred")[1],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_non_gly, "non_gly_pred")[1],
      Infection == "Lower respiratory" ~ confint(lower_resp_non_gly, "non_gly_pred")[1],
      Infection == "Upper respiratory" ~ confint(upper_resp_non_gly, "non_gly_pred")[1],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_non_gly, "non_gly_pred")[1],
      Infection == "Genital infection" ~ confint(genitalinfection_non_gly, "non_gly_pred")[1]
    ),
    Upper_CI = case_when(
      Infection == "Pneumonia" ~ confint(pneumonia_non_gly, "non_gly_pred")[2],
      Infection == "Bacterial skin infection" ~ confint(bacterial_skininfection_non_gly, "non_gly_pred")[2],
      Infection == "UTI" ~ confint(UTI_non_gly, "non_gly_pred")[2],
      Infection == "Cystitis" ~ confint(cystitis_non_gly, "non_gly_pred")[2],
      Infection == "Pyelonephritis" ~ confint(pyelonephritis_non_gly, "non_gly_pred")[2],
      Infection == "Influenza" ~ confint(influenza_non_gly, "non_gly_pred")[2],
      Infection == "Other viral respiratory" ~ confint(otherrespiratoryinfection_non_gly, "non_gly_pred")[2],
      Infection == "Lower respiratory" ~ confint(lower_resp_non_gly, "non_gly_pred")[2],
      Infection == "Upper respiratory" ~ confint(upper_resp_non_gly, "non_gly_pred")[2],
      Infection == "Fungal skin infection" ~ confint(fungal_skininfection_non_gly, "non_gly_pred")[2],
      Infection == "Genital infection" ~ confint(genitalinfection_non_gly, "non_gly_pred")[2]
    ),
  )

# Add infection Ns
MR_non_gly_HbA1c <- MR_non_gly_HbA1c %>%
  mutate(case_when(
    Infection == "Pneumonia" ~ count(hba1c_non_gly %>% filter(pneumonia_GP ==1)),
    Infection == "Bacterial skin infection" ~ count(hba1c_non_gly %>% filter(bacterial_skininfection_GP ==1)),
    Infection == "UTI" ~ count(hba1c_non_gly %>% filter(UTI_GP ==1)),
    Infection == "Cystitis" ~  count(hba1c_non_gly %>% filter(cystitis_GP ==1)),
    Infection == "Pyelonephritis" ~  count(hba1c_non_gly %>% filter(pyelonephritis_GP ==1)),
    Infection == "Influenza" ~  count(hba1c_non_gly %>% filter(influenza_GP ==1)),
    Infection == "Other viral respiratory" ~  count(hba1c_non_gly %>% filter(otherrespiratoryinfection_GP ==1)),
    Infection == "Lower respiratory" ~  count(hba1c_non_gly %>% filter(lower_resp_GP ==1)),
    Infection == "Upper respiratory" ~  count(hba1c_non_gly %>% filter(upper_resp_GP ==1)),
    Infection == "Fungal skin infection" ~  count(hba1c_non_gly %>% filter(fungal_skininfection_GP ==1)),
    Infection == "Genital infection" ~  count(hba1c_non_gly %>% filter(genitalinfection_GP ==1))
  )
  )

# Set p <0.001
MR_non_gly_HbA1c <- MR_non_gly_HbA1c %>% dplyr::select(Infection, N_cases = n, Coef, Lower_CI, Upper_CI, P_value) %>%
  mutate(P_value = ifelse(P_value <0.001, "<0.001", P_value), OR = exp(Coef), OR_lower = exp(Lower_CI), OR_upper = exp(Upper_CI)) %>%
  mutate(OR_10 = OR^10, OR_lower_10 = OR_lower^10, OR_upper_10 = OR_upper^10)

# Save table as CSV
write.csv(MR_non_gly_HbA1c, file = paste0("saveables/", "MR_non_gly_HbA1c_results_primary_care_by_type.csv"), row.names=FALSE)

