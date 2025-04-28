
##Setup

library(tidyverse)
library(forestplot)
library(patchwork)

################################################################################
###Hospitalisation

#HbA1c observational
HbA1c_obs_hosp <- read_csv("Obs_HbA1c_results_by_type.csv")
#HbA1c glycaemic 1SMR
HbA1c_gly_MR_hosp <- read_csv("MR_gly_HbA1c_results_by_type.csv")
#HbA1c non-glycaemic 1SMR
HbA1c_non_gly_MR_hosp <- read_csv("MR_non_gly_HbA1c_results_by_type.csv")

#Infections for forest plots
row_names <- data.frame(Infection = c("Skin Infections", "Bacterial skin infection", "Fungal skin infection", "Respiratory infections", "Pneumonia", "Influenza", "Other viral respiratory",
                                      "Lower respiratory", "Upper respiratory", "Urogenital infections", "UTI", "Cystitis", "Pyelonephritis", "Genital infection"))

#Plot ORs
hosp_results_10 <- HbA1c_obs_hosp %>% select(Infection, Obs_OR = OR_10, Obs_lower = OR_lower_10, Obs_upper = OR_upper_10) %>%
  left_join(HbA1c_gly_MR_hosp %>% select(Infection, MR_g_OR = OR_10, MR_g_lower = OR_lower_10, MR_g_upper = OR_upper_10)) %>%
  left_join(HbA1c_non_gly_MR_hosp %>% select(Infection, MR_ng_OR = OR_10, MR_ng_lower = OR_lower_10, MR_ng_upper = OR_upper_10))

hosp_results_10 <- row_names %>% left_join(hosp_results_10) %>%
  mutate(Infection = ifelse(Infection == "Other viral respiratory", "Non-influenza viral respiratory tract infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "UTI", "Bacterial urinary tract infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Pneumonia", "Bacterial pneumonia", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Genital infection", "Fungal genital infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Lower respiratory", "   Lower respiratory tract infection*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Upper respiratory", "   Upper respiratory tract infection*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Cystitis", "   Cystitis*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Pyelonephritis", "   Pyelonephritis*", Infection))

#Make forest plot
tick <- c(0.25, 0.5, 1, 2, 5)
fp1 <- hosp_results_10 %>% forestplot(labeltext = Infection,
                                 mean = c(Obs_OR, MR_g_OR, MR_ng_OR),
                                 lower = c(Obs_lower, MR_g_lower, MR_ng_lower),
                                 upper = c(Obs_upper, MR_g_upper, MR_ng_upper),
                                 hrzl_lines = gpar(col="#444444"),
                                 col=fpColors(box=c("#7570B3","#E7298A", "#66A61E"), lines=c("#7570B3","#E7298A", "#66A61E"), zero = "gray50"),
                                 boxsize = .125,
                                 graphwidth = unit(12, 'cm'),
                                 clip = c(0.25,5),
                                 zero = 1,
                                 xlog = TRUE,
                                 ci.vertices = FALSE, #makes ends T
                                 new_page = TRUE,
                                 fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI, fpDrawDiamondCI),
                                 lty.ci =1,
                                 txt_gp = fpTxtGp(label = gpar(cex = 1), ticks  = gpar(cex = 1), xlab = gpar(cex = 1), legend = gpar(cex = 1)),
                                 xlab = "Odds Ratio per 10 mmol/mol HbA1c increase",
                                 is.summary = c(TRUE, rep(FALSE, 2), TRUE, rep(FALSE,5), TRUE, rep(FALSE,4)),
                                 legend = c("Observational", "One sample MR Glycaemic", "One sample MR Non-glycaemic"),
                                 legend_args = fpLegend(pos = "top")
)
fp1

#Save to pdf
pdf.options(reset = TRUE, onefile = TRUE)
pdf("HbA1c_infection_hospitalisation_forest_plot.pdf",width=10,height=10)
fp1
dev.off()

################################################################################
###Primary care

#HbA1c observational
HbA1c_obs_GP <- read_csv("Obs_HbA1c_results_primary_care_by_type.csv")
#HbA1c glycaemic 1SMR
HbA1c_gly_MR_GP <- read_csv("MR_gly_HbA1c_results_primary_care_by_type.csv")
#HbA1c non-glycaemic 1SMR
HbA1c_non_gly_MR_GP <- read_csv("MR_non_gly_HbA1c_results_primary_care_by_type.csv")

#Plot ORs
GP_results_10 <- HbA1c_obs_GP %>% select(Infection, Obs_OR = OR_10, Obs_lower = OR_lower_10, Obs_upper = OR_upper_10) %>%
  left_join(HbA1c_gly_MR_GP %>% select(Infection, MR_g_OR = OR_10, MR_g_lower = OR_lower_10, MR_g_upper = OR_upper_10)) %>%
  left_join(HbA1c_non_gly_MR_GP %>% select(Infection, MR_ng_OR = OR_10, MR_ng_lower = OR_lower_10, MR_ng_upper = OR_upper_10))

GP_results_10 <- row_names %>% left_join(GP_results_10) %>%
  mutate(Infection = ifelse(Infection == "Other viral respiratory", "Non-influenza viral respiratory tract infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "UTI", "Bacterial urinary tract infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Pneumonia", "Bacterial pneumonia", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Genital infection", "Fungal genital infection", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Lower respiratory", "   Lower respiratory tract infection*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Upper respiratory", "   Upper respiratory tract infection*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Cystitis", "   Cystitis*", Infection)) %>%
  mutate(Infection = ifelse(Infection == "Pyelonephritis", "   Pyelonephritis*", Infection))

#Make forest plot
tick <- c(0.25, 0.5, 1, 2, 5)
fp2 <- GP_results_10 %>% forestplot(labeltext = Infection,
                                 mean = c(Obs_OR, MR_g_OR, MR_ng_OR),
                                 lower = c(Obs_lower, MR_g_lower, MR_ng_lower),
                                 upper = c(Obs_upper, MR_g_upper, MR_ng_upper),
                                 hrzl_lines = gpar(col="#444444"),
                                 col=fpColors(box=c("#7570B3","#E7298A", "#66A61E"), lines=c("#7570B3","#E7298A", "#66A61E"), zero = "gray50"),
                                 boxsize = .125,
                                 graphwidth = unit(12, 'cm'),
                                 clip = c(0.5,5),
                                 zero = 1,
                                 xlog = TRUE,
                                 ci.vertices = FALSE, #makes ends T
                                 new_page = TRUE,
                                 fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI, fpDrawDiamondCI),
                                 lty.ci =1,
                                 txt_gp = fpTxtGp(label = gpar(cex = 1), ticks  = gpar(cex = 1), xlab = gpar(cex = 1), legend = gpar(cex = 1)),
                                 xlab = "Odds Ratio per 10 mmol/mol HbA1c increase",
                                 is.summary = c(TRUE, rep(FALSE, 2), TRUE, rep(FALSE,5), TRUE, rep(FALSE,4)),
                                 legend = c("Observational", "One sample MR Glycaemic", "One sample MR Non-glycaemic"),
                                 legend_args = fpLegend(pos = "top")
)
fp2

#Save to pdf
pdf.options(reset = TRUE, onefile = TRUE)
pdf("HbA1c_infection_primary_care_forest_plot.pdf",width=10,height=10)
fp2
dev.off()
