source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_adj_phq_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                       phq_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q1, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1", ".RDS"))

m_adj_phq_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                       phq_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q2, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2", ".RDS"))

m_adj_phq_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                       phq_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q3, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3", ".RDS"))

m_adj_phq_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1", ".RDS"))
m_adj_phq_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2", ".RDS"))
m_adj_phq_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## all stratified by insomnia ------------
m_adj_phq_2023_gam_goodsleep_bl <- brmcoda(clr_acc_mhq_goodsleep_bl,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_goodsleep_bl, paste0(outputdir, "m_adj_phq_2023_gam_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_insomnia_mild_bl <- brmcoda(clr_acc_mhq_insomnia_mild_bl,
                                               phq_2023 ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                 icd_any_at_acc,
                                               family = zero_inflated_negbinomial(),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_insomnia_mild_bl, paste0(outputdir, "m_adj_phq_2023_gam_insomnia_mild_bl", ".RDS"))

m_adj_phq_2023_gam_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_insomnia_persistent_bl,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_insomnia_persistent_bl, paste0(outputdir, "m_adj_phq_2023_gam_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_goodsleep_bl", ".RDS"))
m_adj_phq_2023_gam_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_insomnia_mild_bl", ".RDS"))
m_adj_phq_2023_gam_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_sub_goodsleep_bl_99ci <- substitution(
  m_adj_phq_2023_gam_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci <- substitution(
  m_adj_phq_2023_gam_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci <- substitution(
  m_adj_phq_2023_gam_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - good sleep baseline -----------------------
m_adj_phq_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - mild insomnia baseline -----------------------
m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_bl,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_bl,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_bl,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q1_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q2_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q3_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

## quantiles - persistent insomnia baseline -----------------------
m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_bl,
                                                              phq_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_bl,
                                                              phq_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_bl,
                                                              phq_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q1_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q2_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q3_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - insomnia (combined) baseline -----------------------
m_adj_phq_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_phq_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q1_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q2_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_adj_phq_2023_gam_sleep_q3_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))


## phq9 summary --------------------
m_adj_phq_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

summary(m_adj_phq_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_adj_phq_2023_gam_sub_goodsleep_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_insomnia_mild_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_insomnia_persistent_bl_99ci, delta = 20)

# baseline insomnia
summary(m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

# summary(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, delta = 20)
# summary(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, delta = 20)
# summary(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, delta = 20)
# 
# summary(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, delta = 20)
# summary(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, delta = 20)
# summary(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, delta = 20)

summary(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20)
summary(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20)

# standardised estimates
## by quantiles
d_adj_phq_2023_gam_sub_sleep_q1_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q1_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q2_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q2_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q3_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q3_99ci, delta = 20, digits = "asis"))

d_adj_phq_2023_gam_sub_sleep_q1_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q2_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q3_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3$data$phq_2023, na.rm = T), 2)]

## by quantiles + insomnia
d_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20, digits = "asis"))

d_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20, digits = "asis"))
d_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- as.data.table(summary(m_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20, digits = "asis"))

d_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_goodsleep_bl$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_goodsleep_bl$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_goodsleep_bl$data$phq_2023, na.rm = T), 2)]

d_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_insomnia_bl$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_insomnia_bl$data$phq_2023, na.rm = T), 2)]
d_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_insomnia_bl$data$phq_2023, na.rm = T), 2)]

d_adj_phq_2023_gam_sub_sleep_q1_99ci
d_adj_phq_2023_gam_sub_sleep_q2_99ci
d_adj_phq_2023_gam_sub_sleep_q3_99ci

d_adj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci
d_adj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci
d_adj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci

d_adj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci
d_adj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci
d_adj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_adj_gad_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                       gad_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q1, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1", ".RDS"))

m_adj_gad_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                       gad_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q2, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2", ".RDS"))

m_adj_gad_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                       gad_2023 ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                         icd_any_at_acc,
                                       family = zero_inflated_negbinomial(),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q3, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3", ".RDS"))

m_adj_gad_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1", ".RDS"))
m_adj_gad_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2", ".RDS"))
m_adj_gad_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## all stratified by insomnia ------------
m_adj_gad_2023_gam_goodsleep_bl <- brmcoda(clr_acc_mhq_goodsleep_bl,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_goodsleep_bl, paste0(outputdir, "m_adj_gad_2023_gam_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_insomnia_mild_bl <- brmcoda(clr_acc_mhq_insomnia_mild_bl,
                                               gad_2023 ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                 icd_any_at_acc,
                                               family = zero_inflated_negbinomial(),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_insomnia_mild_bl, paste0(outputdir, "m_adj_gad_2023_gam_insomnia_mild_bl", ".RDS"))

m_adj_gad_2023_gam_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_insomnia_persistent_bl,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_insomnia_persistent_bl, paste0(outputdir, "m_adj_gad_2023_gam_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_goodsleep_bl", ".RDS"))
m_adj_gad_2023_gam_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_insomnia_mild_bl", ".RDS"))
m_adj_gad_2023_gam_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_sub_goodsleep_bl_99ci <- substitution(
  m_adj_gad_2023_gam_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci <- substitution(
  m_adj_gad_2023_gam_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci <- substitution(
  m_adj_gad_2023_gam_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - good sleep baseline -----------------------
m_adj_gad_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - mild insomnia baseline -----------------------
m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_bl,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_bl,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_bl,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q1_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q2_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q3_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

## quantiles - persistent insomnia baseline -----------------------
m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_bl,
                                                              gad_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_bl,
                                                              gad_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_bl,
                                                              gad_2023 ~ 
                                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                                icd_any_at_acc,
                                                              family = zero_inflated_negbinomial(),
                                                              chains = 4, cores = 4,
                                                              backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q1_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q2_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q3_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - insomnia (combined) baseline -----------------------
m_adj_gad_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_adj_gad_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q1_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q2_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_adj_gad_2023_gam_sleep_q3_insomnia_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## gad summary --------------------
m_adj_gad_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

summary(m_adj_gad_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_adj_gad_2023_gam_sub_goodsleep_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_insomnia_mild_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_insomnia_persistent_bl_99ci, delta = 20)

# baseline insomnia
summary(m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

summary(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, delta = 20)

summary(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, delta  = 20)

summary(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20)
summary(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20)

# standardised estimates
## by quantiles
d_adj_gad_2023_gam_sub_sleep_q1_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q1_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q2_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q2_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q3_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q3_99ci, delta = 20, digits = "asis"))

d_adj_gad_2023_gam_sub_sleep_q1_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q2_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q3_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3$data$gad_2023, na.rm = T), 2)]

## by quantiles + insomnia
d_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20, digits = "asis"))

d_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20, digits = "asis"))
d_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- as.data.table(summary(m_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20, digits = "asis"))

d_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_goodsleep_bl$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_goodsleep_bl$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_goodsleep_bl$data$gad_2023, na.rm = T), 2)]

d_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_insomnia_bl$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_insomnia_bl$data$gad_2023, na.rm = T), 2)]
d_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_insomnia_bl$data$gad_2023, na.rm = T), 2)]

d_adj_gad_2023_gam_sub_sleep_q1_99ci
d_adj_gad_2023_gam_sub_sleep_q2_99ci
d_adj_gad_2023_gam_sub_sleep_q3_99ci

d_adj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci
d_adj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci
d_adj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci

d_adj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci
d_adj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci
d_adj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci

