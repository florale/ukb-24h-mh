source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_phq_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                   phq_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1, paste0(outputdir, "m_phq_2023_gam_sleep_q1", ".RDS"))

m_phq_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                   phq_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2, paste0(outputdir, "m_phq_2023_gam_sleep_q2", ".RDS"))

m_phq_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                   phq_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3, paste0(outputdir, "m_phq_2023_gam_sleep_q3", ".RDS"))

m_phq_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1", ".RDS"))
m_phq_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2", ".RDS"))
m_phq_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_phq_2023_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_phq_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_phq_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_phq_2023_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_goodsleep,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_goodsleep,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_goodsleep,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep", ".RDS"))
m_phq_2023_gam_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep", ".RDS"))
m_phq_2023_gam_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_phq_2023_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_mild,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_mild,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_mild,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_persistent,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_persistent,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_persistent,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_phq_2023_gam_sub_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_phq_2023_gam_sub_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_insomnia_persistent_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_goodsleep_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, delta = 20)

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_gad_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                   gad_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1, paste0(outputdir, "m_gad_2023_gam_sleep_q1", ".RDS"))

m_gad_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                   gad_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2, paste0(outputdir, "m_gad_2023_gam_sleep_q2", ".RDS"))

m_gad_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                   gad_2023 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3, paste0(outputdir, "m_gad_2023_gam_sleep_q3", ".RDS"))

m_gad_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1", ".RDS"))
m_gad_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2", ".RDS"))
m_gad_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_gad_2023_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_gad_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_gad_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_gad_2023_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_goodsleep,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_goodsleep,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_goodsleep,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep", ".RDS"))
m_gad_2023_gam_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep", ".RDS"))
m_gad_2023_gam_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_gad_2023_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_mild,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_mild,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_mild,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_gad_2023_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_persistent,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_persistent,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_persistent,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, delta = 20)
