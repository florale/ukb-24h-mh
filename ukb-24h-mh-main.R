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
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_phq_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_phq_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## all stratified by insomnia ------------
m_phq_2023_gam_goodsleep_bl <- brmcoda(clr_acc_mhq_goodsleep_bl,
                                    phq_2023 ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                      icd_any_at_acc,
                                    family = zero_inflated_negbinomial(),
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_goodsleep_bl, paste0(outputdir, "m_phq_2023_gam_goodsleep_bl", ".RDS"))

m_phq_2023_gam_insomnia_mild_bl <- brmcoda(clr_acc_mhq_insomnia_mild_bl,
                                        phq_2023 ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                          icd_any_at_acc,
                                        family = zero_inflated_negbinomial(),
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_insomnia_mild_bl, paste0(outputdir, "m_phq_2023_gam_insomnia_mild_bl", ".RDS"))

m_phq_2023_gam_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_insomnia_persistent_bl,
                                              phq_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_insomnia_persistent_bl, paste0(outputdir, "m_phq_2023_gam_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_goodsleep_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_goodsleep_bl", ".RDS"))
m_phq_2023_gam_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_insomnia_mild_bl", ".RDS"))
m_phq_2023_gam_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_sub_goodsleep_bl_99ci <- substitution(
  m_phq_2023_gam_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_goodsleep_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_insomnia_mild_bl_99ci <- substitution(
  m_phq_2023_gam_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_insomnia_mild_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_insomnia_persistent_bl_99ci <- substitution(
  m_phq_2023_gam_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_insomnia_persistent_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - good sleep 2023 -----------------------
m_phq_2023_gam_sleep_q1_goodsleep_2023 <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_2023,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_goodsleep_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_2023", ".RDS"))

m_phq_2023_gam_sleep_q2_goodsleep_2023 <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_2023,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_goodsleep_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_2023", ".RDS"))

m_phq_2023_gam_sleep_q3_goodsleep_2023 <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_2023,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_goodsleep_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_2023", ".RDS"))

m_phq_2023_gam_sleep_q1_goodsleep_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_2023", ".RDS"))
m_phq_2023_gam_sleep_q2_goodsleep_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_2023", ".RDS"))
m_phq_2023_gam_sleep_q3_goodsleep_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_2023", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci", ".RDS"))

## quantiles - mild insomnia 2023 -----------------------
m_phq_2023_gam_sleep_q1_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_2023,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_mild_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_2023", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_2023,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_mild_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_2023", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_2023,
                                                 phq_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_mild_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_2023", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_2023", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_2023", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_2023", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci", ".RDS"))

## quantiles - persistent insomnia 2023 -----------------------
m_phq_2023_gam_sleep_q1_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_2023,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_persistent_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_2023", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_2023,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_persistent_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_2023", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_2023,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_persistent_2023, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_2023", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_2023", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_2023", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_2023", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci", ".RDS"))


## quantiles - good sleep 2016 -----------------------
m_phq_2023_gam_sleep_q1_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_2016,
                                                  phq_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_goodsleep_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_2016", ".RDS"))

m_phq_2023_gam_sleep_q2_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_2016,
                                                  phq_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_goodsleep_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_2016", ".RDS"))

m_phq_2023_gam_sleep_q3_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_2016,
                                                  phq_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_goodsleep_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_2016", ".RDS"))

m_phq_2023_gam_sleep_q1_goodsleep_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_2016", ".RDS"))
m_phq_2023_gam_sleep_q2_goodsleep_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_2016", ".RDS"))
m_phq_2023_gam_sleep_q3_goodsleep_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_2016", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci", ".RDS"))

## quantiles - mild insomnia 2016 -----------------------
m_phq_2023_gam_sleep_q1_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_2016,
                                                      phq_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_mild_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_2016", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_2016,
                                                      phq_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_mild_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_2016", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_2016,
                                                      phq_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_mild_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_2016", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_2016", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_2016", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_2016", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci", ".RDS"))

## quantiles - persistent insomnia 2016 -----------------------
m_phq_2023_gam_sleep_q1_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_2016,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_persistent_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_2016", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_2016,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_persistent_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_2016", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_2016,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_persistent_2016, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_2016", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_2016", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_2016", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_2016", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci", ".RDS"))

## quantiles - good sleep baseline -----------------------
m_phq_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                phq_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_phq_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                phq_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_phq_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                phq_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_phq_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_phq_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_phq_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - mild insomnia baseline -----------------------
m_phq_2023_gam_sleep_q1_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_mild_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_mild_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_bl,
                                                    phq_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_mild_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

## quantiles - persistent insomnia baseline -----------------------
m_phq_2023_gam_sleep_q1_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_bl,
                                                          phq_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_insomnia_persistent_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_sleep_q2_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_bl,
                                                          phq_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_insomnia_persistent_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_sleep_q3_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_bl,
                                                          phq_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_insomnia_persistent_bl, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_sleep_q1_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_phq_2023_gam_sub_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_phq_2023_gam_sub_goodsleep_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_insomnia_mild_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_insomnia_persistent_bl_99ci, delta = 20)

# 2023 insomnia
summary(m_phq_2023_gam_sub_sleep_q1_goodsleep_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_goodsleep_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_goodsleep_2023_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci, delta = 20)

# 2016 insomnia
summary(m_phq_2023_gam_sub_sleep_q1_goodsleep_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_goodsleep_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_goodsleep_2016_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci, delta = 20)

# baseline insomnia
summary(m_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, delta = 20)

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
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_gad_2023_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_gad_2023_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## all stratified by insomnia ------------
m_gad_2023_gam_goodsleep_bl <- brmcoda(clr_acc_mhq_goodsleep_bl,
                                    gad_2023 ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                      icd_any_at_acc,
                                    family = zero_inflated_negbinomial(),
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_goodsleep_bl, paste0(outputdir, "m_gad_2023_gam_goodsleep_bl", ".RDS"))

m_gad_2023_gam_insomnia_mild_bl <- brmcoda(clr_acc_mhq_insomnia_mild_bl,
                                        gad_2023 ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                          icd_any_at_acc,
                                        family = zero_inflated_negbinomial(),
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_insomnia_mild_bl, paste0(outputdir, "m_gad_2023_gam_insomnia_mild_bl", ".RDS"))

m_gad_2023_gam_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_insomnia_persistent_bl,
                                              gad_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_insomnia_persistent_bl, paste0(outputdir, "m_gad_2023_gam_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_goodsleep_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_goodsleep_bl", ".RDS"))
m_gad_2023_gam_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_insomnia_mild_bl", ".RDS"))
m_gad_2023_gam_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_sub_goodsleep_bl_99ci <- substitution(
  m_gad_2023_gam_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_goodsleep_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_insomnia_mild_bl_99ci <- substitution(
  m_gad_2023_gam_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_insomnia_mild_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_insomnia_persistent_bl_99ci <- substitution(
  m_gad_2023_gam_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_insomnia_persistent_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

## quantiles - good sleep 2023 -----------------------
m_gad_2023_gam_sleep_q1_goodsleep_2023 <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_2023,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_goodsleep_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_2023", ".RDS"))

m_gad_2023_gam_sleep_q2_goodsleep_2023  <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_2023,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_goodsleep_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_2023", ".RDS"))

m_gad_2023_gam_sleep_q3_goodsleep_2023 <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_2023 ,
                                             gad_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_goodsleep_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_2023", ".RDS"))

m_gad_2023_gam_sleep_q1_goodsleep_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_2023", ".RDS"))
m_gad_2023_gam_sleep_q2_goodsleep_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_2023", ".RDS"))
m_gad_2023_gam_sleep_q3_goodsleep_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_2023", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_goodsleep_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci", ".RDS"))

## quantiles - mild insomnia 2023 -----------------------
m_gad_2023_gam_sleep_q1_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_2023,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_mild_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_2023", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_2023,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_mild_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_2023", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_mild_2023 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_2023,
                                                 gad_2023 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_mild_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_2023", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_2023", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_2023", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_mild_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_2023", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_mild_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci", ".RDS"))

## quantiles - persistent insomnia 2023-----------------------
m_gad_2023_gam_sleep_q1_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_2023,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_persistent_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_2023", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_2023,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_persistent_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_2023", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_persistent_2023 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_2023,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_persistent_2023, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_2023", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_2023", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_2023", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_persistent_2023 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_2023", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_persistent_2023,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci", ".RDS"))

## quantiles - good sleep 2016 -----------------------
m_gad_2023_gam_sleep_q1_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_2016,
                                                  gad_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_goodsleep_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_2016", ".RDS"))

m_gad_2023_gam_sleep_q2_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_2016,
                                                  gad_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_goodsleep_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_2016", ".RDS"))

m_gad_2023_gam_sleep_q3_goodsleep_2016 <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_2016,
                                                  gad_2023 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_goodsleep_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_2016", ".RDS"))

m_gad_2023_gam_sleep_q1_goodsleep_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_2016", ".RDS"))
m_gad_2023_gam_sleep_q2_goodsleep_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_2016", ".RDS"))
m_gad_2023_gam_sleep_q3_goodsleep_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_2016", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_goodsleep_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci", ".RDS"))

## quantiles - mild insomnia 2016 -----------------------
m_gad_2023_gam_sleep_q1_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_2016,
                                                      gad_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_mild_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_2016", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_2016,
                                                      gad_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_mild_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_2016", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_mild_2016 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_2016,
                                                      gad_2023 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_mild_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_2016", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_2016", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_2016", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_mild_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_2016", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_mild_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci", ".RDS"))

## quantiles - persistent insomnia 2016 -----------------------
m_gad_2023_gam_sleep_q1_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_2016,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_persistent_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_2016", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_2016,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_persistent_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_2016", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_persistent_2016 <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_2016,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                              icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_persistent_2016, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_2016", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_2016", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_2016", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_persistent_2016 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_2016", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_persistent_2016,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci", ".RDS"))


## quantiles - good sleep baseline -----------------------
m_gad_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                gad_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_gad_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                gad_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_gad_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                gad_2023 ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                  icd_any_at_acc,
                                                family = zero_inflated_negbinomial(),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_gad_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_gad_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_gad_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - mild insomnia baseline -----------------------
m_gad_2023_gam_sleep_q1_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_mild_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_mild_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_mild_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild_bl,
                                                    gad_2023 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_mild_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_mild_bl", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_mild_bl", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_mild_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_mild_bl", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_mild_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

## quantiles - persistent insomnia baseline -----------------------
m_gad_2023_gam_sleep_q1_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent_bl,
                                                          gad_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_insomnia_persistent_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_sleep_q2_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent_bl,
                                                          gad_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_insomnia_persistent_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_sleep_q3_insomnia_persistent_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent_bl,
                                                          gad_2023 ~ 
                                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                                            s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                            icd_any_at_acc,
                                                          family = zero_inflated_negbinomial(),
                                                          chains = 4, cores = 4,
                                                          backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_insomnia_persistent_bl, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_sleep_q1_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_persistent_bl", ".RDS"))
m_gad_2023_gam_sleep_q2_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_persistent_bl", ".RDS"))
m_gad_2023_gam_sleep_q3_insomnia_persistent_bl <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_persistent_bl", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_insomnia_persistent_bl,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.995
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_gad_2023_gam_sub_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_goodsleep_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_insomnia_mild_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_insomnia_persistent_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_99ci, delta = 20)

summary(m_gad_2023_gam_sub_goodsleep_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_insomnia_mild_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_insomnia_persistent_bl_99ci, delta = 20)

# 2023 insomnia
summary(m_gad_2023_gam_sub_sleep_q1_goodsleep_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_goodsleep_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_goodsleep_2023_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2023_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2023_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2023_99ci, delta = 20)

# 2016 insomnia
summary(m_gad_2023_gam_sub_sleep_q1_goodsleep_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_goodsleep_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_goodsleep_2016_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_2016_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_2016_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_2016_99ci, delta = 20)

# baseline insomnia
summary(m_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci, delta = 20)
