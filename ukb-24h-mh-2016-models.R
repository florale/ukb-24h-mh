source("ukb-24h-mh-utils.R")
source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## stratified ------------
m_phq_2016_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                   phq_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1, paste0(outputdir, "m_phq_2016_gam_sleep_q1", ".RDS"))

m_phq_2016_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                   phq_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2, paste0(outputdir, "m_phq_2016_gam_sleep_q2", ".RDS"))

m_phq_2016_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                   phq_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3, paste0(outputdir, "m_phq_2016_gam_sleep_q3", ".RDS"))

m_phq_2016_gam_sub_sleep_q1 <- substitution(
  m_phq_2016_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1", ".RDS"))

m_phq_2016_gam_sub_sleep_q2 <- substitution(
  m_phq_2016_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2", ".RDS"))

m_phq_2016_gam_sub_sleep_q3 <- substitution(
  m_phq_2016_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3", ".RDS"))

## quantile - good sleep -----------------------
m_phq_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_phq_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_phq_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq_2016_gam_sleep_q3_goodsleep", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq_2016_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq_2016_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq_2016_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq_2016_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq_2016_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq_2016_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                                 phq_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                 phq_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                 phq_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq_2016_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq_2016_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq_2016_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq_2016_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq_2016_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - moderate insomnia -----------------------
m_phq_2016_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                     phq_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                     phq_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                     phq_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_phq_2016_gam_sleep_q1_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_phq_2016_gam_sleep_q2_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_phq_2016_gam_sleep_q3_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

## quantile - severe insomnia -----------------------
m_phq_2016_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                                   phq_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                   phq_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                   phq_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sleep_q3_insomnia_severe", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_phq_2016_gam_sleep_q1_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_phq_2016_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_phq_2016_gam_sleep_q2_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_phq_2016_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_phq_2016_gam_sleep_q3_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq_2016_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                       phq_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                       phq_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                       phq_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq_2016_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq_2016_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq_2016_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4
)
saveRDS(m_phq_2016_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

## phq9 summary --------------------
m_phq_2016_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1", ".RDS"))
m_phq_2016_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2", ".RDS"))
m_phq_2016_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_2016_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_2016_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_2016_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_2016_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_phq_2016_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_phq_2016_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_phq_2016_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_phq_2016_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_phq_2016_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_2016_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_2016_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

summary(m_phq_2016_gam_sub_sleep_q1, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3, delta = 20)

summary(m_phq_2016_gam_sub_sleep_q1_goodsleep, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2_goodsleep, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3_goodsleep, delta = 20)

summary(m_phq_2016_gam_sub_sleep_q1_insomnia_mild, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2_insomnia_mild, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3_insomnia_mild, delta = 20)

summary(m_phq_2016_gam_sub_sleep_q1_insomnia_moderate, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2_insomnia_moderate, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3_insomnia_moderate, delta = 20)

summary(m_phq_2016_gam_sub_sleep_q1_insomnia_severe, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2_insomnia_severe, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3_insomnia_severe, delta = 20)

summary(m_phq_2016_gam_sub_sleep_q1_insomnia_persistent, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q2_insomnia_persistent, delta = 20)
summary(m_phq_2016_gam_sub_sleep_q3_insomnia_persistent, delta = 20)

# GAD7 -------------------
## stratified ------------
m_gad_2016_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                   gad_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1, paste0(outputdir, "m_gad_2016_gam_sleep_q1", ".RDS"))

m_gad_2016_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                   gad_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2, paste0(outputdir, "m_gad_2016_gam_sleep_q2", ".RDS"))

m_gad_2016_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                   gad_2016 ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                     icd_any_at_acc,
                                   family = zero_inflated_negbinomial(),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3, paste0(outputdir, "m_gad_2016_gam_sleep_q3", ".RDS"))

m_gad_2016_gam_sub_sleep_q1 <- substitution(
  m_gad_2016_gam_sleep_q1,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1", ".RDS"))

m_gad_2016_gam_sub_sleep_q2 <- substitution(
  m_gad_2016_gam_sleep_q2,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2", ".RDS"))

m_gad_2016_gam_sub_sleep_q3 <- substitution(
  m_gad_2016_gam_sleep_q3,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3", ".RDS"))

## quantile - good sleep -----------------------
m_gad_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                             gad_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_gad_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_gad_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                             gad_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_gad_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_gad_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                             gad_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                               icd_any_at_acc,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3_goodsleep, paste0(outputdir, "m_gad_2016_gam_sleep_q3_goodsleep", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_goodsleep <- substitution(
  m_gad_2016_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_gad_2016_gam_sub_sleep_q2_goodsleep <- substitution(
  m_gad_2016_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_gad_2016_gam_sub_sleep_q3_goodsleep <- substitution(
  m_gad_2016_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_gad_2016_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                                 gad_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_gad_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                 gad_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_gad_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                 gad_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                   icd_any_at_acc,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sleep_q3_insomnia_mild", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_gad_2016_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_gad_2016_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_gad_2016_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_gad_2016_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_gad_2016_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - moderate insomnia -----------------------
m_gad_2016_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                     gad_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                     gad_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                     gad_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                       icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_gad_2016_gam_sleep_q1_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_gad_2016_gam_sleep_q2_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_gad_2016_gam_sleep_q3_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

## quantile - severe insomnia -----------------------
m_gad_2016_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                                   gad_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_gad_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                   gad_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_gad_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                   gad_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                     icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sleep_q3_insomnia_severe", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_gad_2016_gam_sleep_q1_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_gad_2016_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_gad_2016_gam_sleep_q2_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_gad_2016_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_gad_2016_gam_sleep_q3_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
m_gad_2016_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                       gad_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_gad_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                       gad_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_gad_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                       gad_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2016_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_gad_2016_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_gad_2016_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_gad_2016_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_gad_2016_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_gad_2016_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4
)
saveRDS(m_gad_2016_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

## gad summary --------------------
m_gad_2016_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1", ".RDS"))
m_gad_2016_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2", ".RDS"))
m_gad_2016_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_gad_2016_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_gad_2016_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_gad_2016_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_gad_2016_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_gad_2016_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_gad_2016_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_gad_2016_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_gad_2016_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_gad_2016_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_gad_2016_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_gad_2016_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

summary(m_gad_2016_gam_sub_sleep_q1, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3, delta = 20)

summary(m_gad_2016_gam_sub_sleep_q1_goodsleep, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2_goodsleep, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3_goodsleep, delta = 20)

summary(m_gad_2016_gam_sub_sleep_q1_insomnia_mild, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2_insomnia_mild, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3_insomnia_mild, delta = 20)

summary(m_gad_2016_gam_sub_sleep_q1_insomnia_moderate, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2_insomnia_moderate, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3_insomnia_moderate, delta = 20)

summary(m_gad_2016_gam_sub_sleep_q1_insomnia_severe, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2_insomnia_severe, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3_insomnia_severe, delta = 20)

summary(m_gad_2016_gam_sub_sleep_q1_insomnia_persistent, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q2_insomnia_persistent, delta = 20)
summary(m_gad_2016_gam_sub_sleep_q3_insomnia_persistent, delta = 20)

# phq8 -------------------
## quantile - good sleep -----------------------
m_phq8_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_phq8_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_phq8_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_q3_goodsleep", ".RDS"))

m_phq8_2016_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq8_2016_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq8_2016_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq8_2016_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq8_2016_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq8_2016_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq8_2016_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                                  phq8_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                  phq8_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                  phq8_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                    icd_any_at_acc,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq8_2016_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq8_2016_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq8_2016_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - moderate insomnia -----------------------
m_phq8_2016_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                      phq8_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                      phq8_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                      phq8_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                        icd_any_at_acc,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_phq8_2016_gam_sleep_q1_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_phq8_2016_gam_sleep_q2_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_phq8_2016_gam_sleep_q3_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

## quantile - severe insomnia -----------------------
m_phq8_2016_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                                    phq8_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                    phq8_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                    phq8_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                      icd_any_at_acc,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sleep_q3_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_phq8_2016_gam_sleep_q1_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_phq8_2016_gam_sleep_q2_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_phq8_2016_gam_sleep_q3_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq8_2016_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                        phq8_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                        phq8_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                        phq8_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq8_2016_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq8_2016_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq8_2016_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4
)
saveRDS(m_phq8_2016_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

## phq8 summary --------------------
summary(m_phq8_2016_gam_sub_sleep_q1_goodsleep, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q2_goodsleep, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q3_goodsleep, delta = 20)

summary(m_phq8_2016_gam_sub_sleep_q1_insomnia_mild, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q2_insomnia_mild, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q3_insomnia_mild, delta = 20)

summary(m_phq8_2016_gam_sub_sleep_q1_insomnia_moderate, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q2_insomnia_moderate, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q3_insomnia_moderate, delta = 20)

summary(m_phq8_2016_gam_sub_sleep_q1_insomnia_severe, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q2_insomnia_severe, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q3_insomnia_severe, delta = 20)

summary(m_phq8_2016_gam_sub_sleep_q1_insomnia_persistent, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q2_insomnia_persistent, delta = 20)
summary(m_phq8_2016_gam_sub_sleep_q3_insomnia_persistent, delta = 20)

