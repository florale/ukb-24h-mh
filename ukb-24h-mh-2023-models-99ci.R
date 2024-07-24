source("ukb-24h-mh-utils.R")
source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## stratified by sleep quantiles ------------
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

# ## stratified by insomnia ------------
# m_phq_2023_gam_insomnia_ <- readRDS(paste0(outputdir, "m_phq_2023_gam_insomnia_mild", ".RDS"))
# m_phq_2023_gam_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_insomnia_mild", ".RDS"))
# m_phq_2023_gam_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_insomnia_persistent", ".RDS"))
# 
# m_phq_2023_gam_sub_insomnia_mild_99ci <- substitution(
#   m_phq_2023_gam_insomnia_mild,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_insomnia_mild_99ci", ".RDS"))
# 
# m_phq_2023_gam_sub_insomnia_mild_99ci <- substitution(
#   m_phq_2023_gam_insomnia_mild,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_insomnia_mild_99ci", ".RDS"))
# 
# m_phq_2023_gam_sub_insomnia_persistent_99ci <- substitution(
#   m_phq_2023_gam_insomnia_persistent,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_insomnia_persistent_99ci", ".RDS"))

## quantile - good sleep -----------------------
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

## quantile - mild insomnia -----------------------
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

# ## quantile - moderate insomnia -----------------------
# m_phq_2023_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_moderate,
#                                                      phq_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_moderate,
#                                                      phq_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_moderate,
#                                                      phq_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q1_insomnia_moderate <- substitution(
#   m_phq_2023_gam_sleep_q1_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q2_insomnia_moderate <- substitution(
#   m_phq_2023_gam_sleep_q2_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q3_insomnia_moderate <- substitution(
#   m_phq_2023_gam_sleep_q3_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))
# 
# ## quantile - severe insomnia -----------------------
# m_phq_2023_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_severe,
#                                                    phq_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_severe", ".RDS"))
# 
# m_phq_2023_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_severe,
#                                                    phq_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_severe", ".RDS"))
# 
# m_phq_2023_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_severe,
#                                                    phq_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_phq_2023_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_severe", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q1_insomnia_severe <- substitution(
#   m_phq_2023_gam_sleep_q1_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q2_insomnia_severe <- substitution(
#   m_phq_2023_gam_sleep_q2_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q3_insomnia_severe <- substitution(
#   m_phq_2023_gam_sleep_q3_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_phq_2023_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4
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

# m_phq_2023_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
# m_phq_2023_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
# m_phq_2023_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))
# 
# m_phq_2023_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
# m_phq_2023_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
# m_phq_2023_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

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

# summary(m_phq_2023_gam_sub_sleep_q1_insomnia_moderate_99ci, delta = 20)
# summary(m_phq_2023_gam_sub_sleep_q2_insomnia_moderate_99ci, delta = 20)
# summary(m_phq_2023_gam_sub_sleep_q3_insomnia_moderate_99ci, delta = 20)
# 
# summary(m_phq_2023_gam_sub_sleep_q1_insomnia_severe_99ci, delta = 20)
# summary(m_phq_2023_gam_sub_sleep_q2_insomnia_severe_99ci, delta = 20)
# summary(m_phq_2023_gam_sub_sleep_q3_insomnia_severe_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, delta = 20)

# GAD7 -------------------
## stratified ------------
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

## quantile - good sleep -----------------------
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

## quantile - mild insomnia -----------------------
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

# ## quantile - moderate insomnia -----------------------
# m_gad_2023_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_moderate,
#                                                      gad_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_moderate,
#                                                      gad_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_moderate,
#                                                      gad_2023 ~ 
#                                                        s(ilr1) + s(ilr2) + s(ilr3) +
#                                                        s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                        icd_any_at_acc,
#                                                      family = zero_inflated_negbinomial(),
#                                                      chains = 4, cores = 4,
#                                                      backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q1_insomnia_moderate <- substitution(
#   m_gad_2023_gam_sleep_q1_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q2_insomnia_moderate <- substitution(
#   m_gad_2023_gam_sleep_q2_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q3_insomnia_moderate <- substitution(
#   m_gad_2023_gam_sleep_q3_insomnia_moderate,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))
# 
# ## quantile - severe insomnia -----------------------
# m_gad_2023_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q1_insomnia_severe,
#                                                    gad_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sleep_q1_insomnia_severe", ".RDS"))
# 
# m_gad_2023_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q2_insomnia_severe,
#                                                    gad_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sleep_q2_insomnia_severe", ".RDS"))
# 
# m_gad_2023_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_2023_sleep_q3_insomnia_severe,
#                                                    gad_2023 ~ 
#                                                      s(ilr1) + s(ilr2) + s(ilr3) +
#                                                      s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
#                                                      icd_any_at_acc,
#                                                    family = zero_inflated_negbinomial(),
#                                                    chains = 4, cores = 4,
#                                                    backend = "cmdstanr"
# )
# saveRDS(m_gad_2023_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sleep_q3_insomnia_severe", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q1_insomnia_severe <- substitution(
#   m_gad_2023_gam_sleep_q1_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q2_insomnia_severe <- substitution(
#   m_gad_2023_gam_sleep_q2_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q3_insomnia_severe <- substitution(
#   m_gad_2023_gam_sleep_q3_insomnia_severe,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4,
#   ci = 0.99
# )
# saveRDS(m_gad_2023_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
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
# m_gad_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1", ".RDS"))
# m_gad_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2", ".RDS"))
# m_gad_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild_99ci", ".RDS"))

# m_gad_2023_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
# m_gad_2023_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
# m_gad_2023_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))
# 
# m_gad_2023_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
# m_gad_2023_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
# m_gad_2023_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

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

# summary(m_gad_2023_gam_sub_sleep_q1_insomnia_moderate, delta = 20)
# summary(m_gad_2023_gam_sub_sleep_q2_insomnia_moderate, delta = 20)
# summary(m_gad_2023_gam_sub_sleep_q3_insomnia_moderate, delta = 20)
# 
# summary(m_gad_2023_gam_sub_sleep_q1_insomnia_severe, delta = 20)
# summary(m_gad_2023_gam_sub_sleep_q2_insomnia_severe, delta = 20)
# summary(m_gad_2023_gam_sub_sleep_q3_insomnia_severe, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_insomnia_persistent_99ci, delta = 20)

