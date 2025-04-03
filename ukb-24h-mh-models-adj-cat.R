source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_adj_phq_cat_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                           phq_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q1, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                           phq_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q2, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                           phq_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q3, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q1,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q2,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q3,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## quantiles - good sleep baseline -----------------------
m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                        phq_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                        phq_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                        phq_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - insomnia baseline -----------------------
m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                       phq_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                       phq_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                       phq_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## summary all -------------------
m_adj_phq_cat_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

summary(m_adj_phq_cat_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q3_99ci, delta = 20)

# baseline insomnia
summary(m_adj_phq_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

summary(m_adj_phq_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20)
summary(m_adj_phq_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20)

# GAD7
## all stratified by sleep quantiles ------------
m_adj_gad_cat_2023_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                           gad_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q1, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                           gad_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q2, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                           gad_2023_binary ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                             icd_any_at_acc,
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q3, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q1,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q2,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q3,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci", ".RDS"))

## quantiles - good sleep baseline -----------------------
m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                        gad_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                        gad_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                        gad_2023_binary ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                          icd_any_at_acc,
                                                        family = bernoulli(link = "logit"),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - insomnia baseline -----------------------
m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                       gad_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                       gad_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                       gad_2023_binary ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation) +
                                                         icd_any_at_acc,
                                                       family = bernoulli(link = "logit"),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl,
  delta = 20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.999,
  summary = FALSE
)
saveRDS(m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## summary all -------------------
m_adj_gad_cat_2023_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

summary(m_adj_gad_cat_2023_gam_sub_sleep_q1_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q2_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q3_99ci, delta = 20)

# baseline insomnia
summary(m_adj_gad_cat_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 20)

summary(m_adj_gad_cat_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 20)
summary(m_adj_gad_cat_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 20)

# GAD7
