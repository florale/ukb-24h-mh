source("ukb-24h-mh-data.R")

# in dep and anx incidences only -------------------
## ilr-------------------------
clr_acc_mhq_sleep_q1_dep_anx <- complr(data = d_acc_mhq_dep_anx[sleep <= quantile_sleep[[2]]],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q2_dep_anx <- complr(data = d_acc_mhq_dep_anx[sleep %gl% quantile_sleep[c(2:3)]],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q3_dep_anx <- complr(data = d_acc_mhq_dep_anx[sleep >= quantile_sleep[[3]]],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)

clr_acc_mhq_2023_sleep_q1_dep_anx_goodsleep <- complr(data = d_acc_mhq_dep_anx[sleep <= quantile_sleep[[2]] & insomnia_2023 == "good sleep"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_2023_sleep_q2_dep_anx_goodsleep <- complr(data = d_acc_mhq_dep_anx[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "good sleep"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_2023_sleep_q3_dep_anx_goodsleep <- complr(data = d_acc_mhq_dep_anx[sleep >= quantile_sleep[[3]] & insomnia_2023 == "good sleep"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)

clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_mild <- complr(data = d_acc_mhq_dep_anx[sleep <= quantile_sleep[[2]] & insomnia_2023 == "mild"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)
clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_mild <- complr(data = d_acc_mhq_dep_anx[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "mild"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)
clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_mild <- complr(data = d_acc_mhq_dep_anx[sleep >= quantile_sleep[[3]] & insomnia_2023 == "mild"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)

clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_persistent <- complr(data = d_acc_mhq_dep_anx[sleep <= quantile_sleep[[2]] & insomnia_2023 %in% c("moderate", "severe")],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_persistent <- complr(data = d_acc_mhq_dep_anx[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 %in% c("moderate", "severe")],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_persistent <- complr(data = d_acc_mhq_dep_anx[sleep >= quantile_sleep[[3]] & insomnia_2023 %in% c("moderate", "severe")],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_phq_2023_gam_sleep_q1_dep_anx <- brmcoda(clr_acc_mhq_sleep_q1_dep_anx,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_dep_anx, paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx", ".RDS"))

m_phq_2023_gam_sleep_q2_dep_anx <- brmcoda(clr_acc_mhq_sleep_q2_dep_anx,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_dep_anx, paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx", ".RDS"))

m_phq_2023_gam_sleep_q3_dep_anx <- brmcoda(clr_acc_mhq_sleep_q3_dep_anx,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_dep_anx, paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx", ".RDS"))

# m_phq_2023_gam_sleep_q1_dep_anx <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx", ".RDS"))
# m_phq_2023_gam_sleep_q2_dep_anx <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx", ".RDS"))
# m_phq_2023_gam_sleep_q3_dep_anx <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_phq_2023_gam_sleep_q1_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_dep_anx_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q2_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_dep_anx_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q3_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_dep_anx_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_goodsleep", ".RDS"))

# m_phq_2023_gam_sleep_q1_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q2_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q3_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild", ".RDS"))

# m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent", ".RDS"))

# m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci, delta = 20)

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_gad_2023_gam_sleep_q1_dep_anx <- brmcoda(clr_acc_mhq_sleep_q1_dep_anx,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_dep_anx, paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx", ".RDS"))

m_gad_2023_gam_sleep_q2_dep_anx <- brmcoda(clr_acc_mhq_sleep_q2_dep_anx,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_dep_anx, paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx", ".RDS"))

m_gad_2023_gam_sleep_q3_dep_anx <- brmcoda(clr_acc_mhq_sleep_q3_dep_anx,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                           # + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_dep_anx, paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx", ".RDS"))

# m_gad_2023_gam_sleep_q1_dep_anx <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx", ".RDS"))
# m_gad_2023_gam_sleep_q2_dep_anx <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx", ".RDS"))
# m_gad_2023_gam_sleep_q3_dep_anx <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_dep_anx,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_gad_2023_gam_sleep_q1_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_dep_anx_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q2_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_dep_anx_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q3_dep_anx_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                     # + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_dep_anx_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_goodsleep", ".RDS"))

# m_gad_2023_gam_sleep_q1_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q2_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q3_dep_anx_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_dep_anx_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                         # + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild", ".RDS"))

# m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_dep_anx_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_dep_anx_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_dep_anx_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_dep_anx_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation),
                                                               # + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent", ".RDS"))

# m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_dep_anx_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci, delta = 20)

# in bmi healthy -------------------
## ilr-------------------------
clr_acc_mhq_sleep_q1_healthy <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q2_healthy <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q3_healthy <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)

clr_acc_mhq_2023_sleep_q1_healthy_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "good sleep" & bmig == "undernormal"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_2023_sleep_q2_healthy_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "good sleep" & bmig == "undernormal"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_2023_sleep_q3_healthy_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "good sleep" & bmig == "undernormal"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      sbp = sbp,
                                                      total = 1440)

clr_acc_mhq_2023_sleep_q1_healthy_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "mild" & bmig == "undernormal"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)
clr_acc_mhq_2023_sleep_q2_healthy_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "mild" & bmig == "undernormal"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)
clr_acc_mhq_2023_sleep_q3_healthy_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "mild" & bmig == "undernormal"],
                                                          transform = "ilr",
                                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                          sbp = sbp,
                                                          total = 1440)

clr_acc_mhq_2023_sleep_q1_healthy_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "undernormal"],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
clr_acc_mhq_2023_sleep_q2_healthy_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 %in% c("moderate", "severe") & bmig == "undernormal"],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
clr_acc_mhq_2023_sleep_q3_healthy_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "undernormal"],
                                                                transform = "ilr",
                                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                sbp = sbp,
                                                                total = 1440)
# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_phq_2023_gam_sleep_q1_healthy <- brmcoda(clr_acc_mhq_sleep_q1_healthy,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_healthy, paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy", ".RDS"))

m_phq_2023_gam_sleep_q2_healthy <- brmcoda(clr_acc_mhq_sleep_q2_healthy,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_healthy, paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy", ".RDS"))

m_phq_2023_gam_sleep_q3_healthy <- brmcoda(clr_acc_mhq_sleep_q3_healthy,
                                           phq_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_healthy, paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy", ".RDS"))

m_phq_2023_gam_sleep_q1_healthy <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy", ".RDS"))
m_phq_2023_gam_sleep_q2_healthy <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy", ".RDS"))
m_phq_2023_gam_sleep_q3_healthy <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_healthy_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_healthy_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_healthy_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_healthy_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_healthy_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_phq_2023_gam_sleep_q1_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_healthy_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q2_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_healthy_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q3_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_goodsleep,
                                                     phq_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_healthy_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_goodsleep", ".RDS"))

# m_phq_2023_gam_sleep_q1_healthy_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q2_healthy_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q3_healthy_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_phq_2023_gam_sleep_q1_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_healthy_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q2_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_healthy_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q3_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_insomnia_mild,
                                                         phq_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_healthy_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_insomnia_mild", ".RDS"))

# m_phq_2023_gam_sleep_q1_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q2_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q3_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_insomnia_persistent,
                                                               phq_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent", ".RDS"))

# m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_healthy_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_healthy_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_healthy_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci, delta = 20)

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_gad_2023_gam_sleep_q1_healthy <- brmcoda(clr_acc_mhq_sleep_q1_healthy,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_healthy, paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy", ".RDS"))

m_gad_2023_gam_sleep_q2_healthy <- brmcoda(clr_acc_mhq_sleep_q2_healthy,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_healthy, paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy", ".RDS"))

m_gad_2023_gam_sleep_q3_healthy <- brmcoda(clr_acc_mhq_sleep_q3_healthy,
                                           gad_2023 ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                           family = zero_inflated_negbinomial(),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_healthy, paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy", ".RDS"))

# m_gad_2023_gam_sleep_q1_healthy <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy", ".RDS"))
# m_gad_2023_gam_sleep_q2_healthy <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy", ".RDS"))
# m_gad_2023_gam_sleep_q3_healthy <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_healthy_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_healthy_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_healthy_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_healthy_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_healthy,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_healthy_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_gad_2023_gam_sleep_q1_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_healthy_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q2_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_healthy_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q3_healthy_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_goodsleep,
                                                     gad_2023 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_healthy_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_goodsleep", ".RDS"))

# m_gad_2023_gam_sleep_q1_healthy_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q2_healthy_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q3_healthy_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_healthy_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_gad_2023_gam_sleep_q1_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_healthy_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q2_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_healthy_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q3_healthy_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_insomnia_mild,
                                                         gad_2023 ~ 
                                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                         family = zero_inflated_negbinomial(),
                                                         chains = 4, cores = 4,
                                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_healthy_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_insomnia_mild", ".RDS"))

# m_gad_2023_gam_sleep_q1_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q2_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q3_healthy_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_healthy_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_healthy_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_healthy_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_healthy_insomnia_persistent,
                                                               gad_2023 ~ 
                                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                                 s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                               family = zero_inflated_negbinomial(),
                                                               chains = 4, cores = 4,
                                                               backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent", ".RDS"))

# m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_healthy_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_healthy_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_healthy_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_healthy_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci, delta = 20)

# in bmi overweight -------------------
## ilr-------------------------
clr_acc_mhq_sleep_q1_overweight <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)
clr_acc_mhq_sleep_q2_overweight <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)
clr_acc_mhq_sleep_q3_overweight <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)

clr_acc_mhq_2023_sleep_q1_overweight_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "good sleep" & bmig == "over"],
                                                         transform = "ilr",
                                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                         sbp = sbp,
                                                         total = 1440)
clr_acc_mhq_2023_sleep_q2_overweight_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "good sleep" & bmig == "over"],
                                                         transform = "ilr",
                                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                         sbp = sbp,
                                                         total = 1440)
clr_acc_mhq_2023_sleep_q3_overweight_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "good sleep" & bmig == "over"],
                                                         transform = "ilr",
                                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                         sbp = sbp,
                                                         total = 1440)

clr_acc_mhq_2023_sleep_q1_overweight_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "mild" & bmig == "over"],
                                                             transform = "ilr",
                                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                             sbp = sbp,
                                                             total = 1440)
clr_acc_mhq_2023_sleep_q2_overweight_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "mild" & bmig == "over"],
                                                             transform = "ilr",
                                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                             sbp = sbp,
                                                             total = 1440)
clr_acc_mhq_2023_sleep_q3_overweight_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "mild" & bmig == "over"],
                                                             transform = "ilr",
                                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                             sbp = sbp,
                                                             total = 1440)

clr_acc_mhq_2023_sleep_q1_overweight_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "over"],
                                                                   transform = "ilr",
                                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                   sbp = sbp,
                                                                   total = 1440)
clr_acc_mhq_2023_sleep_q2_overweight_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 %in% c("moderate", "severe") & bmig == "over"],
                                                                   transform = "ilr",
                                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                   sbp = sbp,
                                                                   total = 1440)
clr_acc_mhq_2023_sleep_q3_overweight_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "over"],
                                                                   transform = "ilr",
                                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                                   sbp = sbp,
                                                                   total = 1440)
# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_phq_2023_gam_sleep_q1_overweight <- brmcoda(clr_acc_mhq_sleep_q1_overweight,
                                              phq_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                              + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_overweight, paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight", ".RDS"))

m_phq_2023_gam_sleep_q2_overweight <- brmcoda(clr_acc_mhq_sleep_q2_overweight,
                                              phq_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                              + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_overweight, paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight", ".RDS"))

m_phq_2023_gam_sleep_q3_overweight <- brmcoda(clr_acc_mhq_sleep_q3_overweight,
                                              phq_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                              + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_overweight, paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight", ".RDS"))

m_phq_2023_gam_sleep_q1_overweight <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight", ".RDS"))
m_phq_2023_gam_sleep_q2_overweight <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight", ".RDS"))
m_phq_2023_gam_sleep_q3_overweight <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_overweight_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_overweight_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_overweight_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_overweight_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_overweight_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_phq_2023_gam_sleep_q1_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_goodsleep,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                        + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_overweight_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q2_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_goodsleep,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                        + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_overweight_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q3_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_goodsleep,
                                                        phq_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                        + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_overweight_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_goodsleep", ".RDS"))

# m_phq_2023_gam_sleep_q1_overweight_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q2_overweight_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q3_overweight_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_phq_2023_gam_sleep_q1_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_insomnia_mild,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                            + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_overweight_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q2_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_insomnia_mild,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                            + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_overweight_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q3_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_insomnia_mild,
                                                            phq_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                            + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_overweight_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_insomnia_mild", ".RDS"))

# m_phq_2023_gam_sleep_q1_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q2_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q3_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_insomnia_persistent,
                                                                  phq_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                  + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_insomnia_persistent,
                                                                  phq_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                  + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_insomnia_persistent,
                                                                  phq_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                  + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent", ".RDS"))

# m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_overweight_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_overweight_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_overweight_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci, delta = 20)

summary(m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci, delta = 20)

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_gad_2023_gam_sleep_q1_overweight <- brmcoda(clr_acc_mhq_sleep_q1_overweight,
                                              gad_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_overweight, paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight", ".RDS"))

m_gad_2023_gam_sleep_q2_overweight <- brmcoda(clr_acc_mhq_sleep_q2_overweight,
                                              gad_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_overweight, paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight", ".RDS"))

m_gad_2023_gam_sleep_q3_overweight <- brmcoda(clr_acc_mhq_sleep_q3_overweight,
                                              gad_2023 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                + icd_any_at_acc,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_overweight, paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight", ".RDS"))

# m_gad_2023_gam_sleep_q1_overweight <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight", ".RDS"))
# m_gad_2023_gam_sleep_q2_overweight <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight", ".RDS"))
# m_gad_2023_gam_sleep_q3_overweight <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_overweight_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_overweight_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_overweight_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_overweight_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_overweight,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_overweight_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_gad_2023_gam_sleep_q1_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_goodsleep,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                          + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_overweight_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q2_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_goodsleep,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                          + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_overweight_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q3_overweight_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_goodsleep,
                                                        gad_2023 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                          + icd_any_at_acc,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_overweight_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_goodsleep", ".RDS"))

# m_gad_2023_gam_sleep_q1_overweight_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q2_overweight_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q3_overweight_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_overweight_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_gad_2023_gam_sleep_q1_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_insomnia_mild,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                              + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_overweight_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q2_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_insomnia_mild,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                              + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_overweight_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q3_overweight_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_insomnia_mild,
                                                            gad_2023 ~ 
                                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                                              s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                              + icd_any_at_acc,
                                                            family = zero_inflated_negbinomial(),
                                                            chains = 4, cores = 4,
                                                            backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_overweight_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_insomnia_mild", ".RDS"))

# m_gad_2023_gam_sleep_q1_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q2_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q3_overweight_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_overweight_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_overweight_insomnia_persistent,
                                                                  gad_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                    + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_overweight_insomnia_persistent,
                                                                  gad_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                    + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_overweight_insomnia_persistent,
                                                                  gad_2023 ~ 
                                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                                    s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                                    + icd_any_at_acc,
                                                                  family = zero_inflated_negbinomial(),
                                                                  chains = 4, cores = 4,
                                                                  backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent", ".RDS"))

# m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_overweight_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_overweight_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_overweight_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_overweight_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci, delta = 20)

summary(m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci, delta = 20)

# in bmi obese -------------------
## ilr-------------------------
clr_acc_mhq_sleep_q1_obese <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)
clr_acc_mhq_sleep_q2_obese <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)
clr_acc_mhq_sleep_q3_obese <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)

clr_acc_mhq_2023_sleep_q1_obese_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "good sleep" & bmig == "obese"],
                                                    transform = "ilr",
                                                    parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                    sbp = sbp,
                                                    total = 1440)
clr_acc_mhq_2023_sleep_q2_obese_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "good sleep" & bmig == "obese"],
                                                    transform = "ilr",
                                                    parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                    sbp = sbp,
                                                    total = 1440)
clr_acc_mhq_2023_sleep_q3_obese_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "good sleep" & bmig == "obese"],
                                                    transform = "ilr",
                                                    parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                    sbp = sbp,
                                                    total = 1440)

clr_acc_mhq_2023_sleep_q1_obese_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "mild" & bmig == "obese"],
                                                        transform = "ilr",
                                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                        sbp = sbp,
                                                        total = 1440)
clr_acc_mhq_2023_sleep_q2_obese_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "mild" & bmig == "obese"],
                                                        transform = "ilr",
                                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                        sbp = sbp,
                                                        total = 1440)
clr_acc_mhq_2023_sleep_q3_obese_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "mild" & bmig == "obese"],
                                                        transform = "ilr",
                                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                        sbp = sbp,
                                                        total = 1440)

clr_acc_mhq_2023_sleep_q1_obese_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "obese"],
                                                              transform = "ilr",
                                                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                              sbp = sbp,
                                                              total = 1440)
clr_acc_mhq_2023_sleep_q2_obese_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 %in% c("moderate", "severe") & bmig == "obese"],
                                                              transform = "ilr",
                                                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                              sbp = sbp,
                                                              total = 1440)
clr_acc_mhq_2023_sleep_q3_obese_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 %in% c("moderate", "severe") & bmig == "obese"],
                                                              transform = "ilr",
                                                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                              sbp = sbp,
                                                              total = 1440)
# PHQ9 -------------------
## all stratified by sleep quantiles ------------
m_phq_2023_gam_sleep_q1_obese <- brmcoda(clr_acc_mhq_sleep_q1_obese,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_obese, paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese", ".RDS"))

m_phq_2023_gam_sleep_q2_obese <- brmcoda(clr_acc_mhq_sleep_q2_obese,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_obese, paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese", ".RDS"))

m_phq_2023_gam_sleep_q3_obese <- brmcoda(clr_acc_mhq_sleep_q3_obese,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_obese, paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese", ".RDS"))

# m_phq_2023_gam_sleep_q1_obese <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese", ".RDS"))
# m_phq_2023_gam_sleep_q2_obese <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese", ".RDS"))
# m_phq_2023_gam_sleep_q3_obese <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_obese_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_obese_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_obese_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_obese_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_obese,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_obese_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_phq_2023_gam_sleep_q1_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_goodsleep,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_obese_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q2_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_goodsleep,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_obese_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_goodsleep", ".RDS"))

m_phq_2023_gam_sleep_q3_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_goodsleep,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_obese_goodsleep, paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_goodsleep", ".RDS"))

# m_phq_2023_gam_sleep_q1_obese_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q2_obese_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_goodsleep", ".RDS"))
# m_phq_2023_gam_sleep_q3_obese_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_obese_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_obese_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_obese_goodsleep,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_phq_2023_gam_sleep_q1_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_insomnia_mild,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_obese_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q2_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_insomnia_mild,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_obese_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_insomnia_mild", ".RDS"))

m_phq_2023_gam_sleep_q3_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_insomnia_mild,
                                                       phq_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_obese_insomnia_mild, paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_insomnia_mild", ".RDS"))

# m_phq_2023_gam_sleep_q1_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q2_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_insomnia_mild", ".RDS"))
# m_phq_2023_gam_sleep_q3_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_obese_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_obese_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_obese_insomnia_mild,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_phq_2023_gam_sleep_q1_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_insomnia_persistent,
                                                             phq_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q1_obese_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q2_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_insomnia_persistent,
                                                             phq_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q2_obese_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sleep_q3_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_insomnia_persistent,
                                                             phq_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_phq_2023_gam_sleep_q3_obese_insomnia_persistent, paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_insomnia_persistent", ".RDS"))

# m_phq_2023_gam_sleep_q1_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q1_obese_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q2_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q2_obese_insomnia_persistent", ".RDS"))
# m_phq_2023_gam_sleep_q3_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sleep_q3_obese_insomnia_persistent", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q1_obese_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q2_obese_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- substitution(
  m_phq_2023_gam_sleep_q3_obese_insomnia_persistent,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci, paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

## phq9 summary --------------------
m_phq_2023_gam_sub_sleep_q1_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

summary(m_phq_2023_gam_sub_sleep_q1_obese_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_obese_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_obese_99ci, delta = 19)

summary(m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci, delta = 19)

summary(m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci, delta = 19)

summary(m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci, delta = 20)
summary(m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci, delta = 19)

# GAD7 -------------------
## all stratified by sleep quantiles ------------
m_gad_2023_gam_sleep_q1_obese <- brmcoda(clr_acc_mhq_sleep_q1_obese,
                                         gad_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_obese, paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese", ".RDS"))

m_gad_2023_gam_sleep_q2_obese <- brmcoda(clr_acc_mhq_sleep_q2_obese,
                                         gad_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_obese, paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese", ".RDS"))

m_gad_2023_gam_sleep_q3_obese <- brmcoda(clr_acc_mhq_sleep_q3_obese,
                                         gad_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                           + icd_any_at_acc,
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_obese, paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese", ".RDS"))

# m_gad_2023_gam_sleep_q1_obese <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese", ".RDS"))
# m_gad_2023_gam_sleep_q2_obese <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese", ".RDS"))
# m_gad_2023_gam_sleep_q3_obese <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_obese_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_obese_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_obese_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_obese_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_obese,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_obese_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

## quantiles - good sleep -----------------------
m_gad_2023_gam_sleep_q1_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_goodsleep,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_obese_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q2_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_goodsleep,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_obese_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_goodsleep", ".RDS"))

m_gad_2023_gam_sleep_q3_obese_goodsleep <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_goodsleep,
                                                   gad_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                     + icd_any_at_acc,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_obese_goodsleep, paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_goodsleep", ".RDS"))

# m_gad_2023_gam_sleep_q1_obese_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q2_obese_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_goodsleep", ".RDS"))
# m_gad_2023_gam_sleep_q3_obese_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_obese_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_obese_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_obese_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

## quantiles - mild insomnia -----------------------
m_gad_2023_gam_sleep_q1_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_insomnia_mild,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_obese_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q2_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_insomnia_mild,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_obese_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_insomnia_mild", ".RDS"))

m_gad_2023_gam_sleep_q3_obese_insomnia_mild <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_insomnia_mild,
                                                       gad_2023 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                         + icd_any_at_acc,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_obese_insomnia_mild, paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_insomnia_mild", ".RDS"))

# m_gad_2023_gam_sleep_q1_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q2_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_insomnia_mild", ".RDS"))
# m_gad_2023_gam_sleep_q3_obese_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_obese_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_obese_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_obese_insomnia_mild,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

## quantiles - persistent insomnia -----------------------
m_gad_2023_gam_sleep_q1_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q1_obese_insomnia_persistent,
                                                             gad_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q1_obese_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q2_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q2_obese_insomnia_persistent,
                                                             gad_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q2_obese_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sleep_q3_obese_insomnia_persistent <- brmcoda(clr_acc_mhq_2023_sleep_q3_obese_insomnia_persistent,
                                                             gad_2023 ~ 
                                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                                               s(age_at_acc) + sex + white + working + edu + never_smoked + current_drinker + s(deprivation)
                                                               + icd_any_at_acc,
                                                             family = zero_inflated_negbinomial(),
                                                             chains = 4, cores = 4,
                                                             backend = "cmdstanr"
)
saveRDS(m_gad_2023_gam_sleep_q3_obese_insomnia_persistent, paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_insomnia_persistent", ".RDS"))

# m_gad_2023_gam_sleep_q1_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q1_obese_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q2_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q2_obese_insomnia_persistent", ".RDS"))
# m_gad_2023_gam_sleep_q3_obese_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sleep_q3_obese_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q1_obese_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q2_obese_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- substitution(
  m_gad_2023_gam_sleep_q3_obese_insomnia_persistent,
  delta = 1:19,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.99
)
saveRDS(m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci, paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

## gad summary --------------------
m_gad_2023_gam_sub_sleep_q1_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

summary(m_gad_2023_gam_sub_sleep_q1_obese_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_obese_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_obese_99ci, delta = 19)

summary(m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci, delta = 19)

summary(m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci, delta = 19)

summary(m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci, delta = 20)
summary(m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci, delta = 19)
