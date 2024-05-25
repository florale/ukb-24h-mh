source("24h-mh-2023-data.R")

# PHQ4 -------------------
## quantile - good sleep -----------------------
m_phq4_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                         phq4_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq4_gam_sleep_q1_goodsleep", ".RDS"))

m_phq4_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                         phq4_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq4_gam_sleep_q2_goodsleep", ".RDS"))

m_phq4_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                         phq4_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq4_gam_sleep_q3_goodsleep", ".RDS"))

m_phq4_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq4_gam_sleep_q1_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq4_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq4_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq4_gam_sleep_q2_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq4_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq4_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq4_gam_sleep_q3_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq4_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq4_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                             phq4_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq4_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq4_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                             phq4_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq4_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq4_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                             phq4_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq4_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq4_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq4_gam_sleep_q1_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq4_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq4_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq4_gam_sleep_q2_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq4_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq4_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq4_gam_sleep_q3_insomnia_mild,
  delta = 1:29,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq4_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq4_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                   phq4_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq4_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                   phq4_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq4_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                   phq4_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq4_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq4_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq4_gam_sleep_q1_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq4_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq4_gam_sleep_q2_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq4_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq4_gam_sleep_q3_insomnia_persistent,
  delta = 1:25,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq4_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

# PHQ8 -------------------
## quantile - good sleep -----------------------
m_phq8_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                         phq8_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_gam_sleep_q1_goodsleep", ".RDS"))

m_phq8_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                         phq8_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_gam_sleep_q2_goodsleep", ".RDS"))

m_phq8_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                         phq8_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq8_gam_sleep_q3_goodsleep", ".RDS"))

m_phq8_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq8_gam_sleep_q1_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq8_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq8_gam_sleep_q2_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq8_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq8_gam_sleep_q3_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq8_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                             phq8_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                             phq8_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                             phq8_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq8_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq8_gam_sleep_q1_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq8_gam_sleep_q2_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq8_gam_sleep_q3_insomnia_mild,
  delta = 1:29,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_mild", ".RDS"))


## quantile - persistent insomnia -----------------------
m_phq8_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                   phq8_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                   phq8_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                   phq8_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq8_gam_sleep_q1_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq8_gam_sleep_q2_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq8_gam_sleep_q3_insomnia_persistent,
  delta = 1:25,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

# PHQ9 -------------------
## quantile - good sleep -----------------------
m_phq_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq_gam_sleep_q1_goodsleep", ".RDS"))

m_phq_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                        family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq_gam_sleep_q2_goodsleep", ".RDS"))

m_phq_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                         phq_2023 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + deprivation +
                                           dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                        family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq_gam_sleep_q3_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq_gam_sleep_q1_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq_gam_sleep_q2_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq_gam_sleep_q3_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                            family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                             phq_2023 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + deprivation +
                                               dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                            family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq_gam_sleep_q1_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq_gam_sleep_q2_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq_gam_sleep_q3_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023, 
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                          
                                                  family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                   phq_2023 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + deprivation +
                                                     dep_anx_diag_2023 + dep_sustances_2023 + cts5_2023,                                         
                                                  family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq_gam_sleep_q1_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq_gam_sleep_q2_insomnia_persistent,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq_gam_sleep_q3_insomnia_persistent,
  delta = 1:25,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))
