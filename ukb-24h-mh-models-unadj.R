source("ukb-24h-mh-data.R")

# PHQ9 -------------------
## quantiles - good sleep baseline -----------------------
m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                      phq_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                      phq_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                      phq_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - insomnia (combined) baseline -----------------------
m_unadj_phq_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                     phq_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                     phq_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                     phq_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_phq_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_unadj_phq_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_unadj_phq_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_unadj_phq_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q1_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q2_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_unadj_phq_2023_gam_sleep_q3_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## phq9 summary --------------------
m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

# baseline insomnia
summary(m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]

summary(m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]

# # standardised estimates
# ## by quantiles
# d_unadj_phq_2023_gam_sub_sleep_q1_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q1_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q2_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q2_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q3_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q3_99ci, delta = 30, digits = "asis"))
# 
# d_unadj_phq_2023_gam_sub_sleep_q1_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q2_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q3_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3$data$phq_2023, na.rm = T), 2)]
# 
# ## by quantiles + insomnia
# d_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 30, digits = "asis"))
# 
# d_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 30, digits = "asis"))
# d_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- as.data.table(summary(m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 30, digits = "asis"))
# 
# d_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_goodsleep_bl$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_goodsleep_bl$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_goodsleep_bl$data$phq_2023, na.rm = T), 2)]
# 
# d_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q1_insomnia_bl$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q2_insomnia_bl$data$phq_2023, na.rm = T), 2)]
# d_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci[, SMean := round(Mean / sd(clr_acc_mhq_sleep_q3_insomnia_bl$data$phq_2023, na.rm = T), 2)]

# GAD7 -------------------
## quantiles - good sleep baseline -----------------------
m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep_bl,
                                                      gad_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep_bl,
                                                      gad_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep_bl,
                                                      gad_2023 ~ 
                                                        # s(ilr1) + s(ilr2) + s(ilr3),
                                                        ilr1 + ilr2 + ilr3,
                                                      family = zero_inflated_negbinomial(),
                                                      iter = 4000, warmup = 1000,
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q1_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q2_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q3_goodsleep_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

## quantiles - insomnia (combined) baseline -----------------------
m_unadj_gad_2023_gam_sleep_q1_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_bl,
                                                     gad_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q1_insomnia_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q1_insomnia_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q2_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_bl,
                                                     gad_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q2_insomnia_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q2_insomnia_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q3_insomnia_bl <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_bl,
                                                     gad_2023 ~ 
                                                       # s(ilr1) + s(ilr2) + s(ilr3),
                                                       ilr1 + ilr2 + ilr3,
                                                     family = zero_inflated_negbinomial(),
                                                     iter = 4000, warmup = 1000,
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_unadj_gad_2023_gam_sleep_q3_insomnia_bl, paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_unadj_gad_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_unadj_gad_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_unadj_gad_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q1_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q2_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- substitution(
  m_unadj_gad_2023_gam_sleep_q3_insomnia_bl,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  scale = "response",
  cores = 4,
  ci = 0.999
)
saveRDS(m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci, paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## gad summary --------------------
m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

# baseline insomnia
summary(m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]

summary(m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
summary(m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci, delta = 30)[, estimates := paste0(round(Mean, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]")][, .(From, To, estimates)]
