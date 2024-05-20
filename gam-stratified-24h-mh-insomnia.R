source("gam-24h-mh-data.R")

# notes:
## for sleep period, q1 = q1, q2 = q2 + q3, q3 = q4

# INSOMIA -------------------------
# by insomnia only not sleep period -----------
clr_acc_mhq_goodsleep <- complr(data = d_acc_mhq[insomnia == "good sleep"],
                                transform = "ilr",
                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                sbp = sbp,
                                total = 1440)
clr_acc_mhq_insomnia_mild <- complr(data = d_acc_mhq[insomnia == "mild"],
                                    transform = "ilr",
                                    parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                    sbp = sbp,
                                    total = 1440)
clr_acc_mhq_insomnia_moderate <- complr(data = d_acc_mhq[insomnia == "moderate"],
                                        transform = "ilr",
                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                        sbp = sbp,
                                        total = 1440)
clr_acc_mhq_insomnia_severe <- complr(data = d_acc_mhq[insomnia == "severe"],
                                      transform = "ilr",
                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                      sbp = sbp,
                                      total = 1440)

m_phq_gam_goodsleep <- brmcoda(clr_acc_mhq_goodsleep,
                               phq ~ 
                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                 age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                               family = zero_inflated_negbinomial(link = "log"),
                               chains = 4, cores = 4,
                               backend = "cmdstanr"
)
saveRDS(m_phq_gam_goodsleep, paste0(outputdir, "m_phq_gam_goodsleep", ".RDS"))

m_phq_gam_insomnia_mild <- brmcoda(clr_acc_mhq_insomnia_mild,
                                   phq ~ 
                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                   family = zero_inflated_negbinomial(link = "log"),
                                   chains = 4, cores = 4,
                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_insomnia_mild, paste0(outputdir, "m_phq_gam_insomnia_mild", ".RDS"))

m_phq_gam_insomnia_moderate <- brmcoda(clr_acc_mhq_insomnia_moderate,
                                       phq ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                       family = zero_inflated_negbinomial(link = "log"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_phq_gam_insomnia_moderate, paste0(outputdir, "m_phq_gam_insomnia_moderate", ".RDS"))

m_phq_gam_insomnia_severe <- brmcoda(clr_acc_mhq_insomnia_severe,
                                     phq ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                     family = zero_inflated_negbinomial(link = "log"),
                                     chains = 4, cores = 4,
                                     backend = "cmdstanr"
)
saveRDS(m_phq_gam_insomnia_severe, paste0(outputdir, "m_phq_gam_insomnia_severe", ".RDS"))

m_phq_gam_sub_goodsleep <- substitution(
  m_phq_gam_goodsleep,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_goodsleep, paste0(outputdir, "m_phq_gam_sub_goodsleep", ".RDS"))

m_phq_gam_sub_insomnia_mild <- substitution(
  m_phq_gam_insomnia_mild,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_insomnia_mild, paste0(outputdir, "m_phq_gam_sub_insomnia_mild", ".RDS"))

m_phq_gam_sub_insomnia_moderate <- substitution(
  m_phq_gam_insomnia_moderate,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_insomnia_moderate, paste0(outputdir, "m_phq_gam_sub_insomnia_moderate", ".RDS"))

m_phq_gam_sub_insomnia_severe <- substitution(
  m_phq_gam_insomnia_severe,
  delta = 1:28,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_insomnia_severe, paste0(outputdir, "m_phq_gam_sub_insomnia_severe", ".RDS"))

summary(m_phq_gam_sub_goodsleep, delta = 25)
summary(m_phq_gam_sub_insomnia_mild, delta = 25)
summary(m_phq_gam_sub_insomnia_moderate, delta = 25)
summary(m_phq_gam_sub_insomnia_severe, delta = 25)

# quantile - good sleep -----------------------
clr_acc_mhq_sleep_q1_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q2_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q3_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)

m_phq_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                        phq ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                        family = zero_inflated_negbinomial(),
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq_gam_sleep_q1_goodsleep", ".RDS"))

m_phq_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                        phq ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                        family = zero_inflated_negbinomial(),
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq_gam_sleep_q2_goodsleep", ".RDS"))

m_phq_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                        phq ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
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


# quantile - mild insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

m_phq_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                            phq ~ 
                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                              age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                            family = zero_inflated_negbinomial(link = "log"),
                                            chains = 4, cores = 4,
                                            backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                            phq ~ 
                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                              age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                            family = zero_inflated_negbinomial(link = "log"),
                                            chains = 4, cores = 4,
                                            backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                            phq ~ 
                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                              age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                            family = zero_inflated_negbinomial(link = "log"),
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
  delta = 1:28,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))


# quantile - moderate insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_moderate <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q2_insomnia_moderate <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q3_insomnia_moderate <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)

m_phq_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                phq ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                family = zero_inflated_negbinomial(link = "log"),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                phq ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                family = zero_inflated_negbinomial(link = "log"),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                phq ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                family = zero_inflated_negbinomial(link = "log"),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_phq_gam_sleep_q1_insomnia_moderate,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_phq_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_phq_gam_sleep_q2_insomnia_moderate,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_phq_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_phq_gam_sleep_q3_insomnia_moderate,
  delta = 1:25,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

# quantile - severe insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_severe <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q2_insomnia_severe <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q3_insomnia_severe <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)

m_phq_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                              phq ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                              family = zero_inflated_negbinomial(link = "log"),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                              phq ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                              family = zero_inflated_negbinomial(link = "log"),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                              phq ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                              family = zero_inflated_negbinomial(link = "log"),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_gam_sleep_q3_insomnia_severe", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_phq_gam_sleep_q1_insomnia_severe,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_phq_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_phq_gam_sleep_q2_insomnia_severe,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_phq_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_phq_gam_sleep_q3_insomnia_severe,
  delta = 1:24,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

# quantile - moderate + severe = persistent insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q2_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q3_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)

m_phq_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                   phq ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                   phq ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                   phq ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
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

## PHQ summary --------------------------
summary(m_phq_gam_sub_sleep_q1_goodsleep, delta = 25)
summary(m_phq_gam_sub_sleep_q2_goodsleep, delta = 25)
summary(m_phq_gam_sub_sleep_q3_goodsleep, delta = 25)

summary(m_phq_gam_sub_sleep_q1_insomnia_mild, delta = 25)
summary(m_phq_gam_sub_sleep_q2_insomnia_mild, delta = 25)
summary(m_phq_gam_sub_sleep_q3_insomnia_mild, delta = 25)

summary(m_phq_gam_sub_sleep_q1_insomnia_moderate, delta = 25)
summary(m_phq_gam_sub_sleep_q2_insomnia_moderate, delta = 25)
summary(m_phq_gam_sub_sleep_q3_insomnia_moderate, delta = 25)

summary(m_phq_gam_sub_sleep_q1_insomnia_severe, delta = 25)
summary(m_phq_gam_sub_sleep_q2_insomnia_severe, delta = 25)
summary(m_phq_gam_sub_sleep_q3_insomnia_severe, delta = 24)

summary(m_phq_gam_sub_sleep_q1_insomnia_persistent, delta = 25)
summary(m_phq_gam_sub_sleep_q2_insomnia_persistent, delta = 25)
summary(m_phq_gam_sub_sleep_q3_insomnia_persistent, delta = 25)

## PHQ8 ---------------------------------
# PHQ8 quantile - good sleep -----------------------
clr_acc_mhq_sleep_q1_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q2_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q3_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)

m_phq8_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                         phq8 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_gam_sleep_q1_goodsleep", ".RDS"))

m_phq8_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                         phq8 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         family = zero_inflated_negbinomial(),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_gam_sleep_q2_goodsleep", ".RDS"))

m_phq8_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                         phq8 ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
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

# PHQ8 quantile - mild insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

m_phq8_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                             phq8 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                             family = zero_inflated_negbinomial(link = "log"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                             phq8 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                             family = zero_inflated_negbinomial(link = "log"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                             phq8 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                             family = zero_inflated_negbinomial(link = "log"),
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
  delta = 1:28,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_mild", ".RDS"))


# PHQ8 quantile - moderate insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_moderate <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q2_insomnia_moderate <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q3_insomnia_moderate <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)

m_phq8_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                 phq8 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                 family = zero_inflated_negbinomial(link = "log"),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                 phq8 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                 family = zero_inflated_negbinomial(link = "log"),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                 phq8 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                 family = zero_inflated_negbinomial(link = "log"),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_phq8_gam_sleep_q1_insomnia_moderate,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_phq8_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_phq8_gam_sleep_q2_insomnia_moderate,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_phq8_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_phq8_gam_sleep_q3_insomnia_moderate,
  delta = 1:25,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

# PHQ8 quantile - severe insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_severe <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q2_insomnia_severe <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q3_insomnia_severe <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)

m_phq8_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                               phq8 ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                               family = zero_inflated_negbinomial(link = "log"),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                               phq8 ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                               family = zero_inflated_negbinomial(link = "log"),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                               phq8 ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                               family = zero_inflated_negbinomial(link = "log"),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq8_gam_sleep_q3_insomnia_severe", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_phq8_gam_sleep_q1_insomnia_severe,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_phq8_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_phq8_gam_sleep_q2_insomnia_severe,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_phq8_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_phq8_gam_sleep_q3_insomnia_severe,
  delta = 1:24,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq8_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

# PHQ8 quantile - moderate + severe = persistent insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q2_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q3_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)

m_phq8_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                   phq8 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                   phq8 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq8_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                   phq8 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                                   family = zero_inflated_negbinomial(link = "log"),
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

# PHQ8 summary ------------------------
summary(m_phq8_gam_sub_sleep_q1_goodsleep, delta = 25)
summary(m_phq8_gam_sub_sleep_q2_goodsleep, delta = 25)
summary(m_phq8_gam_sub_sleep_q3_goodsleep, delta = 25)

summary(m_phq8_gam_sub_sleep_q1_insomnia_mild, delta = 25)
summary(m_phq8_gam_sub_sleep_q2_insomnia_mild, delta = 25)
summary(m_phq8_gam_sub_sleep_q3_insomnia_mild, delta = 25)

summary(m_phq8_gam_sub_sleep_q1_insomnia_moderate, delta = 25)
summary(m_phq8_gam_sub_sleep_q2_insomnia_moderate, delta = 25)
summary(m_phq8_gam_sub_sleep_q3_insomnia_moderate, delta = 25)

summary(m_phq8_gam_sub_sleep_q1_insomnia_severe, delta = 25)
summary(m_phq8_gam_sub_sleep_q2_insomnia_severe, delta = 25)
summary(m_phq8_gam_sub_sleep_q3_insomnia_severe, delta = 24)

summary(m_phq8_gam_sub_sleep_q1_insomnia_persistent, delta = 25)
summary(m_phq8_gam_sub_sleep_q2_insomnia_persistent, delta = 25)
summary(m_phq8_gam_sub_sleep_q3_insomnia_persistent, delta = 24)
