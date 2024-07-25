source("24h-mh-2016/24h-mh-2016-data.R")

# PHQ4 -------------------
## quantile - good sleep -----------------------
m_phq4_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                              phq4_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_phq4_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                              phq4_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_phq4_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                              phq4_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q3_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sleep_q3_goodsleep", ".RDS"))

m_phq4_2016_gam_sub_sleep_q1_goodsleep <- substitution(
  m_phq4_2016_gam_sleep_q1_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q1_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q1_goodsleep", ".RDS"))

m_phq4_2016_gam_sub_sleep_q2_goodsleep <- substitution(
  m_phq4_2016_gam_sleep_q2_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q2_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q2_goodsleep", ".RDS"))

m_phq4_2016_gam_sub_sleep_q3_goodsleep <- substitution(
  m_phq4_2016_gam_sleep_q3_goodsleep,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q3_goodsleep, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q3_goodsleep", ".RDS"))

## quantile - mild insomnia -----------------------
m_phq4_2016_gam_sleep_q1_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_mild,
                                                  phq4_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq4_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                  phq4_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq4_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                  phq4_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sleep_q3_insomnia_mild", ".RDS"))

m_phq4_2016_gam_sub_sleep_q1_insomnia_mild <- substitution(
  m_phq4_2016_gam_sleep_q1_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q1_insomnia_mild", ".RDS"))

m_phq4_2016_gam_sub_sleep_q2_insomnia_mild <- substitution(
  m_phq4_2016_gam_sleep_q2_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q2_insomnia_mild", ".RDS"))

m_phq4_2016_gam_sub_sleep_q3_insomnia_mild <- substitution(
  m_phq4_2016_gam_sleep_q3_insomnia_mild,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q3_insomnia_mild, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

## quantile - moderate insomnia -----------------------
m_phq4_2016_gam_sleep_q1_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_moderate,
                                                      phq4_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq4_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                      phq4_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq4_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                      phq4_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sleep_q3_insomnia_moderate", ".RDS"))

m_phq4_2016_gam_sub_sleep_q1_insomnia_moderate <- substitution(
  m_phq4_2016_gam_sleep_q1_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))

m_phq4_2016_gam_sub_sleep_q2_insomnia_moderate <- substitution(
  m_phq4_2016_gam_sleep_q2_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))

m_phq4_2016_gam_sub_sleep_q3_insomnia_moderate <- substitution(
  m_phq4_2016_gam_sleep_q3_insomnia_moderate,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q3_insomnia_moderate, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

## quantile - severe insomnia -----------------------
m_phq4_2016_gam_sleep_q1_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_severe,
                                                    phq4_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq4_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                    phq4_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq4_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                    phq4_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sleep_q3_insomnia_severe", ".RDS"))

m_phq4_2016_gam_sub_sleep_q1_insomnia_severe <- substitution(
  m_phq4_2016_gam_sleep_q1_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q1_insomnia_severe", ".RDS"))

m_phq4_2016_gam_sub_sleep_q2_insomnia_severe <- substitution(
  m_phq4_2016_gam_sleep_q2_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q2_insomnia_severe", ".RDS"))

m_phq4_2016_gam_sub_sleep_q3_insomnia_severe <- substitution(
  m_phq4_2016_gam_sleep_q3_insomnia_severe,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q3_insomnia_severe, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

## quantile - persistent insomnia -----------------------
m_phq4_2016_gam_sleep_q1_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q1_insomnia_persistent,
                                                        phq4_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq4_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                        phq4_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq4_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                        phq4_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq4_2016_gam_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sleep_q3_insomnia_persistent", ".RDS"))

m_phq4_2016_gam_sub_sleep_q1_insomnia_persistent <- substitution(
  m_phq4_2016_gam_sleep_q1_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))

m_phq4_2016_gam_sub_sleep_q2_insomnia_persistent <- substitution(
  m_phq4_2016_gam_sleep_q2_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))

m_phq4_2016_gam_sub_sleep_q3_insomnia_persistent <- substitution(
  m_phq4_2016_gam_sleep_q3_insomnia_persistent,
  delta = 1:20,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq4_2016_gam_sub_sleep_q3_insomnia_persistent, paste0(outputdir, "m_phq4_2016_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

## phq4 summary --------------------
summary(m_phq4_2016_gam_sub_sleep_q1_goodsleep, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q2_goodsleep, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q3_goodsleep, delta = 20)

summary(m_phq4_2016_gam_sub_sleep_q1_insomnia_mild, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q2_insomnia_mild, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q3_insomnia_mild, delta = 20)

summary(m_phq4_2016_gam_sub_sleep_q1_insomnia_persistent, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q2_insomnia_persistent, delta = 20)
summary(m_phq4_2016_gam_sub_sleep_q3_insomnia_persistent, delta = 20)

# PHQ8 -------------------
# ## by insomnia symptoms only ----------------
# m_phq8_2016_gam_sleep_goodsleep <- brmcoda(clr_acc_mhq_sleep_goodsleep,
#                                          phq8_2016 ~ 
#                                            s(ilr1) + s(ilr2) + s(ilr3) +
#                                            age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
#                                          family = zero_inflated_negbinomial(),
#                                          chains = 4, cores = 4,
#                                          backend = "cmdstanr"
# )
# saveRDS(m_phq8_2016_gam_sleep_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_goodsleep", ".RDS"))
# 
# m_phq8_2016_gam_sleep_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_insomnia_mild,
#                                          phq8_2016 ~ 
#                                            s(ilr1) + s(ilr2) + s(ilr3) +
#                                            age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
#                                          family = zero_inflated_negbinomial(),
#                                          chains = 4, cores = 4,
#                                          backend = "cmdstanr"
# )
# saveRDS(m_phq8_2016_gam_sleep_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_insomnia_mild", ".RDS"))
# 
# m_phq8_2016_gam_sleep_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_insomnia_persistent,
#                                          phq8_2016 ~ 
#                                            s(ilr1) + s(ilr2) + s(ilr3) +
#                                            age + sex + white + working + edu + never_smoked + current_drinker + deprivation ,
#                                          family = zero_inflated_negbinomial(),
#                                          chains = 4, cores = 4,
#                                          backend = "cmdstanr"
# )
# saveRDS(m_phq8_2016_gam_sleep_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_insomnia_persistent", ".RDS"))
# 
# m_phq8_2016_gam_sub_sleep_goodsleep <- substitution(
#   m_phq8_2016_gam_sleep_goodsleep,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4
# )
# saveRDS(m_phq8_2016_gam_sub_sleep_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_goodsleep", ".RDS"))
# 
# m_phq8_2016_gam_sub_sleep_insomnia_mild <- substitution(
#   m_phq8_2016_gam_sleep_insomnia_mild,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4
# )
# saveRDS(m_phq8_2016_gam_sub_sleep_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_insomnia_mild", ".RDS"))
# 
# m_phq8_2016_gam_sub_sleep_insomnia_persistent <- substitution(
#   m_phq8_2016_gam_sleep_insomnia_persistent,
#   delta = 1:20,
#   ref = "grandmean",
#   level = "aggregate",
#   cores = 4
# )
# saveRDS(m_phq8_2016_gam_sub_sleep_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sub_sleep_insomnia_persistent", ".RDS"))
# 
# summary(m_phq8_2016_gam_sub_sleep_goodsleep, delta = 20)
# summary(m_phq8_2016_gam_sub_sleep_insomnia_mild, delta = 20)
# summary(m_phq8_2016_gam_sub_sleep_insomnia_persistent, delta = 20)
# 
## quantile - good sleep -----------------------
m_phq8_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_phq8_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                              family = zero_inflated_negbinomial(),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_phq8_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                              phq8_2016 ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                  phq8_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                  family = zero_inflated_negbinomial(),
                                                  chains = 4, cores = 4,
                                                  backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                  phq8_2016 ~ 
                                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                      phq8_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                      family = zero_inflated_negbinomial(),
                                                      chains = 4, cores = 4,
                                                      backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                      phq8_2016 ~ 
                                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                    phq8_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                    family = zero_inflated_negbinomial(),
                                                    chains = 4, cores = 4,
                                                    backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                    phq8_2016 ~ 
                                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                        phq8_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                        family = zero_inflated_negbinomial(),
                                                        chains = 4, cores = 4,
                                                        backend = "cmdstanr"
)
saveRDS(m_phq8_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq8_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq8_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                        phq8_2016 ~ 
                                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                                          age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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

# PHQ9 -------------------
## quantile - good sleep -----------------------
m_phq_2016_gam_sleep_q1_goodsleep <- brmcoda(clr_acc_mhq_sleep_q1_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_goodsleep, paste0(outputdir, "m_phq_2016_gam_sleep_q1_goodsleep", ".RDS"))

m_phq_2016_gam_sleep_q2_goodsleep <- brmcoda(clr_acc_mhq_sleep_q2_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                             family = zero_inflated_negbinomial(),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_goodsleep, paste0(outputdir, "m_phq_2016_gam_sleep_q2_goodsleep", ".RDS"))

m_phq_2016_gam_sleep_q3_goodsleep <- brmcoda(clr_acc_mhq_sleep_q3_goodsleep,
                                             phq_2016 ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_mild", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_mild,
                                                 phq_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                 family = zero_inflated_negbinomial(),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_mild, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_mild", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_mild <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_mild,
                                                 phq_2016 ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                       age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_moderate,
                                                     phq_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                     family = zero_inflated_negbinomial(),
                                                     chains = 4, cores = 4,
                                                     backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_moderate, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_moderate", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_moderate <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_moderate,
                                                     phq_2016 ~ 
                                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                                       age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_severe", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_severe,
                                                   phq_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                   family = zero_inflated_negbinomial(),
                                                   chains = 4, cores = 4,
                                                   backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_severe, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_severe", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_severe <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_severe,
                                                   phq_2016 ~ 
                                                     s(ilr1) + s(ilr2) + s(ilr3) +
                                                     age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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
                                                         age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q1_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sleep_q1_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sleep_q2_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q2_insomnia_persistent,
                                                       phq_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
                                                       family = zero_inflated_negbinomial(),
                                                       chains = 4, cores = 4,
                                                       backend = "cmdstanr"
)
saveRDS(m_phq_2016_gam_sleep_q2_insomnia_persistent, paste0(outputdir, "m_phq_2016_gam_sleep_q2_insomnia_persistent", ".RDS"))

m_phq_2016_gam_sleep_q3_insomnia_persistent <- brmcoda(clr_acc_mhq_sleep_q3_insomnia_persistent,
                                                       phq_2016 ~ 
                                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                                         age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
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

## phq summary --------------------
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


