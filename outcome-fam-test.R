
local <- TRUE

# MODELS --------------------
if (local) {
  source("ukb-24h-mh-utils.R")
  source(paste0(redir, "ukb_utils.R"))
  
  clr_acc_mhq <- readRDS(paste0(inputdir, "clr_acc_mhq", "RDS"))
  clr_acc_mhq_sleep_q1 <- readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q1", "RDS"))
  clr_acc_mhq_sleep_q2 <- readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q2", "RDS"))
  clr_acc_mhq_sleep_q3 <- readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q3", "RDS"))
  
  
} else {
  library(data.table)
  library(extraoperators)
  library(compositions)
  library(multilevelcoda)
  library(brms)
  library(cmdstanr)
  library(insight)
  
  library(doFuture)
  library(foreach)
  library(parallel)
  library(doRNG)
  library(future)
  library(loo)
  
  outputdir <- ""
  clr_acc_mhq <- readRDS(paste0(outputdir, "clr_acc_mhq.RDS"))
}

m_phq_gam_sleep_q1_negbnm <- brmcoda(clr_acc_mhq_sleep_q1,
                                     phq ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                     chains = 4, cores = 4,
                                     family = negbinomial(link = "log"),
                                     backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_negbnm, paste0(outputdir, "m_phq_gam_sleep_q1_negbnm", ".RDS"))

m_phq_gam_sleep_q1_zinegbnm <- brmcoda(clr_acc_mhq_sleep_q1,
                                       phq ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                       chains = 4, cores = 4,
                                       family = zero_inflated_negbinomial(link = "log"),
                                       backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_zinegbnm, paste0(outputdir, "m_phq_gam_sleep_q1_zinegbnm", ".RDS"))

m_phq_gam_sleep_q1_hnegbnm <- brmcoda(clr_acc_mhq_sleep_q1,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      family = hurdle_negbinomial(link = "log"),
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_hnegbnm, paste0(outputdir, "m_phq_gam_sleep_q1_hnegbnm", ".RDS"))

m_phq_gam_sleep_q1_gamma <- brmcoda(clr_acc_mhq_sleep_q1,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    family = hurdle_gamma(link = "log"),
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_gamma, paste0(outputdir, "m_phq_gam_sleep_q1_gamma", ".RDS"))

# intercept only
m_phq_negbnm <- brmcoda(clr_acc_mhq_sleep_q1,
                        phq ~ 1,
                        # s(ilr1) + s(ilr2) + s(ilr3) +
                        # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                        chains = 4, cores = 4,
                        family = negbinomial(link = "log"),
                        backend = "cmdstanr"
)
saveRDS(m_phq_negbnm, paste0(outputdir, "m_phq_negbnm", ".RDS"))

m_phq_zinegbnm <- brmcoda(clr_acc_mhq_sleep_q1,
                          phq ~ 1,
                          # s(ilr1) + s(ilr2) + s(ilr3) +
                          # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                          chains = 4, cores = 4,
                          family = zero_inflated_negbinomial(link = "log"),
                          backend = "cmdstanr"
)
saveRDS(m_phq_zinegbnm, paste0(outputdir, "m_phq_zinegbnm", ".RDS"))

m_phq_hnegbn <- brmcoda(clr_acc_mhq_sleep_q1,
                        phq ~ 1,
                        # s(ilr1) + s(ilr2) + s(ilr3) +
                        # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                        chains = 4, cores = 4,
                        family = hurdle_negbinomial(link = "log"),
                        backend = "cmdstanr"
)
saveRDS(m_phq_hnegbn, paste0(outputdir, "m_phq_hnegbn", ".RDS"))


m_phq_gamma <- brmcoda(clr_acc_mhq_sleep_q1,
                       phq ~ 1,
                       # s(ilr1) + s(ilr2) + s(ilr3) +
                       # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                       chains = 4, cores = 4,
                       family = hurdle_gamma(link = "log"),
                       backend = "cmdstanr"
)
saveRDS(m_phq_hnegbn, paste0(outputdir, "m_phq_hnegbn", ".RDS"))
# set prior for intercept

prior1 <- prior(normal(3, 3), class = "Intercept")
m_phq_negbnm_p <- brmcoda(clr_acc_mhq_sleep_q1,
                          phq ~ 1,
                          # s(ilr1) + s(ilr2) + s(ilr3) +
                          # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                          chains = 4, cores = 4,
                          family = negbinomial(link = "log"),
                          prior = prior1,
                          backend = "cmdstanr"
)
m_phq_zinegbnm_p <- brmcoda(clr_acc_mhq_sleep_q1,
                            phq ~ 1,
                            # s(ilr1) + s(ilr2) + s(ilr3) +
                            # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                            chains = 4, cores = 4,
                            family = zero_inflated_negbinomial(link = "log"),
                            prior = prior1,
                            backend = "cmdstanr"
)
saveRDS(m_phq_zinegbnm_p, paste0(outputdir, "m_phq_zinegbnm_p", ".RDS"))

m_phq_hnegbn_p <- brmcoda(clr_acc_mhq_sleep_q1,
                          phq ~ 1,
                          # s(ilr1) + s(ilr2) + s(ilr3) +
                          # age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                          chains = 4, cores = 4,
                          family = hurdle_negbinomial(link = "log"),
                          prior = prior1,
                          backend = "cmdstanr"
)
saveRDS(m_phq_hnegbn_p, paste0(outputdir, "m_phq_hnegbn_p", ".RDS"))


# pp check
pp_check(m_phq_gam_sleep_q1) + xlim(c(0, 27))
pp_check(m_phq_gam_sleep_q1_negbnm) + xlim(c(0, 27))
pp_check(m_phq_gam_sleep_q1_zinegbnm) + xlim(c(0, 27))
pp_check(m_phq_gam_sleep_q1_hnegbnm) + xlim(c(0, 27))
pp_check(m_phq_gam_sleep_q1_gamma) + xlim(c(0, 27))

pp_check(m_phq_negbnm) + xlim(c(0, 27))
pp_check(m_phq_zinegbnm) + xlim(c(0, 27))
pp_check(m_phq_hnegbn) + xlim(c(0, 27))
pp_check(m_phq_gamma) + xlim(c(0, 27))

pp_check(m_phq_negbnm_p) + xlim(c(0, 27))
pp_check(m_phq_zinegbnm_p) + xlim(c(0, 27))
pp_check(m_phq_hnegbn_p) + xlim(c(0, 27))


