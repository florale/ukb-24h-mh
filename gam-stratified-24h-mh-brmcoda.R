
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

## by quantile sleep duration --------------------
# phq - quantile sleep ----------------------
m_phq_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1, paste0(outputdir, "m_phq_gam_sleep_q1", ".RDS"))

m_phq_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2, paste0(outputdir, "m_phq_gam_sleep_q2", ".RDS"))

m_phq_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3, paste0(outputdir, "m_phq_gam_sleep_q3", ".RDS"))

# gad - quantile sleep ----------------------
m_gad_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1, paste0(outputdir, "m_gad_gam_sleep_q1", ".RDS"))

m_gad_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2, paste0(outputdir, "m_gad_gam_sleep_q2", ".RDS"))

m_gad_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3, paste0(outputdir, "m_gad_gam_sleep_q3", ".RDS"))

## by raw value sleep duration --------------------
# phq - sleep duration ----------------------
m_phq_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                 phq ~ 
                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                 chains = 4, cores = 4,
                                 backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short, paste0(outputdir, "m_phq_gam_sleep_short", ".RDS"))

m_phq_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal, paste0(outputdir, "m_phq_gam_sleep_normal", ".RDS"))

m_phq_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                phq ~ 
                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                  age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                chains = 4, cores = 4,
                                backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long, paste0(outputdir, "m_phq_gam_sleep_long", ".RDS"))

# gad - sleep duration ----------------------
m_gad_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                 gad ~ 
                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                   age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                 chains = 4, cores = 4,
                                 backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_short, paste0(outputdir, "m_gad_gam_sleep_short", ".RDS"))

m_gad_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_normal, paste0(outputdir, "m_gad_gam_sleep_normal", ".RDS"))

m_gad_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                gad ~ 
                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                  age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                chains = 4, cores = 4,
                                backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_long, paste0(outputdir, "m_gad_gam_sleep_long", ".RDS"))
