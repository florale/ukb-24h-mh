

local <- TRUE

# MODELS --------------------
if (local) {
  source("ukb-24h-mh-utils.R")
  source(paste0(redir, "ukb_utils.R"))
  
  clr_acc_mhq <- readRDS(paste0(inputdir, "clr_acc_mhq", "RDS"))
  clr_acc_mhq_sleep_q1 <-
    readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q1", "RDS"))
  clr_acc_mhq_sleep_q2 <-
    readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q2", "RDS"))
  clr_acc_mhq_sleep_q3 <-
    readRDS(paste0(inputdir, "clr_acc_mhq_sleep_q3", "RDS"))
  
  
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
m_phq_gam_sleep_q1_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q1,
  phq ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  chains = 4,
  cores = 4,
  family = zero_inflated_negbinomial(link = "log"),
  backend = "cmdstanr"
)
saveRDS(
  m_phq_gam_sleep_q1_zinegbnm,
  paste0(outputdir, "m_phq_gam_sleep_q1_zinegbnm", ".RDS")
)

m_phq_gam_sleep_q2_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q2,
  phq ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  family = zero_inflated_negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
saveRDS(
  m_phq_gam_sleep_q2_zinegbnm,
  paste0(outputdir, "m_phq_gam_sleep_q2_zinegbnm", ".RDS")
)

m_phq_gam_sleep_q3_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q3,
  phq ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  family = zero_inflated_negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
saveRDS(
  m_phq_gam_sleep_q3_zinegbnm,
  paste0(outputdir, "m_phq_gam_sleep_q3_zinegbnm", ".RDS")
)

# gad - quantile sleep ----------------------
m_gad_gam_sleep_q1_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q1,
  gad ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  family = zero_inflated_negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
saveRDS(
  m_gad_gam_sleep_q1_zinegbnm,
  paste0(outputdir, "m_gad_gam_sleep_q1_zinegbnm", ".RDS")
)

m_gad_gam_sleep_q2_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q2,
  gad ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  family = zero_inflated_negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
saveRDS(
  m_gad_gam_sleep_q2_zinegbnm,
  paste0(outputdir, "m_gad_gam_sleep_q2_zinegbnm", ".RDS")
)

m_gad_gam_sleep_q3_zinegbnm <- brmcoda(
  clr_acc_mhq_sleep_q3,
  gad ~
    s(ilr1) + s(ilr2) + s(ilr3) +
    age + sex + white + working + edu + never_smoked + current_drinker + deprivation,
  family = zero_inflated_negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
saveRDS(
  m_gad_gam_sleep_q3_zinegbnm,
  paste0(outputdir, "m_gad_gam_sleep_q3_zinegbnm", ".RDS")
)
