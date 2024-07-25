
local <- TRUE

# MODELS --------------------
if (local) {
  source("ukb-24h-mh-utils.R")
  source(paste0(redir, "ukb_utils.R"))
  
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
}

m_phq_gam_sleep_q1_zinegbnm <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q1_zinegbnm", ".RDS"))
m_phq_gam_sleep_q2_zinegbnm <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q2_zinegbnm", ".RDS"))
m_phq_gam_sleep_q3_zinegbnm <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q3_zinegbnm", ".RDS"))

# substitution
## phq - quantile sleep ----------------------
m_phq_gam_sub_sleep_q1_zinegbnm <- substitution(
  m_phq_gam_sleep_q1_zinegbnm,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_zinegbnm, paste0(outputdir, "m_phq_gam_sub_sleep_q1_zinegbnm", ".RDS"))

m_phq_gam_sub_sleep_q2_zinegbnm <- substitution(
  m_phq_gam_sleep_q2_zinegbnm,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_zinegbnm, paste0(outputdir, "m_phq_gam_sub_sleep_q2_zinegbnm", ".RDS"))

m_phq_gam_sub_sleep_q3_zinegbnm <- substitution(
  m_phq_gam_sleep_q3_zinegbnm,
  delta = 1:28,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_zinegbnm, paste0(outputdir, "m_phq_gam_sub_sleep_q3_zinegbnm", ".RDS"))

summary(m_phq_gam_sub_sleep_q1_zinegbnm, delta = 20)
summary(m_phq_gam_sub_sleep_q2_zinegbnm, delta = 20)
summary(m_phq_gam_sub_sleep_q3_zinegbnm, delta = 20)

summary(m_phq_gam_sub_sleep_q1_zinegbnm, delta = 25)
summary(m_phq_gam_sub_sleep_q2_zinegbnm, delta = 25)
summary(m_phq_gam_sub_sleep_q3_zinegbnm, delta = 25)

## gad - quantile sleep ----------------------
m_gad_gam_sub_sleep_q1_zinegbnm <- substitution(
  m_gad_gam_sleep_q1_zinegbnm,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_zinegbnm, paste0(outputdir, "m_gad_gam_sub_sleep_q1_zinegbnm", ".RDS"))

m_gad_gam_sub_sleep_q2_zinegbnm <- substitution(
  m_gad_gam_sleep_q2_zinegbnm,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_zinegbnm, paste0(outputdir, "m_gad_gam_sub_sleep_q2_zinegbnm", ".RDS"))

m_gad_gam_sub_sleep_q3_zinegbnm <- substitution(
  m_gad_gam_sleep_q3_zinegbnm,
  delta = 1:28,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_zinegbnm, paste0(outputdir, "m_gad_gam_sub_sleep_q3_zinegbnm", ".RDS"))

summary(m_gad_gam_sub_sleep_q1_zinegbnm, delta = 20)
summary(m_gad_gam_sub_sleep_q2_zinegbnm, delta = 20)
summary(m_gad_gam_sub_sleep_q3_zinegbnm, delta = 20)

summary(m_gad_gam_sub_sleep_q1_zinegbnm, delta = 25)
summary(m_gad_gam_sub_sleep_q2_zinegbnm, delta = 25)
summary(m_gad_gam_sub_sleep_q3_zinegbnm, delta = 25)
