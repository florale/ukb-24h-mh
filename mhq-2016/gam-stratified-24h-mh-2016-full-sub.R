
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

m_phq_gam_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q1", ".RDS"))
m_phq_gam_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q2", ".RDS"))
m_phq_gam_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_gam_sleep_q3", ".RDS"))

# substitution
## phq - quantile sleep ----------------------
m_phq_gam_sub_sleep_q1 <- substitution(
  m_phq_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1, paste0(outputdir, "m_phq_gam_sub_sleep_q1", ".RDS"))

m_phq_gam_sub_sleep_q2 <- substitution(
  m_phq_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2, paste0(outputdir, "m_phq_gam_sub_sleep_q2", ".RDS"))

m_phq_gam_sub_sleep_q3 <- substitution(
  m_phq_gam_sleep_q3,
  delta = 1:29,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3, paste0(outputdir, "m_phq_gam_sub_sleep_q3", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 20)
summary(m_phq_gam_sub_sleep_q2, delta = 20)
summary(m_phq_gam_sub_sleep_q3, delta = 20)

summary(m_phq_gam_sub_sleep_q1, delta = 25)
summary(m_phq_gam_sub_sleep_q2, delta = 25)
summary(m_phq_gam_sub_sleep_q3, delta = 25)

## gad - quantile sleep ----------------------
m_gad_gam_sub_sleep_q1 <- substitution(
  m_gad_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1, paste0(outputdir, "m_gad_gam_sub_sleep_q1", ".RDS"))

m_gad_gam_sub_sleep_q2 <- substitution(
  m_gad_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2, paste0(outputdir, "m_gad_gam_sub_sleep_q2", ".RDS"))

m_gad_gam_sub_sleep_q3 <- substitution(
  m_gad_gam_sleep_q3,
  delta = 1:29,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3, paste0(outputdir, "m_gad_gam_sub_sleep_q3", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 20)
summary(m_gad_gam_sub_sleep_q2, delta = 20)
summary(m_gad_gam_sub_sleep_q3, delta = 20)

summary(m_gad_gam_sub_sleep_q1, delta = 25)
summary(m_gad_gam_sub_sleep_q2, delta = 25)
summary(m_gad_gam_sub_sleep_q3, delta = 25)

## dep_lifetime - quantile sleep ----------------------
m_dep_lifetime_gam_sub_sleep_q1 <- substitution(
  m_dep_lifetime_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q1, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q1", ".RDS"))

m_dep_lifetime_gam_sub_sleep_q2 <- substitution(
  m_dep_lifetime_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q2, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q2", ".RDS"))

m_dep_lifetime_gam_sub_sleep_q3 <- substitution(
  m_dep_lifetime_gam_sleep_q3,
  delta = 1:28,
  ref = "grandmean",
  level = "aggregate",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q3, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q3", ".RDS"))

summary(m_dep_lifetime_gam_sub_sleep_q1, delta = 20)
summary(m_dep_lifetime_gam_sub_sleep_q2, delta = 20)
summary(m_dep_lifetime_gam_sub_sleep_q3, delta = 20)

