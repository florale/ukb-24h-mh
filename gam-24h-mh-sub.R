
local <- TRUE

# MODELS --------------------
if (local) {
  source("ukb-24h-mh-utils.R")
  source(paste0(redir, "ukb_utils.R"))
  
  clr_acc_mhq <- readRDS(paste0(inputdir, "clr_acc_mhq.RDS"))
  
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

m_phq_gam <- readRDS(paste0(outputdir, "m_phq_gam", ".RDS"))
m_gad_gam <- readRDS(paste0(outputdir, "m_gad_gam", ".RDS"))
m_pcl_gam <- readRDS(paste0(outputdir, "m_pcl_gam", ".RDS"))
m_selfharm_gam <- readRDS(paste0(outputdir, "m_selfharm_gam", ".RDS"))
m_wellbeing_gam <- readRDS(paste0(outputdir, "m_wellbeing_gam", ".RDS"))

m_dep_lifetime_gam <- readRDS(paste0(outputdir, "m_dep_lifetime_gam", ".RDS"))
m_anx_lifetime_gam <- readRDS(paste0(outputdir, "m_anx_lifetime_gam", ".RDS"))
m_bipolar_lifetime_gam <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam", ".RDS"))
m_psychosis_lifetime_gam <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam", ".RDS"))

# substitution
m_phq_gam_sub <- substitution(
  m_phq_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub, paste0(outputdir, "m_phq_gam_sub", ".RDS"))

m_gad_gam_sub <- substitution(
  m_gad_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub, paste0(outputdir, "m_gad_gam_sub", ".RDS"))

m_pcl_gam_sub <- substitution(
  m_pcl_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_pcl_gam_sub, paste0(outputdir, "m_pcl_gam_sub", ".RDS"))

m_selfharm_gam_sub <- substitution(
  m_selfharm_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_selfharm_gam_sub, paste0(outputdir, "m_selfharm_gam_sub", ".RDS"))

m_wellbeing_gam_sub <- substitution(
  m_wellbeing_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub, paste0(outputdir, "m_wellbeing_gam_sub", ".RDS"))

m_dep_lifetime_gam_sub <- substitution(
  m_dep_lifetime_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub, paste0(outputdir, "m_dep_lifetime_gam_sub", ".RDS"))

m_anx_lifetime_gam_sub <- substitution(
  m_anx_lifetime_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub, paste0(outputdir, "m_anx_lifetime_gam_sub", ".RDS"))

m_bipolar_lifetime_gam_sub <- substitution(
  m_bipolar_lifetime_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub, paste0(outputdir, "m_bipolar_lifetime_gam_sub", ".RDS"))

m_psychosis_lifetime_gam_sub <- substitution(
  m_psychosis_lifetime_gam,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub, paste0(outputdir, "m_psychosis_lifetime_gam_sub", ".RDS"))

# substitution results
m_phq_gam_sub <- readRDS(paste0(outputdir, "m_phq_gam_sub", ".RDS"))
m_gad_gam_sub <- readRDS(paste0(outputdir, "m_gad_gam_sub", ".RDS"))
m_pcl_gam_sub <- readRDS(paste0(outputdir, "m_pcl_gam_sub", ".RDS"))
m_selfharm_gam_sub <- readRDS(paste0(outputdir, "m_selfharm_gam_sub", ".RDS"))
m_wellbeing_gam_sub <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub", ".RDS"))

m_dep_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub", ".RDS"))
m_anx_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub", ".RDS"))
m_bipolar_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub", ".RDS"))
m_psychosis_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub", ".RDS"))

summary(m_phq_gam_sub, delta = 30)
summary(m_gad_gam_sub, delta = 30)
summary(m_pcl_gam_sub, delta = 30)
summary(m_wellbeing_gam_sub, delta = 30)

summary(m_selfharm_gam_sub, delta = 30)
summary(m_dep_lifetime_gam_sub, delta = 30)
summary(m_anx_lifetime_gam_sub, delta = 30)
summary(m_bipolar_lifetime_gam_sub, delta = 30)
summary(m_psychosis_lifetime_gam_sub, delta = 30)
