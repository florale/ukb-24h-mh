
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

m_phq_gam_unadj <- readRDS(paste0(outputdir, "m_phq_gam_unadj", ".RDS"))
m_gad_gam_unadj <- readRDS(paste0(outputdir, "m_gad_gam_unadj", ".RDS"))
m_pcl_gam_unadj <- readRDS(paste0(outputdir, "m_pcl_gam_unadj", ".RDS"))
m_selfharm_gam_unadj <- readRDS(paste0(outputdir, "m_selfharm_gam_unadj", ".RDS"))
m_wellbeing_gam_unadj <- readRDS(paste0(outputdir, "m_wellbeing_gam_unadj", ".RDS"))

m_dep_lifetime_gam_unadj <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_unadj", ".RDS"))
m_anx_lifetime_gam_unadj <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_unadj", ".RDS"))
m_bipolar_lifetime_gam_unadj <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_unadj", ".RDS"))
m_psychosis_lifetime_gam_unadj <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_unadj", ".RDS"))

# substitution
m_phq_gam_sub_unadj <- substitution(
  m_phq_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_unadj, paste0(outputdir, "m_phq_gam_sub_unadj", ".RDS"))

m_gad_gam_sub_unadj <- substitution(
  m_gad_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_unadj, paste0(outputdir, "m_gad_gam_sub_unadj", ".RDS"))

m_pcl_gam_sub_unadj <- substitution(
  m_pcl_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_pcl_gam_sub_unadj, paste0(outputdir, "m_pcl_gam_sub_unadj", ".RDS"))

m_selfharm_gam_sub_unadj <- substitution(
  m_selfharm_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_selfharm_gam_sub_unadj, paste0(outputdir, "m_selfharm_gam_sub_unadj", ".RDS"))

m_wellbeing_gam_sub_unadj <- substitution(
  m_wellbeing_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_unadj, paste0(outputdir, "m_wellbeing_gam_sub_unadj", ".RDS"))

m_dep_lifetime_gam_sub_unadj <- substitution(
  m_dep_lifetime_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_unadj, paste0(outputdir, "m_dep_lifetime_gam_sub_unadj", ".RDS"))

m_anx_lifetime_gam_sub_unadj <- substitution(
  m_anx_lifetime_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_unadj, paste0(outputdir, "m_anx_lifetime_gam_sub_unadj", ".RDS"))

m_bipolar_lifetime_gam_sub_unadj <- substitution(
  m_bipolar_lifetime_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_unadj, paste0(outputdir, "m_bipolar_lifetime_gam_sub_unadj", ".RDS"))

m_psychosis_lifetime_gam_sub_unadj <- substitution(
  m_psychosis_lifetime_gam_unadj,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_unadj, paste0(outputdir, "m_psychosis_lifetime_gam_sub_unadj", ".RDS"))

# substitution results
m_phq_gam_sub_unadj <- readRDS(paste0(outputdir, "m_phq_gam_sub_unadj", ".RDS"))
m_gad_gam_sub_unadj <- readRDS(paste0(outputdir, "m_gad_gam_sub_unadj", ".RDS"))
m_pcl_gam_sub_unadj <- readRDS(paste0(outputdir, "m_pcl_gam_sub_unadj", ".RDS"))
m_selfharm_gam_sub_unadj <- readRDS(paste0(outputdir, "m_selfharm_gam_sub_unadj", ".RDS"))
m_wellbeing_gam_sub_unadj <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_unadj", ".RDS"))

m_dep_lifetime_gam_sub_unadj <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_unadj", ".RDS"))
m_anx_lifetime_gam_sub_unadj <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_unadj", ".RDS"))
m_bipolar_lifetime_gam_sub_unadj <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_unadj", ".RDS"))
m_psychosis_lifetime_gam_sub_unadj <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_unadj", ".RDS"))

summary(m_phq_gam_sub_unadj, delta = 30)
summary(m_gad_gam_sub_unadj, delta = 30)
summary(m_pcl_gam_sub_unadj, delta = 30)
summary(m_wellbeing_gam_sub_unadj, delta = 30)

summary(m_selfharm_gam_sub_unadj, delta = 30)
summary(m_dep_lifetime_gam_sub_unadj, delta = 30)
summary(m_anx_lifetime_gam_sub_unadj, delta = 30)
summary(m_bipolar_lifetime_gam_sub_unadj, delta = 30)
summary(m_psychosis_lifetime_gam_sub_unadj, delta = 30)
