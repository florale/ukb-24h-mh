
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
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1, paste0(outputdir, "m_phq_gam_sub_sleep_q1", ".RDS"))

m_phq_gam_sub_sleep_q2 <- substitution(
  m_phq_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2, paste0(outputdir, "m_phq_gam_sub_sleep_q2", ".RDS"))

m_phq_gam_sub_sleep_q3 <- substitution(
  m_phq_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3, paste0(outputdir, "m_phq_gam_sub_sleep_q3", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 25)
summary(m_phq_gam_sub_sleep_q2, delta = 25)
summary(m_phq_gam_sub_sleep_q3, delta = 25)

## phq - sleep ----------------------
m_phq_gam_sub_sleep_short <- substitution(
  m_phq_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short, paste0(outputdir, "m_phq_gam_sub_sleep_short", ".RDS"))

m_phq_gam_sub_sleep_normal <- substitution(
  m_phq_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal, paste0(outputdir, "m_phq_gam_sub_sleep_normal", ".RDS"))

m_phq_gam_sub_sleep_long <- substitution(
  m_phq_gam_sleep_long,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long, paste0(outputdir, "m_phq_gam_sub_sleep_long", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 25)
summary(m_phq_gam_sub_sleep_normal, delta = 25)
summary(m_phq_gam_sub_sleep_long, delta = 25)

## gad - quantile sleep ----------------------
m_gad_gam_sub_sleep_q1 <- substitution(
  m_gad_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1, paste0(outputdir, "m_gad_gam_sub_sleep_q1", ".RDS"))

m_gad_gam_sub_sleep_q2 <- substitution(
  m_gad_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2, paste0(outputdir, "m_gad_gam_sub_sleep_q2", ".RDS"))

m_gad_gam_sub_sleep_q3 <- substitution(
  m_gad_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3, paste0(outputdir, "m_gad_gam_sub_sleep_q3", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_gad_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_gad_gam_sub_sleep_q3, delta = 25, digits = 3)

## gad - sleep ----------------------
m_gad_gam_sub_sleep_short <- substitution(
  m_gad_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_short, paste0(outputdir, "m_gad_gam_sub_sleep_short", ".RDS"))

m_gad_gam_sub_sleep_normal <- substitution(
  m_gad_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_normal, paste0(outputdir, "m_gad_gam_sub_sleep_normal", ".RDS"))

m_gad_gam_sub_sleep_long <- substitution(
  m_gad_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_long, paste0(outputdir, "m_gad_gam_sub_sleep_long", ".RDS"))

summary(m_gad_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_gad_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_gad_gam_sub_sleep_long, delta = 25, digits = 3)

## wellbeing - quantile sleep ----------------------
m_wellbeing_gam_sub_sleep_q1 <- substitution(
  m_wellbeing_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_q1, paste0(outputdir, "m_wellbeing_gam_sub_sleep_q1", ".RDS"))

m_wellbeing_gam_sub_sleep_q2 <- substitution(
  m_wellbeing_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_q2, paste0(outputdir, "m_wellbeing_gam_sub_sleep_q2", ".RDS"))

m_wellbeing_gam_sub_sleep_q3 <- substitution(
  m_wellbeing_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_q3, paste0(outputdir, "m_wellbeing_gam_sub_sleep_q3", ".RDS"))

summary(m_wellbeing_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_wellbeing_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_wellbeing_gam_sub_sleep_q3, delta = 25, digits = 3)

## wellbeing - sleep ----------------------
m_wellbeing_gam_sub_sleep_short <- substitution(
  m_wellbeing_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_short, paste0(outputdir, "m_wellbeing_gam_sub_sleep_short", ".RDS"))

m_wellbeing_gam_sub_sleep_normal <- substitution(
  m_wellbeing_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_normal, paste0(outputdir, "m_wellbeing_gam_sub_sleep_normal", ".RDS"))

m_wellbeing_gam_sub_sleep_long <- substitution(
  m_wellbeing_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_wellbeing_gam_sub_sleep_long, paste0(outputdir, "m_wellbeing_gam_sub_sleep_long", ".RDS"))

summary(m_wellbeing_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_wellbeing_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_wellbeing_gam_sub_sleep_long, delta = 25, digits = 3)

## dep_lifetime - quantile sleep ----------------------
m_dep_lifetime_gam_sub_sleep_q1 <- substitution(
  m_dep_lifetime_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q1, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q1", ".RDS"))

m_dep_lifetime_gam_sub_sleep_q2 <- substitution(
  m_dep_lifetime_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q2, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q2", ".RDS"))

m_dep_lifetime_gam_sub_sleep_q3 <- substitution(
  m_dep_lifetime_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_q3, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q3", ".RDS"))

summary(m_dep_lifetime_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_dep_lifetime_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_dep_lifetime_gam_sub_sleep_q3, delta = 25, digits = 3)

## dep_lifetime - sleep ----------------------
m_dep_lifetime_gam_sub_sleep_short <- substitution(
  m_dep_lifetime_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_short, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_short", ".RDS"))

m_dep_lifetime_gam_sub_sleep_normal <- substitution(
  m_dep_lifetime_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_normal, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_normal", ".RDS"))

m_dep_lifetime_gam_sub_sleep_long <- substitution(
  m_dep_lifetime_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_dep_lifetime_gam_sub_sleep_long, paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_long", ".RDS"))

summary(m_dep_lifetime_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_dep_lifetime_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_dep_lifetime_gam_sub_sleep_long, delta = 25, digits = 3)

## anx_lifetime - quantile sleep ----------------------
m_anx_lifetime_gam_sub_sleep_q1 <- substitution(
  m_anx_lifetime_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_q1, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q1", ".RDS"))

m_anx_lifetime_gam_sub_sleep_q2 <- substitution(
  m_anx_lifetime_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_q2, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q2", ".RDS"))

m_anx_lifetime_gam_sub_sleep_q3 <- substitution(
  m_anx_lifetime_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_q3, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q3", ".RDS"))

summary(m_anx_lifetime_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_anx_lifetime_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_anx_lifetime_gam_sub_sleep_q3, delta = 25, digits = 3)

## anx_lifetime - sleep ----------------------
m_anx_lifetime_gam_sub_sleep_short <- substitution(
  m_anx_lifetime_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_short, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_short", ".RDS"))

m_anx_lifetime_gam_sub_sleep_normal <- substitution(
  m_anx_lifetime_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_normal, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_normal", ".RDS"))

m_anx_lifetime_gam_sub_sleep_long <- substitution(
  m_anx_lifetime_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_anx_lifetime_gam_sub_sleep_long, paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_long", ".RDS"))

summary(m_anx_lifetime_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_anx_lifetime_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_anx_lifetime_gam_sub_sleep_long, delta = 25, digits = 3)

## bipolar_lifetime - quantile sleep ----------------------
m_bipolar_lifetime_gam_sub_sleep_q1 <- substitution(
  m_bipolar_lifetime_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_q1, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q1", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_q2 <- substitution(
  m_bipolar_lifetime_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_q2, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q2", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_q3 <- substitution(
  m_bipolar_lifetime_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_q3, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q3", ".RDS"))

summary(m_bipolar_lifetime_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_bipolar_lifetime_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_bipolar_lifetime_gam_sub_sleep_q3, delta = 25, digits = 3)

## bipolar_lifetime - sleep ----------------------
m_bipolar_lifetime_gam_sub_sleep_short <- substitution(
  m_bipolar_lifetime_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_short, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_short", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_normal <- substitution(
  m_bipolar_lifetime_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_normal, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_normal", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_long <- substitution(
  m_bipolar_lifetime_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_bipolar_lifetime_gam_sub_sleep_long, paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_long", ".RDS"))

summary(m_bipolar_lifetime_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_bipolar_lifetime_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_bipolar_lifetime_gam_sub_sleep_long, delta = 25, digits = 3)

## psychosis_lifetime - quantile sleep ----------------------
m_psychosis_lifetime_gam_sub_sleep_q1 <- substitution(
  m_psychosis_lifetime_gam_sleep_q1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_q1, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q1", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_q2 <- substitution(
  m_psychosis_lifetime_gam_sleep_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_q2, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q2", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_q3 <- substitution(
  m_psychosis_lifetime_gam_sleep_q3,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_q3, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q3", ".RDS"))

summary(m_psychosis_lifetime_gam_sub_sleep_q1, delta = 25, digits = 3)
summary(m_psychosis_lifetime_gam_sub_sleep_q2, delta = 25, digits = 3)
summary(m_psychosis_lifetime_gam_sub_sleep_q3, delta = 25, digits = 3)

## psychosis_lifetime - sleep ----------------------
m_psychosis_lifetime_gam_sub_sleep_short <- substitution(
  m_psychosis_lifetime_gam_sleep_short,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_short, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_short", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_normal <- substitution(
  m_psychosis_lifetime_gam_sleep_normal,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_normal, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_normal", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_long <- substitution(
  m_psychosis_lifetime_gam_sleep_long,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_psychosis_lifetime_gam_sub_sleep_long, paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_long", ".RDS"))

summary(m_psychosis_lifetime_gam_sub_sleep_short, delta = 25, digits = 3)
summary(m_psychosis_lifetime_gam_sub_sleep_normal, delta = 25, digits = 3)
summary(m_psychosis_lifetime_gam_sub_sleep_long, delta = 25, digits = 3)

