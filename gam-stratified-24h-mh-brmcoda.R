
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

# phq - quantile sleep ----------------------
m_phq_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1, paste0(outputdir, "m_phq_gam_sleep_q1", ".RDS"))

m_phq_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2, paste0(outputdir, "m_phq_gam_sleep_q2", ".RDS"))

m_phq_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                              phq ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3, paste0(outputdir, "m_phq_gam_sleep_q3", ".RDS"))

# phq - sleep duration ----------------------
m_phq_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                 phq ~ 
                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                   age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                 chains = 4, cores = 4,
                                 backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short, paste0(outputdir, "m_phq_gam_sleep_short", ".RDS"))

m_phq_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal, paste0(outputdir, "m_phq_gam_sleep_normal", ".RDS"))

m_phq_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                phq ~ 
                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                  age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                chains = 4, cores = 4,
                                backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long, paste0(outputdir, "m_phq_gam_sleep_long", ".RDS"))

# gad - quantile sleep ----------------------
m_gad_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1, paste0(outputdir, "m_gad_gam_sleep_q1", ".RDS"))

m_gad_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2, paste0(outputdir, "m_gad_gam_sleep_q2", ".RDS"))

m_gad_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                              gad ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3, paste0(outputdir, "m_gad_gam_sleep_q3", ".RDS"))

# gad - sleep duration ----------------------
m_gad_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                 gad ~ 
                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                   age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                 chains = 4, cores = 4,
                                 backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_short, paste0(outputdir, "m_gad_gam_sleep_short", ".RDS"))

m_gad_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_normal, paste0(outputdir, "m_gad_gam_sleep_normal", ".RDS"))

m_gad_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                gad ~ 
                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                  age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                chains = 4, cores = 4,
                                backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_long, paste0(outputdir, "m_gad_gam_sleep_long", ".RDS"))

# wellbeing - quantile sleep ----------------------
m_wellbeing_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                    wellbeing ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_q1, paste0(outputdir, "m_wellbeing_gam_sleep_q1", ".RDS"))

m_wellbeing_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                    wellbeing ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_q2, paste0(outputdir, "m_wellbeing_gam_sleep_q2", ".RDS"))

m_wellbeing_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                    wellbeing ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_q3, paste0(outputdir, "m_wellbeing_gam_sleep_q3", ".RDS"))

# wellbeing - sleep duration ----------------------
m_wellbeing_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                       wellbeing ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_short, paste0(outputdir, "m_wellbeing_gam_sleep_short", ".RDS"))

m_wellbeing_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                        wellbeing ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_normal, paste0(outputdir, "m_wellbeing_gam_sleep_normal", ".RDS"))

m_wellbeing_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                      wellbeing ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam_sleep_long, paste0(outputdir, "m_wellbeing_gam_sleep_long", ".RDS"))

# dep_lifetime - quantile sleep ----------------------
m_dep_lifetime_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                       dep_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_q1, paste0(outputdir, "m_dep_lifetime_gam_sleep_q1", ".RDS"))

m_dep_lifetime_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                       dep_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_q2, paste0(outputdir, "m_dep_lifetime_gam_sleep_q2", ".RDS"))

m_dep_lifetime_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                       dep_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_q3, paste0(outputdir, "m_dep_lifetime_gam_sleep_q3", ".RDS"))

# dep_lifetime - sleep duration ----------------------
m_dep_lifetime_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                          dep_lifetime ~ 
                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                            age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                          family = bernoulli(link = "logit"),
                                          chains = 4, cores = 4,
                                          backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_short, paste0(outputdir, "m_dep_lifetime_gam_sleep_short", ".RDS"))

m_dep_lifetime_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                           dep_lifetime ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_normal, paste0(outputdir, "m_dep_lifetime_gam_sleep_normal", ".RDS"))

m_dep_lifetime_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                         dep_lifetime ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                         family = bernoulli(link = "logit"),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam_sleep_long, paste0(outputdir, "m_dep_lifetime_gam_sleep_long", ".RDS"))

# anx_lifetime - quantile sleep ----------------------
m_anx_lifetime_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                       anx_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_q1, paste0(outputdir, "m_anx_lifetime_gam_sleep_q1", ".RDS"))

m_anx_lifetime_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                       anx_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_q2, paste0(outputdir, "m_anx_lifetime_gam_sleep_q2", ".RDS"))

m_anx_lifetime_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                       anx_lifetime ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       family = bernoulli(link = "logit"),
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_q3, paste0(outputdir, "m_anx_lifetime_gam_sleep_q3", ".RDS"))

# anx_lifetime - sleep duration ----------------------
m_anx_lifetime_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                          anx_lifetime ~ 
                                            s(ilr1) + s(ilr2) + s(ilr3) +
                                            age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                          family = bernoulli(link = "logit"),
                                          chains = 4, cores = 4,
                                          backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_short, paste0(outputdir, "m_anx_lifetime_gam_sleep_short", ".RDS"))

m_anx_lifetime_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                           anx_lifetime ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_normal, paste0(outputdir, "m_anx_lifetime_gam_sleep_normal", ".RDS"))

m_anx_lifetime_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                         anx_lifetime ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                         family = bernoulli(link = "logit"),
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam_sleep_long, paste0(outputdir, "m_anx_lifetime_gam_sleep_long", ".RDS"))

# bipolar_lifetime - quantile sleep ----------------------
m_bipolar_lifetime_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                           bipolar_lifetime ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_q1, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_q1", ".RDS"))

m_bipolar_lifetime_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                           bipolar_lifetime ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_q2, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_q2", ".RDS"))

m_bipolar_lifetime_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                           bipolar_lifetime ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           family = bernoulli(link = "logit"),
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_q3, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_q3", ".RDS"))

# bipolar_lifetime - sleep duration ----------------------
m_bipolar_lifetime_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                              bipolar_lifetime ~ 
                                                s(ilr1) + s(ilr2) + s(ilr3) +
                                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                              family = bernoulli(link = "logit"),
                                              chains = 4, cores = 4,
                                              backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_short, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_short", ".RDS"))

m_bipolar_lifetime_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                               bipolar_lifetime ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                               family = bernoulli(link = "logit"),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_normal, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_normal", ".RDS"))

m_bipolar_lifetime_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                             bipolar_lifetime ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             family = bernoulli(link = "logit"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam_sleep_long, paste0(outputdir, "m_bipolar_lifetime_gam_sleep_long", ".RDS"))

# psychosis_lifetime - quantile sleep ----------------------
m_psychosis_lifetime_gam_sleep_q1 <- brmcoda(clr_acc_mhq_sleep_q1,
                                             psychosis_lifetime ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             family = bernoulli(link = "logit"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_q1, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_q1", ".RDS"))

m_psychosis_lifetime_gam_sleep_q2 <- brmcoda(clr_acc_mhq_sleep_q2,
                                             psychosis_lifetime ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             family = bernoulli(link = "logit"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_q2, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_q2", ".RDS"))

m_psychosis_lifetime_gam_sleep_q3 <- brmcoda(clr_acc_mhq_sleep_q3,
                                             psychosis_lifetime ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             family = bernoulli(link = "logit"),
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_q3, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_q3", ".RDS"))

# psychosis_lifetime - sleep duration ----------------------
m_psychosis_lifetime_gam_sleep_short <- brmcoda(clr_acc_mhq_sleep_short,
                                                psychosis_lifetime ~ 
                                                  s(ilr1) + s(ilr2) + s(ilr3) +
                                                  age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                                family = bernoulli(link = "logit"),
                                                chains = 4, cores = 4,
                                                backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_short, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_short", ".RDS"))

m_psychosis_lifetime_gam_sleep_normal <- brmcoda(clr_acc_mhq_sleep_normal,
                                                 psychosis_lifetime ~ 
                                                   s(ilr1) + s(ilr2) + s(ilr3) +
                                                   age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                                 family = bernoulli(link = "logit"),
                                                 chains = 4, cores = 4,
                                                 backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_normal, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_normal", ".RDS"))

m_psychosis_lifetime_gam_sleep_long <- brmcoda(clr_acc_mhq_sleep_long,
                                               psychosis_lifetime ~ 
                                                 s(ilr1) + s(ilr2) + s(ilr3) +
                                                 age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                               family = bernoulli(link = "logit"),
                                               chains = 4, cores = 4,
                                               backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam_sleep_long, paste0(outputdir, "m_psychosis_lifetime_gam_sleep_long", ".RDS"))
