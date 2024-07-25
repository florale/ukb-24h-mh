
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

m_phq_gam <- brmcoda(clr_acc_mhq,
                     phq ~ 
                       s(ilr1) + s(ilr2) + s(ilr3) +
                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                     chains = 4, cores = 4,
                     backend = "cmdstanr"
)
saveRDS(m_phq_gam, paste0(outputdir, "m_phq_gam", ".RDS"))

m_gad_gam <- brmcoda(clr_acc_mhq,
                     gad ~ 
                       s(ilr1) + s(ilr2) + s(ilr3) +
                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                     chains = 4, cores = 4,
                     backend = "cmdstanr"
)
saveRDS(m_gad_gam, paste0(outputdir, "m_gad_gam", ".RDS"))

m_pcl_gam <- brmcoda(clr_acc_mhq,
                     pcl ~ 
                       s(ilr1) + s(ilr2) + s(ilr3) +
                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                     chains = 4, cores = 4,
                     backend = "cmdstanr"
)
saveRDS(m_pcl_gam, paste0(outputdir, "m_pcl_gam", ".RDS"))

m_wellbeing_gam <- brmcoda(clr_acc_mhq,
                           wellbeing ~ 
                             s(ilr1) + s(ilr2) + s(ilr3) +
                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                           chains = 4, cores = 4,
                           backend = "cmdstanr"
)
saveRDS(m_wellbeing_gam, paste0(outputdir, "m_wellbeing_gam", ".RDS"))

m_dep_lifetime_gam <- brmcoda(clr_acc_mhq,
                              dep_lifetime ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              family = bernoulli(link = "logit"),
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_dep_lifetime_gam, paste0(outputdir, "m_dep_lifetime_gam", ".RDS"))

m_anx_lifetime_gam <- brmcoda(clr_acc_mhq,
                              anx_lifetime ~ 
                                s(ilr1) + s(ilr2) + s(ilr3) +
                                age + sex + white + working + edu + smoking + alcohol + deprivation, 
                              family = bernoulli(link = "logit"),
                              chains = 4, cores = 4,
                              backend = "cmdstanr"
)
saveRDS(m_anx_lifetime_gam, paste0(outputdir, "m_anx_lifetime_gam", ".RDS"))

m_bipolar_lifetime_gam <- brmcoda(clr_acc_mhq,
                                  bipolar_lifetime ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                  family = bernoulli(link = "logit"),
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_bipolar_lifetime_gam, paste0(outputdir, "m_bipolar_lifetime_gam", ".RDS"))

m_psychosis_lifetime_gam <- brmcoda(clr_acc_mhq,
                                    psychosis_lifetime ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    family = bernoulli(link = "logit"),
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_psychosis_lifetime_gam, paste0(outputdir, "m_psychosis_lifetime_gam", ".RDS"))

m_selfharm_gam <- brmcoda(clr_acc_mhq,
                          selfharm ~ 
                            s(ilr1) + s(ilr2) + s(ilr3) +
                            age + sex + white + working + edu + smoking + alcohol + deprivation, 
                          family = bernoulli(link = "logit"),
                          chains = 4, cores = 4,
                          backend = "cmdstanr"
)
saveRDS(m_selfharm_gam, paste0(outputdir, "m_selfharm_gam", ".RDS"))

# m_phq_gam <- readRDS(paste0(outputdir, "m_phq_gam", ".RDS"))
# m_gad_gam <- readRDS(paste0(outputdir, "m_gad_gam", ".RDS"))
# m_pcl_gam <- readRDS(paste0(outputdir, "m_pcl_gam", ".RDS"))
# m_selfharm_gam <- readRDS(paste0(outputdir, "m_selfharm_gam", ".RDS"))
# m_wellbeing_gam <- readRDS(paste0(outputdir, "m_wellbeing_gam", ".RDS"))
# m_dep_lifetime_gam <- readRDS(paste0(outputdir, "m_dep_lifetime_gam", ".RDS"))
# m_anx_lifetime_gam <- readRDS(paste0(outputdir, "m_anx_lifetime_gam", ".RDS"))
# m_bipolar_lifetime_gam <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam", ".RDS"))
# m_psychosis_lifetime_gam <-readRDS(paste0(outputdir, "m_psychosis_lifetime_gam", ".RDS"))

