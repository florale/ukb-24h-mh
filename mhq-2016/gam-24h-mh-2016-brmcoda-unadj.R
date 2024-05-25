
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

m_phq_gam_unadj <- brmcoda(clr_acc_mhq,
                     phq ~ 
                       s(ilr1) + s(ilr2) + s(ilr3), 
                     iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_phq_gam_unadj, paste0(outputdir, "m_phq_gam_unadj", ".RDS"))

m_gad_gam_unadj <- brmcoda(clr_acc_mhq,
                     gad ~ 
                       s(ilr1) + s(ilr2) + s(ilr3), 
                     iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_gad_gam_unadj, paste0(outputdir, "m_gad_gam_unadj", ".RDS"))

m_pcl_gam_unadj <- brmcoda(clr_acc_mhq,
                     pcl ~ 
                       s(ilr1) + s(ilr2) + s(ilr3), 
                     iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_pcl_gam_unadj, paste0(outputdir, "m_pcl_gam_unadj", ".RDS"))

m_wellbeing_gam_unadj <- brmcoda(clr_acc_mhq,
                           wellbeing ~ 
                             s(ilr1) + s(ilr2) + s(ilr3), 
                           iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                           backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_wellbeing_gam, paste0(outputdir, "m_wellbeing_gam_unadj", ".RDS"))

m_dep_lifetime_gam_unadj <- brmcoda(clr_acc_mhq,
                              dep_lifetime ~ 
                                s(ilr1) + s(ilr2) + s(ilr3), 
                              family = bernoulli(link = "logit"),
                              iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                              backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_dep_lifetime_gam_unadj, paste0(outputdir, "m_dep_lifetime_gam_unadj", ".RDS"))

m_anx_lifetime_gam_unadj <- brmcoda(clr_acc_mhq,
                              anx_lifetime ~ 
                                s(ilr1) + s(ilr2) + s(ilr3), 
                              family = bernoulli(link = "logit"),
                              iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                              backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_anx_lifetime_gam_unadj, paste0(outputdir, "m_anx_lifetime_gam_unadj", ".RDS"))

m_bipolar_lifetime_gam_unadj <- brmcoda(clr_acc_mhq,
                                  bipolar_lifetime ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3), 
                                  family = bernoulli(link = "logit"),
                                  iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                  backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_bipolar_lifetime_gam_unadj, paste0(outputdir, "m_bipolar_lifetime_gam_unadj", ".RDS"))

m_psychosis_lifetime_gam_unadj <- brmcoda(clr_acc_mhq,
                                    psychosis_lifetime ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3), 
                                    family = bernoulli(link = "logit"),
                                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                    backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_psychosis_lifetime_gam_unadj, paste0(outputdir, "m_psychosis_lifetime_gam_unadj", ".RDS"))


m_selfharm_gam_unadj <- brmcoda(clr_acc_mhq,
                          selfharm ~ 
                            s(ilr1) + s(ilr2) + s(ilr3), 
                          family = bernoulli(link = "logit"),
                          iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                          backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(m_selfharm_gam_unadj, paste0(outputdir, "m_selfharm_gam_unadj", ".RDS"))
