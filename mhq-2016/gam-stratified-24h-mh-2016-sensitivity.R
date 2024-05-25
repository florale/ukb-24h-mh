source("gam-24h-mh-data.R")

# BMI ----------------------------
# phq stratified by sleep duration and bmi healthy ----------------------
clr_acc_mhq_sleep_q1_healthy <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q2_healthy <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_q3_healthy <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "undernormal"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)

m_phq_gam_sleep_q1_healthy <- brmcoda(clr_acc_mhq_sleep_q1_healthy,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_healthy, paste0(outputdir, "m_phq_gam_sleep_q1_healthy", ".RDS"))

m_phq_gam_sleep_q2_healthy <- brmcoda(clr_acc_mhq_sleep_q2_healthy,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_healthy, paste0(outputdir, "m_phq_gam_sleep_q2_healthy", ".RDS"))

m_phq_gam_sleep_q3_healthy <- brmcoda(clr_acc_mhq_sleep_q3_healthy,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_healthy, paste0(outputdir, "m_phq_gam_sleep_q3_healthy", ".RDS"))

m_phq_gam_sub_sleep_q1_healthy <- substitution(
  m_phq_gam_sleep_q1_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_q1_healthy", ".RDS"))

m_phq_gam_sub_sleep_q2_healthy <- substitution(
  m_phq_gam_sleep_q2_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_q2_healthy", ".RDS"))

m_phq_gam_sub_sleep_q3_healthy <- substitution(
  m_phq_gam_sleep_q3_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_q3_healthy", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 25)
summary(m_phq_gam_sub_sleep_q1_healthy, delta = 25)

summary(m_phq_gam_sub_sleep_q2, delta = 25)
summary(m_phq_gam_sub_sleep_q2_healthy, delta = 25)

summary(m_phq_gam_sub_sleep_q3, delta = 25)
summary(m_phq_gam_sub_sleep_q3_healthy, delta = 25)

# phq stratified by sleep duration overweight ----------------------
clr_acc_mhq_sleep_q1_overweight <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)
clr_acc_mhq_sleep_q2_overweight <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)]& bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)
clr_acc_mhq_sleep_q3_overweight <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "over"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          sbp = sbp,
                                          total = 1440)

m_phq_gam_sleep_q1_overweight <- brmcoda(clr_acc_mhq_sleep_q1_overweight,
                                         phq ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_overweight, paste0(outputdir, "m_phq_gam_sleep_q1_overweight", ".RDS"))

m_phq_gam_sleep_q2_overweight <- brmcoda(clr_acc_mhq_sleep_q2_overweight,
                                         phq ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_overweight, paste0(outputdir, "m_phq_gam_sleep_q2_overweight", ".RDS"))

m_phq_gam_sleep_q3_overweight <- brmcoda(clr_acc_mhq_sleep_q3_overweight,
                                         phq ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_overweight, paste0(outputdir, "m_phq_gam_sleep_q3_overweight", ".RDS"))

m_phq_gam_sub_sleep_q1_overweight <- substitution(
  m_phq_gam_sleep_q1_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_q1_overweight", ".RDS"))

m_phq_gam_sub_sleep_q2_overweight <- substitution(
  m_phq_gam_sleep_q2_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_q2_overweight", ".RDS"))

m_phq_gam_sub_sleep_q3_overweight <- substitution(
  m_phq_gam_sleep_q3_overweight,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_q3_overweight", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 25)
summary(m_phq_gam_sub_sleep_q1_overweight, delta = 25)

summary(m_phq_gam_sub_sleep_q2, delta = 25)
summary(m_phq_gam_sub_sleep_q2_overweight, delta = 25)

summary(m_phq_gam_sub_sleep_q3, delta = 25)
summary(m_phq_gam_sub_sleep_q3_overweight, delta = 25)

# phq stratified by sleep duration obese ----------------------
clr_acc_mhq_sleep_q1_obese <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)
clr_acc_mhq_sleep_q2_obese <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)
clr_acc_mhq_sleep_q3_obese <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & bmig == "obese"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)

m_phq_gam_sleep_q1_obese <- brmcoda(clr_acc_mhq_sleep_q1_obese,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_obese, paste0(outputdir, "m_phq_gam_sleep_q1_obese", ".RDS"))

m_phq_gam_sleep_q2_obese <- brmcoda(clr_acc_mhq_sleep_q2_obese,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_obese, paste0(outputdir, "m_phq_gam_sleep_q2_obese", ".RDS"))

m_phq_gam_sleep_q3_obese <- brmcoda(clr_acc_mhq_sleep_q3_obese,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_obese, paste0(outputdir, "m_phq_gam_sleep_q3_obese", ".RDS"))

m_phq_gam_sub_sleep_q1_obese <- substitution(
  m_phq_gam_sleep_q1_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_obese, paste0(outputdir, "m_phq_gam_sub_sleep_q1_obese", ".RDS"))

m_phq_gam_sub_sleep_q2_obese <- substitution(
  m_phq_gam_sleep_q2_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_obese, paste0(outputdir, "m_phq_gam_sub_sleep_q2_obese", ".RDS"))

m_phq_gam_sub_sleep_q3_obese <- substitution(
  m_phq_gam_sleep_q3_obese,
  delta = 1:18,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_obese, paste0(outputdir, "m_phq_gam_sub_sleep_q3_obese", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 20)
summary(m_phq_gam_sub_sleep_q1_obese, delta = 20)

summary(m_phq_gam_sub_sleep_q2, delta = 20)
summary(m_phq_gam_sub_sleep_q2_obese, delta = 20)

summary(m_phq_gam_sub_sleep_q3, delta = 20)
summary(m_phq_gam_sub_sleep_q3_obese, delta = 20)

# gad stratified by sleep duration and bmi healthy ----------------------
m_gad_gam_sleep_q1_healthy <- brmcoda(clr_acc_mhq_sleep_q1_healthy,
                                      gad ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1_healthy, paste0(outputdir, "m_gad_gam_sleep_q1_healthy", ".RDS"))

m_gad_gam_sleep_q2_healthy <- brmcoda(clr_acc_mhq_sleep_q2_healthy,
                                      gad ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2_healthy, paste0(outputdir, "m_gad_gam_sleep_q2_healthy", ".RDS"))

m_gad_gam_sleep_q3_healthy <- brmcoda(clr_acc_mhq_sleep_q3_healthy,
                                      gad ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3_healthy, paste0(outputdir, "m_gad_gam_sleep_q3_healthy", ".RDS"))

m_gad_gam_sub_sleep_q1_healthy <- substitution(
  m_gad_gam_sleep_q1_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_healthy, paste0(outputdir, "m_gad_gam_sub_sleep_q1_healthy", ".RDS"))

m_gad_gam_sub_sleep_q2_healthy <- substitution(
  m_gad_gam_sleep_q2_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_healthy, paste0(outputdir, "m_gad_gam_sub_sleep_q2_healthy", ".RDS"))

m_gad_gam_sub_sleep_q3_healthy <- substitution(
  m_gad_gam_sleep_q3_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_healthy, paste0(outputdir, "m_gad_gam_sub_sleep_q3_healthy", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 25)
summary(m_gad_gam_sub_sleep_q1_healthy, delta = 25)

summary(m_gad_gam_sub_sleep_q2, delta = 25)
summary(m_gad_gam_sub_sleep_q2_healthy, delta = 25)

summary(m_gad_gam_sub_sleep_q3, delta = 25)
summary(m_gad_gam_sub_sleep_q3_healthy, delta = 25)

# gad stratified by sleep duration overweight ----------------------
m_gad_gam_sleep_q1_overweight <- brmcoda(clr_acc_mhq_sleep_q1_overweight,
                                         gad ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1_overweight, paste0(outputdir, "m_gad_gam_sleep_q1_overweight", ".RDS"))

m_gad_gam_sleep_q2_overweight <- brmcoda(clr_acc_mhq_sleep_q2_overweight,
                                         gad ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2_overweight, paste0(outputdir, "m_gad_gam_sleep_q2_overweight", ".RDS"))

m_gad_gam_sleep_q3_overweight <- brmcoda(clr_acc_mhq_sleep_q3_overweight,
                                         gad ~ 
                                           s(ilr1) + s(ilr2) + s(ilr3) +
                                           age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                         chains = 4, cores = 4,
                                         backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3_overweight, paste0(outputdir, "m_gad_gam_sleep_q3_overweight", ".RDS"))

m_gad_gam_sub_sleep_q1_overweight <- substitution(
  m_gad_gam_sleep_q1_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_overweight, paste0(outputdir, "m_gad_gam_sub_sleep_q1_overweight", ".RDS"))

m_gad_gam_sub_sleep_q2_overweight <- substitution(
  m_gad_gam_sleep_q2_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_overweight, paste0(outputdir, "m_gad_gam_sub_sleep_q2_overweight", ".RDS"))

m_gad_gam_sub_sleep_q3_overweight <- substitution(
  m_gad_gam_sleep_q3_overweight,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_overweight, paste0(outputdir, "m_gad_gam_sub_sleep_q3_overweight", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 25)
summary(m_gad_gam_sub_sleep_q1_overweight, delta = 25)

summary(m_gad_gam_sub_sleep_q2, delta = 25)
summary(m_gad_gam_sub_sleep_q2_overweight, delta = 25)

summary(m_gad_gam_sub_sleep_q3, delta = 25)
summary(m_gad_gam_sub_sleep_q3_overweight, delta = 25)

# gad stratified by sleep duration obese ----------------------
m_gad_gam_sleep_q1_obese <- brmcoda(clr_acc_mhq_sleep_q1_obese,
                                    gad ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1_obese, paste0(outputdir, "m_gad_gam_sleep_q1_obese", ".RDS"))

m_gad_gam_sleep_q2_obese <- brmcoda(clr_acc_mhq_sleep_q2_obese,
                                    gad ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2_obese, paste0(outputdir, "m_gad_gam_sleep_q2_obese", ".RDS"))

m_gad_gam_sleep_q3_obese <- brmcoda(clr_acc_mhq_sleep_q3_obese,
                                    gad ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3_obese, paste0(outputdir, "m_gad_gam_sleep_q3_obese", ".RDS"))

m_gad_gam_sub_sleep_q1_obese <- substitution(
  m_gad_gam_sleep_q1_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_obese, paste0(outputdir, "m_gad_gam_sub_sleep_q1_obese", ".RDS"))

m_gad_gam_sub_sleep_q2_obese <- substitution(
  m_gad_gam_sleep_q2_obese,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_obese, paste0(outputdir, "m_gad_gam_sub_sleep_q2_obese", ".RDS"))

m_gad_gam_sub_sleep_q3_obese <- substitution(
  m_gad_gam_sleep_q3_obese,
  delta = 1:18,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_obese, paste0(outputdir, "m_gad_gam_sub_sleep_q3_obese", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 20)
summary(m_gad_gam_sub_sleep_q1_obese, delta = 20)

summary(m_gad_gam_sub_sleep_q2, delta = 20)
summary(m_gad_gam_sub_sleep_q2_obese, delta = 20)

summary(m_gad_gam_sub_sleep_q3, delta = 20)
summary(m_gad_gam_sub_sleep_q3_obese, delta = 20)

# LIFETIME MENTAL HEALTH ----------------------------
# phq stratified by sleep duration and lifetime MH ----------------------
clr_acc_mhq_sleep_q1_mh1 <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & (dep_lifetime == 1 | anx_lifetime == 1)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)
clr_acc_mhq_sleep_q2_mh1 <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & (dep_lifetime == 1 | anx_lifetime == 1)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)
clr_acc_mhq_sleep_q3_mh1 <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & (dep_lifetime == 1 | anx_lifetime == 1)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)

m_phq_gam_sleep_q1_mh1 <- brmcoda(clr_acc_mhq_sleep_q1_mh1,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_mh1, paste0(outputdir, "m_phq_gam_sleep_q1_mh1", ".RDS"))

m_phq_gam_sleep_q2_mh1 <- brmcoda(clr_acc_mhq_sleep_q2_mh1,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_mh1, paste0(outputdir, "m_phq_gam_sleep_q2_mh1", ".RDS"))

m_phq_gam_sleep_q3_mh1 <- brmcoda(clr_acc_mhq_sleep_q3_mh1,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_mh1, paste0(outputdir, "m_phq_gam_sleep_q3_mh1", ".RDS"))

m_phq_gam_sub_sleep_q1_mh1 <- substitution(
  m_phq_gam_sleep_q1_mh1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_mh1, paste0(outputdir, "m_phq_gam_sub_sleep_q1_mh1", ".RDS"))

m_phq_gam_sub_sleep_q2_mh1 <- substitution(
  m_phq_gam_sleep_q2_mh1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_mh1, paste0(outputdir, "m_phq_gam_sub_sleep_q2_mh1", ".RDS"))

m_phq_gam_sub_sleep_q3_mh1 <- substitution(
  m_phq_gam_sleep_q3_mh1,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_mh1, paste0(outputdir, "m_phq_gam_sub_sleep_q3_mh1", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 20)
summary(m_phq_gam_sub_sleep_q1_mh1, delta = 20)

summary(m_phq_gam_sub_sleep_q2, delta = 20)
summary(m_phq_gam_sub_sleep_q2_mh1, delta = 20)

summary(m_phq_gam_sub_sleep_q3, delta = 20)
summary(m_phq_gam_sub_sleep_q3_mh1, delta = 20)

# phq stratified by sleep duration and wo lifetime MH ----------------------
clr_acc_mhq_sleep_q1_mh0 <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & (dep_lifetime == 0 & anx_lifetime == 0)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)
clr_acc_mhq_sleep_q2_mh0 <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & (dep_lifetime == 0 & anx_lifetime == 0)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)
clr_acc_mhq_sleep_q3_mh0 <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & (dep_lifetime == 0 & anx_lifetime == 0)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)

m_phq_gam_sleep_q1_mh0 <- brmcoda(clr_acc_mhq_sleep_q1_mh0,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q1_mh0, paste0(outputdir, "m_phq_gam_sleep_q1_mh0", ".RDS"))

m_phq_gam_sleep_q2_mh0 <- brmcoda(clr_acc_mhq_sleep_q2_mh0,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q2_mh0, paste0(outputdir, "m_phq_gam_sleep_q2_mh0", ".RDS"))

m_phq_gam_sleep_q3_mh0 <- brmcoda(clr_acc_mhq_sleep_q3_mh0,
                                  phq ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_q3_mh0, paste0(outputdir, "m_phq_gam_sleep_q3_mh0", ".RDS"))

m_phq_gam_sub_sleep_q1_mh0 <- substitution(
  m_phq_gam_sleep_q1_mh0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q1_mh0, paste0(outputdir, "m_phq_gam_sub_sleep_q1_mh0", ".RDS"))

m_phq_gam_sub_sleep_q2_mh0 <- substitution(
  m_phq_gam_sleep_q2_mh0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q2_mh0, paste0(outputdir, "m_phq_gam_sub_sleep_q2_mh0", ".RDS"))

m_phq_gam_sub_sleep_q3_mh0 <- substitution(
  m_phq_gam_sleep_q3_mh0,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_q3_mh0, paste0(outputdir, "m_phq_gam_sub_sleep_q3_mh0", ".RDS"))

summary(m_phq_gam_sub_sleep_q1, delta = 20)
summary(m_phq_gam_sub_sleep_q1_mh1, delta = 20)
summary(m_phq_gam_sub_sleep_q1_mh0, delta = 20)

summary(m_phq_gam_sub_sleep_q2, delta = 20)
summary(m_phq_gam_sub_sleep_q2_mh1, delta = 20)
summary(m_phq_gam_sub_sleep_q2_mh0, delta = 20)

summary(m_phq_gam_sub_sleep_q3, delta = 20)
summary(m_phq_gam_sub_sleep_q3_mh1, delta = 20)
summary(m_phq_gam_sub_sleep_q3_mh0, delta = 20)

# gad stratified by sleep duration and lifetime MH ----------------------
m_gad_gam_sleep_q1_mh1 <- brmcoda(clr_acc_mhq_sleep_q1_mh1,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1_mh1, paste0(outputdir, "m_gad_gam_sleep_q1_mh1", ".RDS"))

m_gad_gam_sleep_q2_mh1 <- brmcoda(clr_acc_mhq_sleep_q2_mh1,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2_mh1, paste0(outputdir, "m_gad_gam_sleep_q2_mh1", ".RDS"))

m_gad_gam_sleep_q3_mh1 <- brmcoda(clr_acc_mhq_sleep_q3_mh1,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3_mh1, paste0(outputdir, "m_gad_gam_sleep_q3_mh1", ".RDS"))

m_gad_gam_sub_sleep_q1_mh1 <- substitution(
  m_gad_gam_sleep_q1_mh1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_mh1, paste0(outputdir, "m_gad_gam_sub_sleep_q1_mh1", ".RDS"))

m_gad_gam_sub_sleep_q2_mh1 <- substitution(
  m_gad_gam_sleep_q2_mh1,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_mh1, paste0(outputdir, "m_gad_gam_sub_sleep_q2_mh1", ".RDS"))

m_gad_gam_sub_sleep_q3_mh1 <- substitution(
  m_gad_gam_sleep_q3_mh1,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_mh1, paste0(outputdir, "m_gad_gam_sub_sleep_q3_mh1", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 20)
summary(m_gad_gam_sub_sleep_q1_mh1, delta = 20)

summary(m_gad_gam_sub_sleep_q2, delta = 20)
summary(m_gad_gam_sub_sleep_q2_mh1, delta = 20)

summary(m_gad_gam_sub_sleep_q3, delta = 20)
summary(m_gad_gam_sub_sleep_q3_mh1, delta = 20)

# gad stratified by sleep duration and wo lifetime MH ----------------------
m_gad_gam_sleep_q1_mh0 <- brmcoda(clr_acc_mhq_sleep_q1_mh0,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q1_mh0, paste0(outputdir, "m_gad_gam_sleep_q1_mh0", ".RDS"))

m_gad_gam_sleep_q2_mh0 <- brmcoda(clr_acc_mhq_sleep_q2_mh0,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q2_mh0, paste0(outputdir, "m_gad_gam_sleep_q2_mh0", ".RDS"))

m_gad_gam_sleep_q3_mh0 <- brmcoda(clr_acc_mhq_sleep_q3_mh0,
                                  gad ~ 
                                    s(ilr1) + s(ilr2) + s(ilr3) +
                                    age + sex + white + working + edu + never_smoked + current_drinker + deprivation, 
                                  chains = 4, cores = 4,
                                  backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_q3_mh0, paste0(outputdir, "m_gad_gam_sleep_q3_mh0", ".RDS"))

m_gad_gam_sub_sleep_q1_mh0 <- substitution(
  m_gad_gam_sleep_q1_mh0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q1_mh0, paste0(outputdir, "m_gad_gam_sub_sleep_q1_mh0", ".RDS"))

m_gad_gam_sub_sleep_q2_mh0 <- substitution(
  m_gad_gam_sleep_q2_mh0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q2_mh0, paste0(outputdir, "m_gad_gam_sub_sleep_q2_mh0", ".RDS"))

m_gad_gam_sub_sleep_q3_mh0 <- substitution(
  m_gad_gam_sleep_q3_mh0,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_q3_mh0, paste0(outputdir, "m_gad_gam_sub_sleep_q3_mh0", ".RDS"))

summary(m_gad_gam_sub_sleep_q1, delta = 20)
summary(m_gad_gam_sub_sleep_q1_mh1, delta = 20)
summary(m_gad_gam_sub_sleep_q1_mh0, delta = 20)

summary(m_gad_gam_sub_sleep_q2, delta = 20)
summary(m_gad_gam_sub_sleep_q2_mh1, delta = 20)
summary(m_gad_gam_sub_sleep_q2_mh0, delta = 20)

summary(m_gad_gam_sub_sleep_q3, delta = 20)
summary(m_gad_gam_sub_sleep_q3_mh1, delta = 20)
summary(m_gad_gam_sub_sleep_q3_mh0, delta = 20)
