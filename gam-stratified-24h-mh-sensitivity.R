# stratified by mvpa quantile -------------------------
table(cut(clr_acc_mhq$data$mvpa, quantile(clr_acc_mhq$data$mvpa, c (0, 1/3, 2/3, 1))))
quantile_mvpa <- quantile(clr_acc_mhq$data$mvpa, c (0, 1/3, 2/3, 1))

clr_acc_mhq_mvpa_q1 <- complr(data = d_acc_mhq[mvpa <= quantile_mvpa[[2]]],
                              transform = "ilr",
                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                              sbp = sbp,
                              total = 1440)
clr_acc_mhq_mvpa_q2 <- complr(data = d_acc_mhq[mvpa %gl% quantile_mvpa[c(2:3)]],
                              transform = "ilr",
                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                              sbp = sbp,
                              total = 1440)
clr_acc_mhq_mvpa_q3 <- complr(data = d_acc_mhq[mvpa >= quantile_mvpa[[3]]],
                              transform = "ilr",
                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                              sbp = sbp,
                              total = 1440)

saveRDS(clr_acc_mhq_mvpa_q1, paste0(inputdir, "clr_acc_mhq_mvpa_q1", ".RDS"))
saveRDS(clr_acc_mhq_mvpa_q2, paste0(inputdir, "clr_acc_mhq_mvpa_q2", ".RDS"))
saveRDS(clr_acc_mhq_mvpa_q3, paste0(inputdir, "clr_acc_mhq_mvpa_q3", ".RDS"))

m_phq_gam_sub_mvpa_q1 <- substitution(
  m_phq_gam_mvpa_q1,
  delta = 1:10,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_mvpa_q1, paste0(outputdir, "m_phq_gam_sub_mvpa_q1", ".RDS"))

m_phq_gam_sub_mvpa_q2 <- substitution(
  m_phq_gam_mvpa_q2,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_mvpa_q2, paste0(outputdir, "m_phq_gam_sub_mvpa_q2", ".RDS"))

m_phq_gam_sub_mvpa_q3 <- substitution(
  m_phq_gam_mvpa_q3,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_mvpa_q3, paste0(outputdir, "m_phq_gam_sub_mvpa_q3", ".RDS"))

resp_g         <- c("Depressive Symptoms - MVPA Q1", "Depressive Symptoms - MVPA Q2", "Depressive Symptoms - MVPA Q3"
                    # "Anxiety Symptoms - Sleep Short", "Anxiety Symptoms - Sleep Normal", "Anxiety Symptoms - Sleep Long",
                    # "Wellbeing - Sleep Short", "Wellbeing - Sleep Normal", "Wellbeing - Sleep Long",
                    # "Lifetime Depression - Sleep Short", "Lifetime Depression - Sleep Normal", "Lifetime Depression - Sleep Long",
                    # "Lifetime Anxiety - Sleep Short", "Lifetime Anxiety - Sleep Normal", "Lifetime Anxiety - Sleep Long",
                    # "Lifetime Psychosis - Sleep Short", "Lifetime Psychosis - Sleep Normal", "Lifetime Psychosis - Sleep Long"
)

# stratified by sleep duration health bmi ----------------------
clr_acc_mhq_sleep_short_healthy <- complr(data = d_acc_mhq[sleep <= 7/24 & bmi < 25],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_normal_healthy <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24)& bmi < 25],
                                              transform = "ilr",
                                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                              sbp = sbp,
                                              total = 1440)
clr_acc_mhq_sleep_long_healthy <- complr(data = d_acc_mhq[sleep >= 9/24 & bmi < 25],
                                            transform = "ilr",
                                            parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                            sbp = sbp,
                                            total = 1440)


m_phq_gam_sleep_short_healthy <- brmcoda(clr_acc_mhq_sleep_short_healthy,
                                            phq ~ 
                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                              age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                            chains = 4, cores = 4,
                                            backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short_healthy, paste0(outputdir, "m_phq_gam_sleep_short_healthy", ".RDS"))

m_phq_gam_sleep_normal_healthy <- brmcoda(clr_acc_mhq_sleep_normal_healthy,
                                             phq ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal_healthy, paste0(outputdir, "m_phq_gam_sleep_normal_healthy", ".RDS"))

m_phq_gam_sleep_long_healthy <- brmcoda(clr_acc_mhq_sleep_long_healthy,
                                           phq ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long_healthy, paste0(outputdir, "m_phq_gam_sleep_long_healthy", ".RDS"))

m_phq_gam_sub_sleep_short_healthy <- substitution(
  m_phq_gam_sleep_short_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_short_healthy", ".RDS"))

m_phq_gam_sub_sleep_normal_healthy <- substitution(
  m_phq_gam_sleep_normal_healthy,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_normal_healthy", ".RDS"))

m_phq_gam_sub_sleep_long_healthy <- substitution(
  m_phq_gam_sleep_long_healthy,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long_healthy, paste0(outputdir, "m_phq_gam_sub_sleep_long_healthy", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 25)
summary(m_phq_gam_sub_sleep_short_healthy, delta = 25)

summary(m_phq_gam_sub_sleep_normal, delta = 25)
summary(m_phq_gam_sub_sleep_normal_healthy, delta = 25)

summary(m_phq_gam_sub_sleep_long, delta = 25)
summary(m_phq_gam_sub_sleep_long_healthy, delta = 25)

# stratified by sleep duration overweight ----------------------
nrow(d_acc_mhq[bmi> 25])

clr_acc_mhq_sleep_short_overweight <- complr(data = d_acc_mhq[sleep <= 7/24 & bmi %gele% c(25, 30)],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_normal_overweight <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24)& bmi %gele% c(25, 30)],
                                              transform = "ilr",
                                              parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                              sbp = sbp,
                                              total = 1440)
clr_acc_mhq_sleep_long_overweight <- complr(data = d_acc_mhq[sleep >= 9/24 & bmi %gele% c(25, 30)],
                                            transform = "ilr",
                                            parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                            sbp = sbp,
                                            total = 1440)


m_phq_gam_sleep_short_overweight <- brmcoda(clr_acc_mhq_sleep_short_overweight,
                                            phq ~ 
                                              s(ilr1) + s(ilr2) + s(ilr3) +
                                              age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                            chains = 4, cores = 4,
                                            backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short_overweight, paste0(outputdir, "m_phq_gam_sleep_short_overweight", ".RDS"))

m_phq_gam_sleep_normal_overweight <- brmcoda(clr_acc_mhq_sleep_normal_overweight,
                                             phq ~ 
                                               s(ilr1) + s(ilr2) + s(ilr3) +
                                               age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                             chains = 4, cores = 4,
                                             backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal_overweight, paste0(outputdir, "m_phq_gam_sleep_normal_overweight", ".RDS"))

m_phq_gam_sleep_long_overweight <- brmcoda(clr_acc_mhq_sleep_long_overweight,
                                           phq ~ 
                                             s(ilr1) + s(ilr2) + s(ilr3) +
                                             age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                           chains = 4, cores = 4,
                                           backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long_overweight, paste0(outputdir, "m_phq_gam_sleep_long_overweight", ".RDS"))

m_phq_gam_sub_sleep_short_overweight <- substitution(
  m_phq_gam_sleep_short_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_short_overweight", ".RDS"))

m_phq_gam_sub_sleep_normal_overweight <- substitution(
  m_phq_gam_sleep_normal_overweight,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_normal_overweight", ".RDS"))

m_phq_gam_sub_sleep_long_overweight <- substitution(
  m_phq_gam_sleep_long_overweight,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long_overweight, paste0(outputdir, "m_phq_gam_sub_sleep_long_overweight", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 25)
summary(m_phq_gam_sub_sleep_short_overweight, delta = 25)

summary(m_phq_gam_sub_sleep_normal, delta = 25)
summary(m_phq_gam_sub_sleep_normal_overweight, delta = 25)

summary(m_phq_gam_sub_sleep_long, delta = 25)
summary(m_phq_gam_sub_sleep_long_overweight, delta = 25)

# stratified by sleep duration obese ----------------------
nrow(d_acc_mhq[bmi> 30])

clr_acc_mhq_sleep_short_bmi30 <- complr(data = d_acc_mhq[sleep <= 7/24 & bmi > 30],
                                        transform = "ilr",
                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                        sbp = sbp,
                                        total = 1440)
clr_acc_mhq_sleep_normal_bmi30 <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24) & bmi > 30],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_long_bmi30 <- complr(data = d_acc_mhq[sleep >= 9/24 & bmi > 30],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)

m_phq_gam_sleep_short_bmi30 <- brmcoda(clr_acc_mhq_sleep_short_bmi30,
                                       phq ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short_bmi30, paste0(outputdir, "m_phq_gam_sleep_short_bmi30", ".RDS"))

m_phq_gam_sleep_normal_bmi30 <- brmcoda(clr_acc_mhq_sleep_normal_bmi30,
                                        phq ~ 
                                          s(ilr1) + s(ilr2) + s(ilr3) +
                                          age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                        chains = 4, cores = 4,
                                        backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal_bmi30, paste0(outputdir, "m_phq_gam_sleep_normal_bmi30", ".RDS"))

m_phq_gam_sleep_long_bmi30 <- brmcoda(clr_acc_mhq_sleep_long_bmi30,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long_bmi30, paste0(outputdir, "m_phq_gam_sleep_long_bmi30", ".RDS"))

m_phq_gam_sub_sleep_short_bmi30 <- substitution(
  m_phq_gam_sleep_short_bmi30,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short_bmi30, paste0(outputdir, "m_phq_gam_sub_sleep_short_bmi30", ".RDS"))

m_phq_gam_sub_sleep_normal_bmi30 <- substitution(
  m_phq_gam_sleep_normal_bmi30,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal_bmi30, paste0(outputdir, "m_phq_gam_sub_sleep_normal_bmi30", ".RDS"))

m_phq_gam_sub_sleep_long_bmi30 <- substitution(
  m_phq_gam_sleep_long_bmi30,
  delta = 1:20,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long_bmi30, paste0(outputdir, "m_phq_gam_sub_sleep_long_bmi30", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 20)
summary(m_phq_gam_sub_sleep_short_bmi30, delta = 20)

summary(m_phq_gam_sub_sleep_normal, delta = 20)
summary(m_phq_gam_sub_sleep_normal_bmi30, delta = 20)

summary(m_phq_gam_sub_sleep_long, delta = 20)
summary(m_phq_gam_sub_sleep_long_bmi30, delta = 20)

# phq stratified by sleep duration and MDD ----------------------
nrow(d_acc_mhq[dep_lifetime == 1])
nrow(d_acc_mhq[sleep <= 7/24 & dep_lifetime == 1])
nrow(d_acc_mhq[sleep %gl% c(7/24, 9/24) & dep_lifetime == 1])
nrow(d_acc_mhq[sleep >= 9/24 & dep_lifetime == 1])

clr_acc_mhq_sleep_short_mdd <- complr(data = d_acc_mhq[sleep <= 7/24 & dep_lifetime == 1],
                                      transform = "ilr",
                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                      sbp = sbp,
                                      total = 1440)
clr_acc_mhq_sleep_normal_mdd <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24) & dep_lifetime == 1],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_long_mdd <- complr(data = d_acc_mhq[sleep >= 9/24 & dep_lifetime == 1],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)

m_phq_gam_sleep_short_mdd <- brmcoda(clr_acc_mhq_sleep_short_mdd,
                                     phq ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                     chains = 4, cores = 4,
                                     backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short_mdd, paste0(outputdir, "m_phq_gam_sleep_short_mdd", ".RDS"))

m_phq_gam_sleep_normal_mdd <- brmcoda(clr_acc_mhq_sleep_normal_mdd,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal_mdd, paste0(outputdir, "m_phq_gam_sleep_normal_mdd", ".RDS"))

m_phq_gam_sleep_long_mdd <- brmcoda(clr_acc_mhq_sleep_long_mdd,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long_mdd, paste0(outputdir, "m_phq_gam_sleep_long_mdd", ".RDS"))

m_phq_gam_sub_sleep_short_mdd <- substitution(
  m_phq_gam_sleep_short_mdd,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short_mdd, paste0(outputdir, "m_phq_gam_sub_sleep_short_mdd", ".RDS"))

m_phq_gam_sub_sleep_normal_mdd <- substitution(
  m_phq_gam_sleep_normal_mdd,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal_mdd, paste0(outputdir, "m_phq_gam_sub_sleep_normal_mdd", ".RDS"))

m_phq_gam_sub_sleep_long_mdd <- substitution(
  m_phq_gam_sleep_long_mdd,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long_mdd, paste0(outputdir, "m_phq_gam_sub_sleep_long_mdd", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 20)
summary(m_phq_gam_sub_sleep_short_mdd, delta = 20)

summary(m_phq_gam_sub_sleep_normal, delta = 20)
summary(m_phq_gam_sub_sleep_normal_mdd, delta = 20)

summary(m_phq_gam_sub_sleep_long, delta = 20)
summary(m_phq_gam_sub_sleep_long_mdd, delta = 20)

# phq stratified by sleep duration and wo MDD ----------------------
nrow(d_acc_mhq[dep_lifetime == 0])
nrow(d_acc_mhq[sleep <= 7/24 & dep_lifetime == 0])
nrow(d_acc_mhq[sleep %gl% c(7/24, 9/24) & dep_lifetime == 0])
nrow(d_acc_mhq[sleep >= 9/24 & dep_lifetime == 0])

clr_acc_mhq_sleep_short_mdd0 <- complr(data = d_acc_mhq[sleep <= 7/24 & dep_lifetime == 0],
                                      transform = "ilr",
                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                      sbp = sbp,
                                      total = 1440)
clr_acc_mhq_sleep_normal_mdd0 <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24) & dep_lifetime == 0],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_long_mdd0 <- complr(data = d_acc_mhq[sleep >= 9/24 & dep_lifetime == 0],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)

m_phq_gam_sleep_short_mdd0 <- brmcoda(clr_acc_mhq_sleep_short_mdd0,
                                     phq ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                     chains = 4, cores = 4,
                                     backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_short_mdd0, paste0(outputdir, "m_phq_gam_sleep_short_mdd0", ".RDS"))

m_phq_gam_sleep_normal_mdd0 <- brmcoda(clr_acc_mhq_sleep_normal_mdd0,
                                      phq ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_normal_mdd0, paste0(outputdir, "m_phq_gam_sleep_normal_mdd0", ".RDS"))

m_phq_gam_sleep_long_mdd0 <- brmcoda(clr_acc_mhq_sleep_long_mdd0,
                                    phq ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_phq_gam_sleep_long_mdd0, paste0(outputdir, "m_phq_gam_sleep_long_mdd0", ".RDS"))

m_phq_gam_sub_sleep_short_mdd0 <- substitution(
  m_phq_gam_sleep_short_mdd0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_short_mdd0, paste0(outputdir, "m_phq_gam_sub_sleep_short_mdd0", ".RDS"))

m_phq_gam_sub_sleep_normal_mdd0 <- substitution(
  m_phq_gam_sleep_normal_mdd0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_normal_mdd0, paste0(outputdir, "m_phq_gam_sub_sleep_normal_mdd0", ".RDS"))

m_phq_gam_sub_sleep_long_mdd0 <- substitution(
  m_phq_gam_sleep_long_mdd0,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_phq_gam_sub_sleep_long_mdd0, paste0(outputdir, "m_phq_gam_sub_sleep_long_mdd0", ".RDS"))

summary(m_phq_gam_sub_sleep_short, delta = 20)
summary(m_phq_gam_sub_sleep_short_mdd, delta = 20)
summary(m_phq_gam_sub_sleep_short_mdd0, delta = 20)

summary(m_phq_gam_sub_sleep_normal, delta = 20)
summary(m_phq_gam_sub_sleep_normal_mdd, delta = 20)
summary(m_phq_gam_sub_sleep_normal_mdd0, delta = 20)

summary(m_phq_gam_sub_sleep_long, delta = 20)
summary(m_phq_gam_sub_sleep_long_mdd, delta = 20)
summary(m_phq_gam_sub_sleep_long_mdd0, delta = 20)

# gad stratified by sleep duration and ANX ----------------------
nrow(d_acc_mhq[anx_lifetime == 1])
nrow(d_acc_mhq[anx_lifetime == 0])

nrow(d_acc_mhq[sleep <= 7/24 & anx_lifetime == 1])
nrow(d_acc_mhq[sleep %gl% c(7/24, 9/24) & anx_lifetime == 1])
nrow(d_acc_mhq[sleep >= 9/24 & anx_lifetime == 1])

clr_acc_mhq_sleep_short_anx <- complr(data = d_acc_mhq[sleep <= 7/24 & anx_lifetime == 1],
                                      transform = "ilr",
                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                      sbp = sbp,
                                      total = 1440)
clr_acc_mhq_sleep_normal_anx <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24) & anx_lifetime == 1],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_long_anx <- complr(data = d_acc_mhq[sleep >= 9/24 & anx_lifetime == 1],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     sbp = sbp,
                                     total = 1440)

m_gad_gam_sleep_short_anx <- brmcoda(clr_acc_mhq_sleep_short_anx,
                                     gad ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                     chains = 4, cores = 4,
                                     backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_short_anx, paste0(outputdir, "m_gad_gam_sleep_short_anx", ".RDS"))

m_gad_gam_sleep_normal_anx <- brmcoda(clr_acc_mhq_sleep_normal_anx,
                                      gad ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_normal_anx, paste0(outputdir, "m_gad_gam_sleep_normal_anx", ".RDS"))

m_gad_gam_sleep_long_anx <- brmcoda(clr_acc_mhq_sleep_long_anx,
                                    gad ~ 
                                      s(ilr1) + s(ilr2) + s(ilr3) +
                                      age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                    chains = 4, cores = 4,
                                    backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_long_anx, paste0(outputdir, "m_gad_gam_sleep_long_anx", ".RDS"))

m_gad_gam_sub_sleep_short_anx <- substitution(
  m_gad_gam_sleep_short_anx,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_short_anx, paste0(outputdir, "m_gad_gam_sub_sleep_short_anx", ".RDS"))

m_gad_gam_sub_sleep_normal_anx <- substitution(
  m_gad_gam_sleep_normal_anx,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_normal_anx, paste0(outputdir, "m_gad_gam_sub_sleep_normal_anx", ".RDS"))

m_gad_gam_sub_sleep_long_anx <- substitution(
  m_gad_gam_sleep_long_anx,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_long_anx, paste0(outputdir, "m_gad_gam_sub_sleep_long_anx", ".RDS"))

summary(m_gad_gam_sub_sleep_short, delta = 20)
summary(m_gad_gam_sub_sleep_short_anx, delta = 20)

summary(m_gad_gam_sub_sleep_normal, delta = 20)
summary(m_gad_gam_sub_sleep_normal_anx, delta = 20)

summary(m_gad_gam_sub_sleep_long, delta = 20)
summary(m_gad_gam_sub_sleep_long_anx, delta = 20)

# gad stratified by sleep duration and wo ANX ----------------------
nrow(d_acc_mhq[anx_lifetime == 0])
nrow(d_acc_mhq[sleep <= 7/24 & anx_lifetime == 0])
nrow(d_acc_mhq[sleep %gl% c(7/24, 9/24) & anx_lifetime == 0])
nrow(d_acc_mhq[sleep >= 9/24 & anx_lifetime == 0])

clr_acc_mhq_sleep_short_anx0 <- complr(data = d_acc_mhq[sleep <= 7/24 & anx_lifetime == 0],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       sbp = sbp,
                                       total = 1440)
clr_acc_mhq_sleep_normal_anx0 <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24) & anx_lifetime == 0],
                                        transform = "ilr",
                                        parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                        sbp = sbp,
                                        total = 1440)
clr_acc_mhq_sleep_long_anx0 <- complr(data = d_acc_mhq[sleep >= 9/24 & anx_lifetime == 0],
                                      transform = "ilr",
                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                      sbp = sbp,
                                      total = 1440)

m_gad_gam_sleep_short_anx0 <- brmcoda(clr_acc_mhq_sleep_short_anx0,
                                      gad ~ 
                                        s(ilr1) + s(ilr2) + s(ilr3) +
                                        age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                      chains = 4, cores = 4,
                                      backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_short_anx0, paste0(outputdir, "m_gad_gam_sleep_short_anx0", ".RDS"))

m_gad_gam_sleep_normal_anx0 <- brmcoda(clr_acc_mhq_sleep_normal_anx0,
                                       gad ~ 
                                         s(ilr1) + s(ilr2) + s(ilr3) +
                                         age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                       chains = 4, cores = 4,
                                       backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_normal_anx0, paste0(outputdir, "m_gad_gam_sleep_normal_anx0", ".RDS"))

m_gad_gam_sleep_long_anx0 <- brmcoda(clr_acc_mhq_sleep_long_anx0,
                                     gad ~ 
                                       s(ilr1) + s(ilr2) + s(ilr3) +
                                       age + sex + white + working + edu + smoking + alcohol + deprivation, 
                                     chains = 4, cores = 4,
                                     backend = "cmdstanr"
)
saveRDS(m_gad_gam_sleep_long_anx0, paste0(outputdir, "m_gad_gam_sleep_long_anx0", ".RDS"))

m_gad_gam_sub_sleep_short_anx0 <- substitution(
  m_gad_gam_sleep_short_anx0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_short_anx0, paste0(outputdir, "m_gad_gam_sub_sleep_short_anx0", ".RDS"))

m_gad_gam_sub_sleep_normal_anx0 <- substitution(
  m_gad_gam_sleep_normal_anx0,
  delta = 1:30,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_normal_anx0, paste0(outputdir, "m_gad_gam_sub_sleep_normal_anx0", ".RDS"))

m_gad_gam_sub_sleep_long_anx0 <- substitution(
  m_gad_gam_sleep_long_anx0,
  delta = 1:25,
  ref = "grandmean",
  level = "combined",
  cores = 4
)
saveRDS(m_gad_gam_sub_sleep_long_anx0, paste0(outputdir, "m_gad_gam_sub_sleep_long_anx0", ".RDS"))

summary(m_gad_gam_sub_sleep_short, delta = 20)
summary(m_gad_gam_sub_sleep_short_anx0, delta = 20)

summary(m_gad_gam_sub_sleep_normal, delta = 20)
summary(m_gad_gam_sub_sleep_normal_anx0, delta = 20)

summary(m_gad_gam_sub_sleep_long, delta = 20)
summary(m_gad_gam_sub_sleep_long_anx0, delta = 20)

