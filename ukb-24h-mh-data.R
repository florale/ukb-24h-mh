source("ukb-24h-mh-setup.R")
source(paste0(redir, "ukb_utils.R"))
# source(paste0(redir, "data/data_mhq_2023.R"))
# # source(paste0(redir, "data/data_mhq_2016.R"))
# source(paste0(redir, "data/data_acc.R"))
# source(paste0(redir, "data/data_demographics.R"))
# 
# d_acc_mhq_all <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(d_acc, d_mhq_2023, dm))
# 
# # subset only good data
# d_acc_mhq_all <- d_acc_mhq_all[acc_data_quality == "Yes"]
# d_acc_mhq_all <- d_acc_mhq_all[, -colnames(ilr_acc), with = FALSE]
# 
# d_acc_mhq_all[, age_at_acc := year(acc_startdate) - year_birth]
# 
# # # n completed
# # nrow(d_acc_mhq_all[!is.na(p20400)]) # 66972
# # nrow(d_acc_mhq_all[!is.na(p28755)]) # 68331
# 
# # # all mh icd data
# d_icd_acc_mh <- readRDS(paste0(inputdir, "d_icd_acc_mh", ".RDS"))
# 
# # fo icd data -----------
# d_acc_icd <- readRDS(paste0(inputdir, "d_acc_icd", ".RDS"))
# 
# icd_not_v_fo_vars <- c(
#   "icd_ii_sub_fo", "icd_i_sub_fo", "icd_ix_sub_fo", "icd_vii_sub_fo",
#   "icd_viii_sub_fo", "icd_i_iii_sub_fo", "icd_xi_sub_fo", "icd_xiv_sub_fo", 
#   "icd_iv_xiii_sub_fo",
#   "icd_vi_sub_fo", "icd_x_sub_fo"
# )
# d_acc_icd[, icd_not_v_sub_fo := do.call(pmin, c(.SD, list(na.rm = TRUE))), .SDcols = icd_not_v_fo_vars]
# 
# icd_any_vars <- c(
#   "icd_i_any", "icd_ii_any", "icd_ix_any", "icd_v_any",
#   "icd_vii_any", "icd_viii_any", "icd_i_iii_any",
#   "icd_xi_any", "icd_xiv_any", "icd_iv_xiii_any", "icd_vi_any",
#   "icd_x_any"
# )
# d_acc_icd[, icd_any := ifelse((Reduce(`|`, lapply(icd_any_vars, function(v) f1(get(v), 1)))), 1, 0)]
# table(d_acc_icd$icd_any, useNA = "always")
# 
# icd_any_not_v_vars <- c(
#   "icd_i_any", "icd_ii_any", "icd_ix_any",
#   "icd_vii_any", "icd_viii_any", "icd_i_iii_any",
#   "icd_xi_any", "icd_xiv_any", "icd_iv_xiii_any", "icd_vi_any",
#   "icd_x_any"
# )
# d_acc_icd[, icd_not_v_any := ifelse((Reduce(`|`, lapply(icd_any_not_v_vars, function(v) f1(get(v), 1)))), 1, 0)]
# table(d_acc_icd$icd_not_v_any, useNA = "always")
# 
# icd_v_fo_vars <- grep("icd_v_.*_fo", names(d_acc_icd), value = TRUE)
# icd_v_vars <- str_remove(icd_v_fo_vars, "_fo")
# 
# icd_v_depressive_vars <- grep("F3[2-3]", icd_v_vars, perl = TRUE, value = T)
# d_acc_icd[, icd_v_depressive := ifelse(Reduce(`|`, lapply(icd_v_depressive_vars, function(v)
#   f1(get(v), 1))),
#   1, 0)]
# table(d_acc_icd$icd_v_depressive, useNA = "always")
# 
# icd_v_anxiety_vars <- grep("F4[0-1]", icd_v_vars, perl = TRUE, value = T)
# d_acc_icd[, icd_v_anxiety := ifelse(Reduce(`|`, lapply(icd_v_anxiety_vars, function(v)
#   f1(get(v), 1))),
#   1, 0)]
# table(d_acc_icd$icd_v_anxiety, useNA = "always")
# 
# # censor 1 years to exclude from healthy --------
# nrow(d_acc_icd[!is.na(icd_v_sub_fo)])
# nrow(d_acc_icd[!is.na(icd_not_v_sub_fo)])
# 
# icd_dep_anx_vars <- c(icd_v_depressive_vars, icd_v_anxiety_vars)
# icd_dep_anx_fo_vars <- paste0(icd_dep_anx_vars, "_fo")
# 
# d_acc_icd[, icd_v_dep_anx := ifelse(icd_v_anxiety == 1 | icd_v_depressive == 1, 1, 0)]
# d_acc_icd[, icd_dep_anx_fo := do.call(pmin, c(.SD, list(na.rm = TRUE))), .SDcols = icd_dep_anx_fo_vars]
# d_acc_icd[, time_diff_dep_anx_acc := (acc_startdate - icd_dep_anx_fo)/365.25]
# 
# d_acc_icd[, time_diff_any_conds_acc := (acc_startdate - icd_sub_fo)/365.25]
# table(round(d_acc_icd$time_diff_any_conds_acc), useNA = "always")
# 
# # make variable of any other major health conditions at time (1 year after) of acc
# d_acc_icd[, icd_any_at_acc := NA]
# d_acc_icd[, icd_any_at_acc := ifelse(time_diff_any_conds_acc > 0 & icd_any == 1, 1, icd_any_at_acc)]
# d_acc_icd[, icd_any_at_acc := ifelse(time_diff_any_conds_acc %gele% c(-1, 0) & icd_any == 1, NA, icd_any_at_acc)]
# d_acc_icd[, icd_any_at_acc := ifelse((time_diff_any_conds_acc < -1 & icd_any == 1) | (icd_any == 0) | (icd_any == 1 & time_diff_any_conds_acc < -1 & is.na(time_diff_any_conds_acc)), 0, icd_any_at_acc)]
# table(d_acc_icd$icd_any_at_acc, useNA = "always")
# 
# d_acc_dep_anx <- d_acc_icd[time_diff_dep_anx_acc >= -1]
# d_acc_icd     <- d_acc_icd[time_diff_dep_anx_acc < -1 | is.na(time_diff_dep_anx_acc)]
# 
# # final dataset--------------
# d_acc_icd <- d_acc_icd[, .(eid, icd_any_at_acc, icd_any, icd_v_any, icd_sub_fo, icd_v_dep_anx, icd_dep_anx_fo, time_diff_dep_anx_acc)]
# 
# # merge with mhq
# d_acc_mhq <- merge(d_acc_icd, d_acc_mhq_all, by = "eid", all.x = TRUE)
# d_acc_mhq <- d_acc_mhq[acc_data_quality == "Yes"]
# 
# # clinical threshold
# d_acc_mhq[, phq_2023_cutoff := cut(phq_2023,
#                                    breaks = c(0, 4, 9, 14, 19, 27),
#                                    labels = c("None",
#                                               "Mild",
#                                               "Moderate",
#                                               "Moderately severe",
#                                               "Severe"
#                                    ))]
# 
# d_acc_mhq[, gad_2023_cutoff := cut(gad_2023,
#                                    breaks = c(0, 4, 9, 14, 21),
#                                    labels = c("None",
#                                               "Mild",
#                                               "Moderate",
#                                               "Severe"
#                                    ))]
# 
# d_acc_mhq[, phq_2023_cutoff4 := cut(phq_2023,
#                                     breaks = c(0, 4, 9, 14),
#                                     labels = c("None",
#                                                "Mild",
#                                                "Moderately-to-severe"
#                                     ))]
# d_acc_mhq[, phq_2023_cutoff4 := relevel(phq_2023_cutoff4, ref = "None")]
# 
# d_acc_mhq[, gad_2023_cutoff4 := cut(gad_2023,
#                                     breaks = c(0, 4, 9, 14),
#                                     labels = c("None",
#                                                "Mild",
#                                                "Moderate-to-severe"
#                                     ))]
# d_acc_mhq[, gad_2023_cutoff4 := relevel(gad_2023_cutoff4, ref = "None")]
# 
# d_acc_mhq[, phq_2023_binary := NA]
# d_acc_mhq[, phq_2023_binary := ifelse(phq_2023 < 10, 0, phq_2023_binary)]
# d_acc_mhq[, phq_2023_binary := ifelse(phq_2023 >= 10, 1, phq_2023_binary)]
# 
# d_acc_mhq[, gad_2023_binary := NA]
# d_acc_mhq[, gad_2023_binary := ifelse(gad_2023 < 10, 0, gad_2023_binary)]
# d_acc_mhq[, gad_2023_binary := ifelse(gad_2023 >= 10, 1, gad_2023_binary)]
# 
# d_acc_mhq[, phq_2016_cutoff := cut(phq_2016,
#                                    breaks = c(0, 4, 9, 14, 19, 27),
#                                    labels = c("None",
#                                               "Mild",
#                                               "Moderate",
#                                               "Moderately severe",
#                                               "Severe"
#                                    ))]
# 
# d_acc_mhq[, gad_2016_cutoff := cut(gad_2016,
#                                    breaks = c(0, 4, 9, 14, 21),
#                                    labels = c("None",
#                                               "Mild",
#                                               "Moderate",
#                                               "Severe"
#                                    ))]
# 
# # complete data
# d_acc_mhq_clean <- d_acc_mhq[!is.na(p29197) & 
#                                !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol) | is.na(icd_any_at_acc))]
# 
# d_acc_mhq_clean[, sleep_group := NA]
# d_acc_mhq_clean[, sleep_group := ifelse(sleep <= quantile(sleep, c (0, 0.25, 0.75, 1))[[2]], "short", sleep_group)]
# d_acc_mhq_clean[, sleep_group := ifelse(sleep %gl% quantile(sleep, c (0, 0.25, 0.75, 1))[c(2:3)], "med", sleep_group)]
# d_acc_mhq_clean[, sleep_group := ifelse(sleep >= quantile(sleep, c (0, 0.25, 0.75, 1))[[3]], "long", sleep_group)]
# d_acc_mhq_clean[, sleep_group := factor(sleep_group, ordered = TRUE,
#                                         levels = c("short", "med", "long"))]
# 
# d_acc_mhq_clean[, insomnia_2023_3g := NA]
# d_acc_mhq_clean[, insomnia_2023_3g := ifelse(insomnia_2023 == "good sleep", "good sleep", insomnia_2023_3g)]
# d_acc_mhq_clean[, insomnia_2023_3g := ifelse(insomnia_2023 == "mild", "mild", insomnia_2023_3g)]
# d_acc_mhq_clean[, insomnia_2023_3g := ifelse(insomnia_2023 %in% c("moderate", "severe"), "moderate-severe", insomnia_2023_3g)]
# d_acc_mhq_clean[, insomnia_2023_3g := ifelse(insomnia_2023 == "good sleep", "good sleep", insomnia_2023_3g)]
# d_acc_mhq_clean[, insomnia_2023_3g := factor(insomnia_2023_3g, ordered = TRUE,
#                                              levels = c("good sleep", "mild", "moderate-severe"))]
# 
# d_acc_mhq_clean[, insomnia_2016_3g := NA]
# d_acc_mhq_clean[, insomnia_2016_3g := ifelse(insomnia_2016 == "good sleep", "good sleep", insomnia_2016_3g)]
# d_acc_mhq_clean[, insomnia_2016_3g := ifelse(insomnia_2016 == "mild", "mild", insomnia_2016_3g)]
# d_acc_mhq_clean[, insomnia_2016_3g := ifelse(insomnia_2016 %in% c("moderate", "severe"), "moderate-severe", insomnia_2016_3g)]
# d_acc_mhq_clean[, insomnia_2016_3g := ifelse(insomnia_2016 == "good sleep", "good sleep", insomnia_2016_3g)]
# d_acc_mhq_clean[, insomnia_2016_3g := factor(insomnia_2016_3g, ordered = TRUE,
#                                              levels = c("good sleep", "mild", "moderate-severe"))]
# saveRDS(d_acc_mhq_clean, paste0(inputdir, "d_acc_mhq_clean", ".RDS"))

d_acc_mhq_clean <- readRDS(paste0(inputdir, "d_acc_mhq_clean", ".RDS"))
quantile_sleep <- quantile(d_acc_mhq_clean$sleep, c (0, 0.25, 0.75, 1))

# composition and ilr ------------------------------------
## complr
clr_acc_mhq <- complr(data = d_acc_mhq_clean,
                      transform = "ilr",
                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                      # sbp = sbp,
                      total = 1440)
saveRDS(clr_acc_mhq, paste0(inputdir, "clr_acc_mhq", ".RDS"))

# stratifed by sleep quantile
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.1, 0.9, 1))))
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, seq(0, 1, 1/3))))

clr_acc_mhq_sleep_q1 <- complr(data = d_acc_mhq_clean[sleep <= quantile_sleep[[2]]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               # sbp = sbp,
                               total = 1440)
clr_acc_mhq_sleep_q2 <- complr(data = d_acc_mhq_clean[sleep %gl% quantile_sleep[c(2:3)]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               # sbp = sbp,
                               total = 1440)
clr_acc_mhq_sleep_q3 <- complr(data = d_acc_mhq_clean[sleep >= quantile_sleep[[3]]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               # sbp = sbp,
                               total = 1440)

clr_acc_mhq_goodsleep_bl <- complr(data = d_acc_mhq_clean[insomnia == "Never/rarely"],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   # sbp = sbp,
                                   total = 1440)
clr_acc_mhq_insomnia_mild_bl <- complr(data = d_acc_mhq_clean[insomnia ==  "Sometimes"],
                                       transform = "ilr",
                                       parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                       # sbp = sbp,
                                       total = 1440)
clr_acc_mhq_insomnia_persistent_bl <- complr(data = d_acc_mhq_clean[insomnia == "Usually"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             # sbp = sbp,
                                             total = 1440)

clr_acc_mhq_goodsleep_2016 <- complr(data = d_acc_mhq_clean[insomnia_2016 == "good sleep"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     # sbp = sbp,
                                     total = 1440)
clr_acc_mhq_insomnia_mild_2016  <- complr(data = d_acc_mhq_clean[insomnia_2016 == "mild"],
                                          transform = "ilr",
                                          parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                          # sbp = sbp,
                                          total = 1440)
clr_acc_mhq_insomnia_persistent_2016  <- complr(data = d_acc_mhq_clean[insomnia_2016 %in% c("moderate", "severe")],
                                                transform = "ilr",
                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                # sbp = sbp,
                                                total = 1440)

clr_acc_mhq_goodsleep_2023 <- complr(data = d_acc_mhq_clean[insomnia_2023 == "good sleep"],
                                     transform = "ilr",
                                     parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                     # sbp = sbp,
                                     total = 1440)
clr_acc_mhq_insomnia_mild_2023 <- complr(data = d_acc_mhq_clean[insomnia_2023 == "mild"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         # sbp = sbp,
                                         total = 1440)
clr_acc_mhq_insomnia_persistent_2023 <- complr(data = d_acc_mhq_clean[insomnia_2023 %in% c("moderate", "severe")],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               # sbp = sbp,
                                               total = 1440)

## baseline insomnia------
### quantile - good sleep -----------------------
clr_acc_mhq_sleep_q1_goodsleep_bl <- complr(data = d_acc_mhq_clean[sleep <= quantile_sleep[[2]] & insomnia == "Never/rarely"],
                                            transform = "ilr",
                                            parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                            # sbp = sbp,
                                            total = 1440)
clr_acc_mhq_sleep_q2_goodsleep_bl <- complr(data = d_acc_mhq_clean[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "Never/rarely"],
                                            transform = "ilr",
                                            parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                            # sbp = sbp,
                                            total = 1440)
clr_acc_mhq_sleep_q3_goodsleep_bl <- complr(data = d_acc_mhq_clean[sleep >= quantile_sleep[[3]] & insomnia == "Never/rarely"],
                                            transform = "ilr",
                                            parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                            # sbp = sbp,
                                            total = 1440)

### quantile - mild  -----------------------
clr_acc_mhq_sleep_q1_insomnia_mild_bl <- complr(data = d_acc_mhq_clean[sleep <= quantile_sleep[[2]] & insomnia == "Sometimes"],
                                                transform = "ilr",
                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                # sbp = sbp,
                                                total = 1440)
clr_acc_mhq_sleep_q2_insomnia_mild_bl <- complr(data = d_acc_mhq_clean[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "Sometimes"],
                                                transform = "ilr",
                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                # sbp = sbp,
                                                total = 1440)
clr_acc_mhq_sleep_q3_insomnia_mild_bl <- complr(data = d_acc_mhq_clean[sleep >= quantile_sleep[[3]] & insomnia == "Sometimes"],
                                                transform = "ilr",
                                                parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                # sbp = sbp,
                                                total = 1440)

### quantile - moderate + severe = persistent insomnia -----------------------
clr_acc_mhq_sleep_q1_insomnia_persistent_bl <- complr(data = d_acc_mhq_clean[sleep <= quantile_sleep[[2]] & insomnia == "Usually"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      # sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_sleep_q2_insomnia_persistent_bl <- complr(data = d_acc_mhq_clean[sleep %gl% quantile_sleep[c(2:3)] & insomnia == "Usually"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      # sbp = sbp,
                                                      total = 1440)
clr_acc_mhq_sleep_q3_insomnia_persistent_bl <- complr(data = d_acc_mhq_clean[sleep >= quantile_sleep[[3]] & insomnia == "Usually"],
                                                      transform = "ilr",
                                                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                      # sbp = sbp,
                                                      total = 1440)

## quantile insomnia -------------------
clr_acc_mhq_sleep_q1_insomnia_bl <- complr(data = d_acc_mhq_clean[sleep <= quantile_sleep[[2]] & insomnia %in% c("Sometimes", "Usually")],
                                           transform = "ilr",
                                           parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                           # sbp = sbp,
                                           total = 1440)
clr_acc_mhq_sleep_q2_insomnia_bl <- complr(data = d_acc_mhq_clean[sleep %gl% quantile_sleep[c(2:3)] & insomnia %in% c("Sometimes", "Usually")],
                                           transform = "ilr",
                                           parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                           # sbp = sbp,
                                           total = 1440)
clr_acc_mhq_sleep_q3_insomnia_bl <- complr(data = d_acc_mhq_clean[sleep >= quantile_sleep[[3]] & insomnia %in% c("Sometimes", "Usually")],
                                           transform = "ilr",
                                           parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                           # sbp = sbp,
                                           total = 1440)

# # sensitivity dataset--------------------
# d_acc_dep_anx <- d_acc_dep_anx[, .(eid, icd_any_at_acc, icd_any, icd_v_any, icd_sub_fo, icd_v_dep_anx, icd_dep_anx_fo, time_diff_dep_anx_acc)]
# 
# # merge with mhq
# d_acc_mhq_dep_anx <- merge(d_acc_dep_anx, d_acc_mhq_all, by = "eid", all.x = TRUE)
# d_acc_mhq_dep_anx <- d_acc_mhq_dep_anx[acc_data_quality == "Yes"]
# 
# # complete data
# d_acc_mhq_dep_anx_clean <- d_acc_mhq_dep_anx[!is.na(p29197) & 
#                                                !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))]
# 
# ### everyone has another condition
# table(d_acc_mhq_dep_anx$icd_any_at_acc, useNA = "always")
# 
# clr_acc_mhq_dep_anx <- complr(d_acc_mhq_dep_anx,
#                               transform = "ilr",
#                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
#                               # sbp = sbp,
#                               total = 1440)
# 
# # stratifed by sleep quantile
# table(cut(clr_acc_mhq_dep_anx$data$sleep, quantile(clr_acc_mhq_dep_anx$data$sleep, c(0, 0.25, 0.75, 1)))) #
# table(cut(clr_acc_mhq_dep_anx$data$sleep, quantile(clr_acc_mhq_dep_anx$data$sleep, c(0, 0.1, 0.9, 1))))
# table(cut(clr_acc_mhq_dep_anx$data$sleep, quantile(clr_acc_mhq_dep_anx$data$sleep, seq(0, 1, 1/3))))
# 
