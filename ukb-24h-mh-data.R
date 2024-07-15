source("ukb-24h-mh-utils.R")
source("24h-mh-2016/24h-mh-2016-data.R")

# # all mh icd data
# d_icd_acc_mh <- readRDS(paste0(inputdir, "d_icd_acc_mh", ".RDS"))

# fo icd data -----------
d_acc_icd <- readRDS(paste0(inputdir, "d_acc_icd", ".RDS"))

icd_not_v_fo_vars <- c(
  "icd_ii_sub_fo", "icd_i_sub_fo", "icd_vii_sub_fo",
  "icd_viii_sub_fo", "icd_i_iii_sub_fo", "icd_xi_sub_fo", "icd_xiv_sub_fo", 
  "icd_iv_xiii_sub_fo",
  "icd_vi_sub_fo", "icd_x_sub_fo"
)
d_acc_icd[, icd_not_v_sub_fo := do.call(pmin, c(.SD, list(na.rm = TRUE))), .SDcols = icd_not_v_fo_vars]

icd_any_vars <- c(
  "icd_i_any", "icd_ii_any", "icd_ix_any", "icd_v_any",
  "icd_vii_any", "icd_viii_any", "icd_i_iii_any",
  "icd_xi_any", "icd_xiv_any", "icd_iv_xiii_any", "icd_vi_any",
  "icd_x_any"
)
d_acc_icd[, icd_any := ifelse((Reduce(`|`, lapply(icd_any_vars, function(v) f1(get(v), 1)))), 1, 0)]
table(d_acc_icd$icd_any, useNA = "always")

# censor 1 years to exclude from healthy --------
nrow(d_acc_icd[!is.na(icd_v_sub_fo)])
nrow(d_acc_icd[!is.na(icd_not_v_sub_fo)])

# time since first cancer diagnoses
## negative value means acc is before diagnosis
# d_acc_icd[, age_diff_cancer_acc := year(acc_startdate) - year(icd_ii_sub_fo)]
d_acc_icd[, time_diff_mh_acc := (acc_startdate - icd_v_sub_fo)/365.25]
# table(d_acc_icd$age_diff_cancer_acc, useNA = "always")
table(round(d_acc_icd$time_diff_mh_acc), useNA = "always")

# time since most recent other diagnoses
# d_acc_icd[, age_diff_other_cond_acc := year(acc_startdate) - year(icd_not_ii_sub_fo)]
d_acc_icd[, time_diff_not_mh_acc := (acc_startdate - icd_not_v_sub_fo)/365.25]
table(round(d_acc_icd$time_diff_not_mh_acc), useNA = "always")
# table(round(as.integer(as.Date("2015-03-02") - as.Date("1938-03-16"))/365.25))

# as.integer(as.Date("2015-03-02") - as.Date("1938-03-16"))/365.25
# table(round(as.integer(as.Date("2015-03-02") - as.Date("1938-03-16"))/365.25))

# exclude people with a mh diagnoses before to 1 year after acc
## ie include people with mh diagnoses after 1 year from acc
d_acc_icd <- d_acc_icd[time_diff_mh_acc < -1 | is.na(time_diff_mh_acc)]

# make variable of any other major health conditions at time (1 year after) of acc
d_acc_icd[, icd_not_mh := NA]
d_acc_icd[, icd_not_mh := ifelse(time_diff_not_mh_acc < -1 | icd_any == 0, 0, icd_not_mh)]
d_acc_icd[, icd_not_mh := ifelse(time_diff_not_mh_acc >= - 1, 1, icd_not_mh)]

table(d_acc_icd$icd_not_mh, useNA = "always")

# --------------
d_acc_icd <- d_acc_icd[, .(eid, time_diff_mh_acc, time_diff_not_mh_acc, icd_not_mh, icd_any)]

# merge
d_acc_mhq <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(d_acc_mhq, d_acc_icd))

# subset only good data
d_acc_mhq <- d_acc_mhq[acc_data_quality == "Yes"]
d_acc_mhq <- d_acc_mhq[, -colnames(ilr_acc), with = FALSE]

quantile_sleep <- quantile(clr_acc_mhq$data$sleep, c (0, 0.25, 0.75, 1))

# d_acc_mhq[, sleep_quantile := NA]
# d_acc_mhq[, sleep_quantile := ifelse(sleep <= quantile_sleep[[2]], "Q1", sleep_quantile)]
# d_acc_mhq[, sleep_quantile := ifelse(sleep %gl% quantile_sleep[c(2:3)], "Q2 and Q3", sleep_quantile)]
# d_acc_mhq[, sleep_quantile := ifelse(sleep >= quantile_sleep[[3]], "Q4", sleep_quantile)]
# 
# table(d_acc_mhq$sleep_quantile, useNA = "always")

# # composition and ilr ------------------------------------
## complr
clr_acc_mhq <- complr(data = d_acc_mhq,
                      transform = "ilr",
                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                      sbp = sbp,
                      total = 1440)
saveRDS(clr_acc_mhq, paste0(inputdir, "clr_acc_mhq", ".RDS"))

# stratifed by sleep quantile
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.1, 0.9, 1))))
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, seq(0, 1, 1/3))))


clr_acc_mhq_sleep_q1 <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               sbp = sbp,
                               total = 1440)
clr_acc_mhq_sleep_q2 <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               sbp = sbp,
                               total = 1440)
clr_acc_mhq_sleep_q3 <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]]],
                               transform = "ilr",
                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                               sbp = sbp,
                               total = 1440)

# saveRDS(clr_acc_mhq_sleep_q1, paste0(inputdir, "clr_acc_mhq_sleep_q1", ".RDS"))
# saveRDS(clr_acc_mhq_sleep_q2, paste0(inputdir, "clr_acc_mhq_sleep_q2", ".RDS"))
# saveRDS(clr_acc_mhq_sleep_q3, paste0(inputdir, "clr_acc_mhq_sleep_q3", ".RDS"))

# quantile - good sleep -----------------------
clr_acc_mhq_sleep_q1_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2016 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q2_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2016 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q3_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2016 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)

# quantile - mild insomnia_2016 -----------------------
clr_acc_mhq_sleep_q1_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2016 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2016 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2016 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

# quantile - moderate insomnia_2016 -----------------------
clr_acc_mhq_sleep_q1_insomnia_moderate <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2016 == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q2_insomnia_moderate <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2016 == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)
clr_acc_mhq_sleep_q3_insomnia_moderate <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2016 == "moderate"],
                                                 transform = "ilr",
                                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                 sbp = sbp,
                                                 total = 1440)

# quantile - severe insomnia_2016 -----------------------
clr_acc_mhq_sleep_q1_insomnia_severe <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2016 == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q2_insomnia_severe <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2016 == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)
clr_acc_mhq_sleep_q3_insomnia_severe <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2016 == "severe"],
                                               transform = "ilr",
                                               parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                               sbp = sbp,
                                               total = 1440)

# quantile - moderate + severe = persistent insomnia_2016 -----------------------
clr_acc_mhq_sleep_q1_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2016 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q2_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2016 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q3_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2016 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)

