
source(paste0(redir, "ukb_utils.R"))
source(paste0(redir, "data/data_mhq.R"))
source(paste0(redir, "data/data_acc.R"))
source(paste0(redir, "data/data_demographics.R"))

d_acc_mhq <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(d_acc, d_mhq, dm))

# subset only good data
d_acc_mhq <- d_acc_mhq[acc_data_quality == "Yes"]
d_acc_mhq <- d_acc_mhq[, -colnames(ilr_acc), with = FALSE]


clr_acc_mhq <- complr(data = d_acc_mhq,
                      transform = "ilr",
                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                      sbp = sbp,
                      total = 1440)
saveRDS(clr_acc_mhq, paste0(inputdir, "clr_acc_mhq", ".RDS"))

# stratifed by sleep quantile
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c (0, 0.1, 0.9, 1))))
quantile_sleep <- quantile(clr_acc_mhq$data$sleep, c (0, 0.1, 0.9, 1))

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

saveRDS(clr_acc_mhq_sleep_q1, paste0(inputdir, "clr_acc_mhq_sleep_q1", ".RDS"))
saveRDS(clr_acc_mhq_sleep_q2, paste0(inputdir, "clr_acc_mhq_sleep_q2", ".RDS"))
saveRDS(clr_acc_mhq_sleep_q3, paste0(inputdir, "clr_acc_mhq_sleep_q3", ".RDS"))

# stratified by sleep duration
nrow(d_acc_mhq[sleep <= 7/24])
nrow(d_acc_mhq[sleep >= 9/24])
nrow(d_acc_mhq[sleep %gl% c(7/24, 9/24)])

clr_acc_mhq_sleep_short <- complr(data = d_acc_mhq[sleep <= 7/24],
                                  transform = "ilr",
                                  parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                  sbp = sbp,
                                  total = 1440)
clr_acc_mhq_sleep_normal <- complr(data = d_acc_mhq[sleep %gl% c(7/24, 9/24)],
                                   transform = "ilr",
                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                   sbp = sbp,
                                   total = 1440)
clr_acc_mhq_sleep_long <- complr(data = d_acc_mhq[sleep >= 9/24],
                                 transform = "ilr",
                                 parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                 sbp = sbp,
                                 total = 1440)

saveRDS(clr_acc_mhq_sleep_short, paste0(inputdir, "clr_acc_mhq_sleep_short", ".RDS"))
saveRDS(clr_acc_mhq_sleep_normal, paste0(inputdir, "clr_acc_mhq_sleep_normal", ".RDS"))
saveRDS(clr_acc_mhq_sleep_long, paste0(inputdir, "clr_acc_mhq_sleep_long", ".RDS"))

