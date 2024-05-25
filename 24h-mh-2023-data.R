source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))
source(paste0(redir, "data/data_mhq_2023.R"))
source(paste0(redir, "data/data_acc.R"))
source(paste0(redir, "data/data_demographics.R"))

d_acc_mhq <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(d_acc, d_mhq_2023, dm))

# subset only good data
d_acc_mhq <- d_acc_mhq[acc_data_quality == "Yes"]
d_acc_mhq <- d_acc_mhq[, -colnames(ilr_acc), with = FALSE]

# complr
clr_acc_mhq <- complr(data = d_acc_mhq,
                      transform = "ilr",
                      parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                      sbp = sbp,
                      total = 1440)

# stratifed by sleep quantile -------------------------
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.1, 0.9, 1))))
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, seq(0, 1, 1/3))))

quantile_sleep <- quantile(clr_acc_mhq$data$sleep, c (0, 0.25, 0.75, 1))

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

# quantile - good sleep -----------------------
clr_acc_mhq_sleep_q1_goodsleep <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q2_goodsleep <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)
clr_acc_mhq_sleep_q3_goodsleep <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "good sleep"],
                                         transform = "ilr",
                                         parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                         sbp = sbp,
                                         total = 1440)

# quantile - mild insomnia_2023 -----------------------
clr_acc_mhq_sleep_q1_insomnia_mild <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_mild <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_mild <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "mild"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

# quantile - moderate insomnia_2023 -----------------------
clr_acc_mhq_sleep_q1_insomnia_moderate <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "moderate"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_moderate <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "moderate"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_moderate <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "moderate"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

# quantile - severe insomnia_2023 -----------------------
clr_acc_mhq_sleep_q1_insomnia_severe <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 == "severe"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q2_insomnia_severe <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 == "severe"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)
clr_acc_mhq_sleep_q3_insomnia_severe <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 == "severe"],
                                             transform = "ilr",
                                             parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                             sbp = sbp,
                                             total = 1440)

# quantile - moderate + severe = persistent insomnia_2023 -----------------------
clr_acc_mhq_sleep_q1_insomnia_persistent <- complr(data = d_acc_mhq[sleep <= quantile_sleep[[2]] & insomnia_2023 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q2_insomnia_persistent <- complr(data = d_acc_mhq[sleep %gl% quantile_sleep[c(2:3)] & insomnia_2023 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)
clr_acc_mhq_sleep_q3_insomnia_persistent <- complr(data = d_acc_mhq[sleep >= quantile_sleep[[3]] & insomnia_2023 %in% c("moderate", "severe")],
                                                   transform = "ilr",
                                                   parts = c("sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp"),
                                                   sbp = sbp,
                                                   total = 1440)