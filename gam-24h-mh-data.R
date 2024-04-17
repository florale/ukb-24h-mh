source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))
source(paste0(redir, "data/data_mhq.R"))
source(paste0(redir, "data/data_acc.R"))
source(paste0(redir, "data/data_demographics.R"))

d_acc_mhq <- Reduce(function(x, y) merge(x, y, by = "eid", all = TRUE), list(d_acc, d_mhq, dm))

# subset only good data
d_acc_mhq <- d_acc_mhq[acc_data_quality == "Yes"]
d_acc_mhq <- d_acc_mhq[, -colnames(ilr_acc), with = FALSE]

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

quantile_sleep <- quantile(clr_acc_mhq$data$sleep, c (0, 0.25, 0.73, 1))

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


# descriptives

egltable(c("age", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", 
           "alcohol","current_drinker", 
           "smoking", "never_smoked",
           "sleep", "mvpa", "lpa", "sb",
           "phq", "gad"
           ), strict = FALSE, data = clr_acc_mhq$data)

egltable(c("age", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", 
           "alcohol","current_drinker", 
           "smoking", "never_smoked",
           "sleep", "mvpa", "lpa", "sb",
           "phq", "gad"
), strict = FALSE, data = clr_acc_mhq_sleep_q1$data)

egltable(c("age", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", 
           "alcohol","current_drinker", 
           "smoking", "never_smoked",
           "sleep", "mvpa", "lpa", "sb",
           "phq", "gad"
), strict = FALSE, data = clr_acc_mhq_sleep_q2$data)

egltable(c("age", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", 
           "alcohol","current_drinker", 
           "smoking", "never_smoked",
           "sleep", "mvpa", "lpa", "sb",
           "phq", "gad"
), strict = FALSE, data = clr_acc_mhq_sleep_q3$data)

nrow(clr_acc_mhq$data[!is.na(age)])
nrow(clr_acc_mhq$data[!is.na(sex)])
nrow(clr_acc_mhq$data[!is.na(ethnicg)])
nrow(clr_acc_mhq$data[!is.na(bmig)])
nrow(clr_acc_mhq$data[!is.na(edu)])
nrow(clr_acc_mhq$data[!is.na(working)])
nrow(clr_acc_mhq$data[!is.na(deprivation)])
nrow(clr_acc_mhq$data[!is.na(smoking)])
nrow(clr_acc_mhq$data[!is.na(alcohol)])

nrow(clr_acc_mhq$data[!is.na(sleep)])
nrow(clr_acc_mhq$data[!is.na(mvpa)])
nrow(clr_acc_mhq$data[!is.na(lpa)])
nrow(clr_acc_mhq$data[!is.na(sb)])

nrow(clr_acc_mhq$data[!is.na(phq)])
nrow(clr_acc_mhq$data[!is.na(gad)])

nrow(clr_acc_mhq$data[!is.na(dep_lifetime)])
nrow(clr_acc_mhq$data[!is.na(anx_lifetime)])
