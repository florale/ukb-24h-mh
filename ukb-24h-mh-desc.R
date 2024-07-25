source("ukb-24h-mh-data.R")

# descriptives
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #

# n complete
nrow(d_acc_mhq_2016[!is.na(p20400)]) # 66972
nrow(d_acc_mhq_2023[!is.na(p29197)]) # 63530

# followup time
d_acc_mhq[, age_diff_mh_2016_acc := (acc_startdate - p20400)/365.25]
table(round(d_acc_mhq$age_diff_mh_2016_acc), useNA = "always")
mean(d_acc_mhq$age_diff_mh_2016_acc, na.rm = TRUE)
sd(d_acc_mhq$age_diff_mh_2016_acc, na.rm = TRUE)

d_acc_mhq[, age_diff_mh_2023_acc := (acc_startdate - as.Date(p29197))/365.25]
table(round(d_acc_mhq$age_diff_mh_2023_acc), useNA = "always")
mean(d_acc_mhq$age_diff_mh_2023_acc, na.rm = TRUE)
sd(d_acc_mhq$age_diff_mh_2023_acc, na.rm = TRUE)

# possible clinical
nrow(d_acc_mhq[phq_2023 >= 10])
nrow(d_acc_mhq[gad_2023 >= 10])

egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "sleep", "mvpa", "lpa", "sb",
           "phq_2016", "gad_2016",
           "insomnia_2016",
           "phq_2023", "gad_2023",
           "insomnia_2023"
           ), strict = FALSE, data = clr_acc_mhq$data)

egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "sleep", "mvpa", "lpa", "sb",
           "phq_2016", "gad_2016",
           "insomnia_2016",
           "phq_2023", "gad_2023",
           "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq_sleep_q1$data)

egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "sleep", "mvpa", "lpa", "sb",
           "phq_2016", "gad_2016",
           "insomnia_2016",
           "phq_2023", "gad_2023",
           "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq_sleep_q2$data)

egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "sleep", "mvpa", "lpa", "sb",
           "phq_2016", "gad_2016",
           "insomnia_2016",
           "phq_2023", "gad_2023",
           "insomnia_2023"
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

nrow(clr_acc_mhq$data[!is.na(phq_2016)])
nrow(clr_acc_mhq$data[!is.na(gad_2016)])

nrow(clr_acc_mhq$data[!is.na(dep_lifetime)])
nrow(clr_acc_mhq$data[!is.na(anx_lifetime)])

table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #