source("ukb-24h-mh-data.R")

# descriptives
table(cut(clr_acc_mhq$data$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #

# n complete
nrow(d_acc_mhq[!is.na(p20400)]) # 66972
# nrow(d_acc_mhq_all[!is.na(p29197)]) # 63530

nrow(d_acc_mhq[!is.na(p29197)]) # 55945

# n diag prior
nrow(d_acc_dep_anx[time_diff_dep_anx_acc > 0]) # 11009

# n < 1y follow up
nrow(d_acc_dep_anx[time_diff_dep_anx_acc %gele% c(-1, 0)]) #368

# exluding dep anx diag up to 1y followup
nrow(d_acc_dep_anx[time_diff_dep_anx_acc >= -1]) # 11377

nrow(d_acc_mhq_dep_anx[time_diff_dep_anx_acc >= -1 & !is.na(p29197)]) # 7585

# n missing covariates
nrow(d_acc_mhq[(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])

# followup time
d_acc_mhq[, age_diff_mh_2016_acc := (acc_startdate - p20400)/365.25]
table(round(d_acc_mhq$age_diff_mh_2016_acc), useNA = "always")
mean(d_acc_mhq$age_diff_mh_2016_acc, na.rm = TRUE)
sd(d_acc_mhq$age_diff_mh_2016_acc, na.rm = TRUE)

d_acc_mhq_clean[, age_diff_mh_2023_acc := (acc_startdate - as.Date(p29197))/365.25]
table(round(d_acc_mhq_clean$age_diff_mh_2023_acc), useNA = "always")
mean(d_acc_mhq_clean$age_diff_mh_2023_acc, na.rm = TRUE)
sd(d_acc_mhq_clean$age_diff_mh_2023_acc, na.rm = TRUE)

# met who
nrow(d_acc_mhq_clean[mvpa*7 >= 150])
nrow(d_acc_mhq_clean[mvpa*7 < 150])

# possible clinical
nrow(d_acc_mhq_clean[phq_2023 >= 10])
nrow(d_acc_mhq_clean[gad_2023 >= 10])

nrow(d_acc_mhq[!is.na(age)])
nrow(d_acc_mhq[!is.na(sex)])
nrow(d_acc_mhq[!is.na(ethnicg)])
nrow(d_acc_mhq[!is.na(bmig)])
nrow(d_acc_mhq[!is.na(edu)])
nrow(d_acc_mhq[!is.na(working)])
nrow(d_acc_mhq[!is.na(deprivation)])
nrow(d_acc_mhq[!is.na(smoking)])
nrow(d_acc_mhq[!is.na(alcohol)])

nrow(d_acc_mhq[!is.na(sleep)])
nrow(d_acc_mhq[!is.na(mvpa)])
nrow(d_acc_mhq[!is.na(lpa)])
nrow(d_acc_mhq[!is.na(sb)])

nrow(d_acc_mhq[!is.na(phq_2016)])
nrow(d_acc_mhq[!is.na(gad_2016)])
nrow(d_acc_mhq[!is.na(phq_2023)])
nrow(d_acc_mhq[!is.na(gad_2023)])

# nrow(d_acc_mhq[!is.na(dep_lifetime)])
# nrow(d_acc_mhq[!is.na(anx_lifetime)])

nrow(d_acc_mhq[is.na(age) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(sex) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(ethnicg) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(bmig) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(edu) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(working) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(deprivation) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(smoking) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(alcohol) & !is.na(p29197)])

# include in main analysis - 
nrow(d_acc_mhq[!is.na(p29197) & 
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol) | is.na(icd_any_at_acc))]) #51359

nrow(d_acc_mhq[!is.na(p29197) & 
                 (is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])

nrow(d_acc_mhq[is.na(icd_any_at_acc)])
# nrow(d_acc_mhq[is.na(phq_2016) & !is.na(p20400)])
# nrow(d_acc_mhq[is.na(gad_2016) & !is.na(p20400)])

nrow(d_acc_mhq[is.na(phq_2023) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(gad_2023) & !is.na(p29197)])
nrow(d_acc_mhq[is.na(insomnia_2023) & !is.na(p29197)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023))])

nrow(d_acc_mhq[!is.na(p29197) & !(is.na(phq_2023) | is.na(gad_2023)) & 
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])
nrow(d_acc_mhq[!is.na(phq_2023) & 
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])
nrow(d_acc_mhq[!is.na(gad_2023) & 
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])

nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(age)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(sex)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(ethnicg)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(bmig)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(edu)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(working)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(deprivation)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(smoking)])
nrow(d_acc_mhq[!is.na(p29197) & (is.na(insomnia_2023) | is.na(phq_2023) | is.na(gad_2023)) & is.na(alcohol)])


# include in 1st sensitivity analysis
nrow(d_acc_mhq_dep_anx[time_diff_dep_anx_acc >= -1 & !is.na(p29197) &
                         !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))]) # 6924

# complete 2023 but not 2016
nrow(d_acc_mhq[!is.na(p29197) & is.na(p20400) &
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])

# include in 2nd sensitivity analysis

nrow(d_acc_mhq[!is.na(p29197) & !is.na(p20400) &
                 !(is.na(age) | is.na(sex) | is.na(ethnicg) | is.na(bmig) | is.na(edu) | is.na(working) | is.na(deprivation) | is.na(smoking) | is.na(alcohol))])



table(cut(d_acc_mhq$sleep, quantile(clr_acc_mhq$data$sleep, c(0, 0.25, 0.75, 1)))) #

nrow(clr_acc_mhq$data)
egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", "deprivationg",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "insomnia",
           "icd_any_at_acc",
           "sleep", "mvpa", "lpa", "sb",
           "sleep_comp", "mvpa_comp", "lpa_comp", "sb_comp",
           # "phq_2016", "gad_2016",
           # "insomnia_2016",
           "phq_2023", "gad_2023",
           "phq_2023_cutoff", "gad_2023_cutoff",
           "phq_2023_cutoff4", "gad_2023_cutoff4"
           # , "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq$data)

nrow(clr_acc_mhq_sleep_q1$data)
egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", "deprivationg",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "insomnia",
           "icd_any_at_acc",
           "sleep", "mvpa", "lpa", "sb",
           # "phq_2016", "gad_2016",
           # "insomnia_2016",
           "phq_2023", "gad_2023",
           "phq_2023_cutoff", "gad_2023_cutoff",
           "phq_2023_cutoff4", "gad_2023_cutoff4"
           # , "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq_sleep_q1$data)

nrow(clr_acc_mhq_sleep_q2$data)
egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", "deprivationg",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "insomnia",
           "icd_any_at_acc",
           "sleep", "mvpa", "lpa", "sb",
           # "phq_2016", "gad_2016",
           # "insomnia_2016",
           "phq_2023", "gad_2023",
           "phq_2023_cutoff", "gad_2023_cutoff",
           "phq_2023_cutoff4", "gad_2023_cutoff4"
           # , "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq_sleep_q2$data)

nrow(clr_acc_mhq_sleep_q3$data)
egltable(c("age", "age_at_acc", "sex", "ethnicg", "white", "bmi", "bmig",
           "edu", "working", "deprivation", "deprivationg",
           "smoking", "never_smoked",
           "alcohol","current_drinker",
           "insomnia",
           "icd_any_at_acc",
           "sleep", "mvpa", "lpa", "sb",
           # "phq_2016", "gad_2016",
           # "insomnia_2016",
           "phq_2023", "gad_2023",
           "phq_2023_cutoff", "gad_2023_cutoff",
           "phq_2023_cutoff4", "gad_2023_cutoff4"
           # , "insomnia_2023"
), strict = FALSE, data = clr_acc_mhq_sleep_q3$data)

# IQR
# 2023
psych::describe(clr_acc_mhq$data$phq_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q1$data$phq_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q2$data$phq_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q3$data$phq_2023, IQR = TRUE)

psych::describe(clr_acc_mhq$data$gad_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q1$data$gad_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q2$data$phq_2023, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q3$data$phq_2023, IQR = TRUE)

# 2016
egltable(c(
  "phq_2016_cutoff", "gad_2016_cutoff"
), strict = FALSE, data = clr_acc_mhq$data)
nrow(clr_acc_mhq$data[!is.na(phq_2016) | !is.na(gad_2016)])

egltable(c(
  "phq_2016_cutoff", "gad_2016_cutoff"
), strict = FALSE, data = clr_acc_mhq_sleep_q1$data)
nrow(clr_acc_mhq_sleep_q1$data[!is.na(phq_2016) | !is.na(gad_2016)])

egltable(c(
  "phq_2016_cutoff", "gad_2016_cutoff"
), strict = FALSE, data = clr_acc_mhq_sleep_q2$data)
nrow(clr_acc_mhq_sleep_q2$data[!is.na(phq_2016) | !is.na(gad_2016)])

egltable(c(
  "phq_2016_cutoff", "gad_2016_cutoff"
), strict = FALSE, data = clr_acc_mhq_sleep_q3$data)
nrow(clr_acc_mhq_sleep_q3$data[!is.na(phq_2016) | !is.na(gad_2016)])

psych::describe(clr_acc_mhq$data$phq_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q1$data$phq_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q2$data$phq_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q3$data$phq_2016, IQR = TRUE)

psych::describe(clr_acc_mhq$data$gad_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q1$data$gad_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q2$data$phq_2016, IQR = TRUE)
psych::describe(clr_acc_mhq_sleep_q3$data$phq_2016, IQR = TRUE)

# internal consistency
phq_2023_v <- c("p29002", "p29003", "p29004", "p29005", "p29006", "p29007", "p29008", "p29009", "p29010")
gad_2023_v <- c("p29058", "p29059", "p29060", "p29061", "p29062", "p29063", "p29064")

psych::alpha(as.data.frame(d_acc_mhq_all[eid %in% clr_acc_mhq$data$eid, phq_2023_v, with = FALSE]), check.keys = TRUE)
psych::alpha(as.data.frame(d_acc_mhq_all[eid %in% clr_acc_mhq$data$eid, gad_2023_v, with = FALSE]), check.keys = TRUE)

psych::omega(as.data.frame(d_acc_mhq_all[eid %in% clr_acc_mhq$data$eid, phq_2023_v, with = FALSE]), check.keys = TRUE)
psych::omega(as.data.frame(d_acc_mhq_all[eid %in% clr_acc_mhq$data$eid, gad_2023_v, with = FALSE]), check.keys = TRUE)

# distribution -----------
clr_acc_mhq <- readRDS(paste0(inputdir, "clr_acc_mhq", ".RDS"))

hist(clr_acc_mhq$data$phq4_2023)

histd <- clr_acc_mhq$data[, .(eid, phq_2023, gad_2023, 
                              sleep, mvpa, lpa, sb)]
histd[, sleep_group := NA]
histd[, sleep_group := ifelse(sleep <= quantile(sleep, c (0, 0.25, 0.75, 1))[[2]], "short", sleep_group)]
histd[, sleep_group := ifelse(sleep %gl% quantile(sleep, c (0, 0.25, 0.75, 1))[c(2:3)], "med", sleep_group)]
histd[, sleep_group := ifelse(sleep >= quantile(sleep, c (0, 0.25, 0.75, 1))[[3]], "long", sleep_group)]
table(histd$sleep_group, useNA = "always")

histd[, avg_sleep := mean(sleep, na.rm = TRUE), by = sleep_group]
histd[, avg_mvpa := mean(mvpa, na.rm = TRUE), by = sleep_group]
histd[, avg_lpa := mean(lpa, na.rm = TRUE), by = sleep_group]
histd[, avg_sb := mean(sb, na.rm = TRUE), by = sleep_group]

# get end point fof xintercept
histd[sleep_group == "short", peak_density_sleep := approxfun(density(histd$sleep)$x, density(histd$sleep)$y)(unique(histd[sleep_group == "short"]$avg_sleep))]
histd[sleep_group == "med", peak_density_sleep := approxfun(density(histd$sleep)$x, density(histd$sleep)$y)(unique(histd[sleep_group == "med"]$avg_sleep))]
histd[sleep_group == "long", peak_density_sleep := approxfun(density(histd$sleep)$x, density(histd$sleep)$y)(unique(histd[sleep_group == "long"]$avg_sleep))]

histd[sleep_group == "short", peak_density_mvpa := approxfun(density(histd$mvpa)$x, density(histd$mvpa)$y)(unique(histd[sleep_group == "short"]$avg_mvpa))]
histd[sleep_group == "med", peak_density_mvpa := approxfun(density(histd$mvpa)$x, density(histd$mvpa)$y)(unique(histd[sleep_group == "med"]$avg_mvpa))]
histd[sleep_group == "long", peak_density_mvpa := approxfun(density(histd$mvpa)$x, density(histd$mvpa)$y)(unique(histd[sleep_group == "long"]$avg_mvpa))]

histd[sleep_group == "short", peak_density_lpa := approxfun(density(histd$lpa)$x, density(histd$lpa)$y)(unique(histd[sleep_group == "short"]$avg_lpa))]
histd[sleep_group == "med", peak_density_lpa := approxfun(density(histd$lpa)$x, density(histd$lpa)$y)(unique(histd[sleep_group == "med"]$avg_lpa))]
histd[sleep_group == "long", peak_density_lpa := approxfun(density(histd$lpa)$x, density(histd$lpa)$y)(unique(histd[sleep_group == "long"]$avg_lpa))]

histd[sleep_group == "short", peak_density_sb := approxfun(density(histd$sb)$x, density(histd$sb)$y)(unique(histd[sleep_group == "short"]$avg_sb))]
histd[sleep_group == "med", peak_density_sb := approxfun(density(histd$sb)$x, density(histd$sb)$y)(unique(histd[sleep_group == "med"]$avg_sb))]
histd[sleep_group == "long", peak_density_sb := approxfun(density(histd$sb)$x, density(histd$sb)$y)(unique(histd[sleep_group == "long"]$avg_sb))]

histd[, quantile_intercept :=  approxfun(density(histd$sleep)$x, density(histd$sleep)$y)(quantile(sleep, c (0, 0.25, 0.75, 1))[[2]])]
histd[, quantile_intercept :=  approxfun(density(histd$sleep)$x, density(histd$sleep)$y)(quantile(sleep, c (0, 0.25, 0.75, 1))[[3]])]

col_hist <- c(
  `short` = "#8AAFCA", #D2ABA3
  `med` = "#456691",
  `long` = "#333333" 
)
colf_hist <- c(
  `short` = "#FAF7F3", #D2ABA3
  `med` = "#D2E0EA",
  `long` = "#ADC7DA" 
)

(phq_2023_hist <- 
    ggplot(histd, aes(x = phq_2023)) +
    geom_bar(color = "#456691", fill = "#ADC7DA") +
    geom_vline(
      aes(xintercept = median(phq_2023, na.rm = TRUE)),
      color = "#999999",
      linetype = "dashed",
      linewidth = 0.75
    ) +
    # ggbreak::scale_y_break(breaks = c(1000, 5000, 20000), scales = c(2, 0.5)) +
    # scale_y_break(c(10000, 35000), breaks = c(10000, 20000, 30000)) +
    
    ggforce::facet_zoom(xlim = 10:27, ylim = 0:1000, horizontal = FALSE, zoom.size = 1) +
    # ggforce::facet_zoom(x = phq_2023 >= 10) +
    # scale_y_sqrt(breaks = c(0, 100, 1000, 5000, 10000, 20000)) +
    labs(x = "Depressive symptoms by PHQ9") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      plot.background     = element_rect(fill = "transparent", colour = "black"),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
)
saveRDS(phq_2023_hist, paste0(outputdir, "phq_2023_hist", ".RDS"))

(gad_2023_hist <- 
    ggplot(histd, aes(x = gad_2023)) +
    geom_bar(color = "#ADC7DA", fill = "#456691") +
    geom_vline(
      aes(xintercept = median(gad_2023, na.rm = TRUE)),
      color = "#999999",
      linetype = "dashed",
      linewidth = 0.75
    ) +
    # ggbreak::scale_y_break(breaks = c(5000, 10000, 30000), scales = c(3, 0.5)) +
    ggforce::facet_zoom(xlim = 10:21, ylim = 0:1000, horizontal = FALSE, zoom.size = 1) +
    labs(x = "Anxiety symptoms by GAD7") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = "black"),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
)
saveRDS(gad_2023_hist, paste0(outputdir, "gad_2023_hist", ".RDS"))

grDevices::png(
  file = paste0(outputdir, "plot_mh_2023_hist", ".png"),
  width = 6000,
  height = 9000,
  res = 900
)

ggarrange(phq_2023_hist, gad_2023_hist,
          nrow = 2,
          common.legend = TRUE,
          legend   = "bottom"
)
dev.off()
# (sleep_hist <- 
#     ggplot(histd, aes(x = sleep)) +
#     geom_density(color = "#999999", fill = NA, linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
#     # geom_vline(
#     #   aes(xintercept = avg_sleep, group = sleep_group, colour = sleep_group),
#     #   linetype = "dashed",
#     #   linewidth = 1
#     # ) +
#     # geom_line(aes(y = quantile_intercept)) + 
#     geom_ribbon(data = data.frame(
#       x = density(histd$sleep)$x,
#       y = density(histd$sleep)$y,
#       quantFct = factor(findInterval(density(histd$sleep)$x, quantile(histd$sleep, probs = c(0.25, 0.75))),
#                         labels = c("short", "med", "long"))
#     ),
#     aes(x = x, ymin = 0, ymax = y, fill = quantFct), linetype = 0, show.legend = FALSE) +
#     geom_segment(
#       aes(avg_sleep, 0, xend = avg_sleep, yend = peak_density_sleep, group = sleep_group, colour = sleep_group),
#       # linetype = "dashed",
#       linewidth = 0.75
#     ) +
#     scale_colour_manual(values = col_hist) +
#     scale_fill_manual(values = colf_hist) +
#     labs(x = "Sleep period") +
#     theme_minimal() +
#     theme(
#       panel.background  = element_blank(),
#       panel.border      = element_blank(),
#       panel.grid.major  = element_blank(),
#       panel.grid.minor  = element_blank(),
#       axis.title.y      = element_blank(),
#       # axis.text.y       = element_blank(),
#       legend.position   = "none",
#       plot.margin       = unit(c(0,0,0,2), "lines")
#     )
# )

(sleep_hist <- 
    ggplot(histd, aes(x = sleep)) +
    geom_density(color = NA, fill = "#F9F5F0",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
    # geom_vline(
    #   aes(xintercept = avg_sleep, group = sleep_group, colour = sleep_group),
    #   linetype = "dashed",
    #   linewidth = 1
    # ) +
    geom_segment(
      aes(avg_sleep, 0, xend = avg_sleep, yend = peak_density_sleep, group = sleep_group, colour = sleep_group),
      # linetype = "dashed",
      linewidth = 0.75
    ) +
    scale_x_continuous(limits = c(300, 800),
                       breaks = c(400, 550, 700),
                       labels = c("400min", "500min", "700min")
    ) +
    scale_colour_manual(values = col_hist) +
    labs(x = "Sleep") +
    hrbrthemes::theme_ipsum(grid="Y") +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      axis.title.x      = element_text(size = 12, hjust = 0.5),
      axis.text.x       = element_text(size = 12),
      legend.position   = "none",
      plot.margin       = unit(c(0,0,0,2), "lines")
    )
)

saveRDS(sleep_hist, paste0(outputdir, "sleep_hist", ".RDS"))

(mvpa_hist <- 
    ggplot(histd, aes(x = mvpa)) +
    geom_density(color = NA, fill = "#F9F5F0",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
    # geom_vline(
    #   aes(xintercept = avg_mvpa, group = sleep_group, colour = sleep_group),
    #   linetype = "dashed",
    #   linewidth = 1
    # ) +
    geom_segment(
      aes(avg_mvpa, 0, xend = avg_mvpa, yend = peak_density_mvpa, group = sleep_group, colour = sleep_group),
      # linetype = "dashed",
      linewidth = 0.75
    ) +
    scale_x_continuous(limits = c(0, 200),
                       breaks = c(0, 50, 100),
                       labels = c("0min", "50min", "100min")
    ) +
    scale_colour_manual(values = col_hist) +
    labs(x = "MVPA") +
    hrbrthemes::theme_ipsum(grid="Y") +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      axis.title.x      = element_text(size = 12, hjust = 0.5),
      axis.text.x       = element_text(size = 12),
      legend.position   = "none",
      plot.margin       = unit(c(0,0,0,2), "lines")
    )
)

saveRDS(mvpa_hist, paste0(outputdir, "mvpa_hist", ".RDS"))

(lpa_hist <- 
    ggplot(histd, aes(x = lpa)) +
    geom_density(color = NA, fill = "#F9F5F0",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
    # geom_vline(
    #   aes(xintercept = avg_lpa, group = sleep_group, colour = sleep_group),
    #   linetype = "dashed",
    #   linewidth = 1
    # ) +
    geom_segment(
      aes(avg_lpa, 0, xend = avg_lpa, yend = peak_density_lpa, group = sleep_group, colour = sleep_group),
      # linetype = "dashed",
      linewidth = 0.75
    ) +
    scale_x_continuous(limits = c(0, 650),
                       breaks = c(100, 300, 600),
                       labels = c("100min", "300min", "600min")
    ) +
    scale_colour_manual(values = col_hist) +
    labs(x = "LPA") +
    hrbrthemes::theme_ipsum(grid="Y") +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      axis.title.x      = element_text(size = 12, hjust = 0.5),
      axis.text.x       = element_text(size = 12),
      legend.position   = "none",
      plot.margin       = unit(c(0,0,0,2), "lines")
    )
)
saveRDS(lpa_hist, paste0(outputdir, "lpa_hist", ".RDS"))

(sb_hist <- 
    ggplot(histd, aes(x = sb)) +
    geom_density(color = NA, fill = "#F9F5F0",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
    # geom_vline(
    #   aes(xintercept = avg_sb, group = sleep_group, colour = sleep_group),
    #   linetype = "dashed",
    #   linewidth = 1
    # ) +
    geom_segment(
      aes(avg_sb, 0, xend = avg_sb, yend = peak_density_sb, group = sleep_group, colour = sleep_group),
      # linetype = "dashed",
      linewidth = 0.75
    ) +
    scale_x_continuous(limits = c(200, 900),
                       breaks = c(400, 550, 700),
                       labels = c("400min", "550min", "700min")
    ) +
    scale_colour_manual(values = col_hist) +
    labs(x = "SB") +
    hrbrthemes::theme_ipsum(grid="Y") +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      axis.title.x      = element_text(size = 12, hjust = 0.5),
      axis.text.x       = element_text(size = 12),
      legend.position   = "none",
      plot.margin       = unit(c(0,0,0,2), "lines")
    )
)
saveRDS(sb_hist, paste0(outputdir, "sb_hist", ".RDS"))

# dag -------------------
coords <- tibble::tribble(
  ~name,           ~x,      ~y,
  "behaviours",     1,       0,
  "mental_health",  1.75,       0,
  "demographics",   1,       1,
  "lifestyle",      1.25,    1,
  "bmi",            1.75,     0.5,
  "icd_prior",      1.5,     1,
  "icd_after",      1.75,       1
)

dag <-  ggdag::dagify(mental_health ~ behaviours,
                      mental_health ~ demographics + lifestyle + bmi + icd_prior + icd_after,
                      behaviours    ~  demographics + lifestyle + bmi + icd_prior,
                      # lifestyle ~ demographics,
                      bmi ~  demographics + lifestyle + icd_prior + behaviours,
                      icd_after ~  demographics + lifestyle + icd_prior + behaviours,
                      icd_prior ~ demographics + lifestyle,
                      exposure = "behaviours",
                      outcome = "mental_health",
                      latent = c("bmi", "icd_after"),
                      
                      labels = c("mental_health" = "Mental health\n(depressive and anxiety symptoms)",
                                 "behaviours" = "24h behaviours",
                                 "demographics" = "Demographic factors\n(age, sex, deprivation,\neducation, employment)",
                                 "lifestyle" = "Lifestyle factors\n(smoking, alcohol)",
                                 "icd_prior" = "Chronic conditions\nprior to accelerometry",
                                 "icd_after" = "Chronic conditions\nat follow-up",
                                 # "employment" = "Employment",
                                 "bmi" = "BMI"),
                      
                      # labels = c("cancer" = "Exposure",
                      #            "behaviours" = "Outcome",
                      #            "demographics" = "Confounders",
                      #            "lifestyle" = "Confounders",
                      #            # "education" = "Education",
                      #            # "employment" = "Employment",
                      #            "bmi" = "Mechanism"),
                      coords = coords
)

plot_dag <- ggdag::ggdag_parents(dag, "behaviours",
                                 text = FALSE
)
plot_dag[["data"]]$parent <- NA
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "mental_health", "Outcome", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "behaviours", "Exposure", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "bmi", "Mechanism (Unadjusted)", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "icd_after", "Mechanism (Unadjusted)", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "demographics", "Confounder (Adjusted)", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "lifestyle", "Confounder (Adjusted)", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- ifelse(plot_dag[["data"]]$name == "icd_prior", "Confounder (Adjusted)", plot_dag[["data"]]$parent)
plot_dag[["data"]]$parent <- factor(plot_dag[["data"]]$parent, ordered = TRUE,
                                    levels = c(
                                      "Exposure",
                                      "Outcome",
                                      "Confounder (Adjusted)",
                                      "Mechanism (Unadjusted)"
                                    ))

(plot_dag <- plot_dag +
    geom_dag_edges(
      arrow_directed = grid::arrow(length = grid::unit(2, "pt"), type = "closed"),
      arrow_bidirected = grid::arrow(length = grid::unit(2, "pt"), ends = "both", type = "closed")) +
    geom_dag_point(aes(color = parent)) +  # Adjust node colors
    scale_color_manual(values = c(
      "Exposure" = "#456691", # DCD5CE
      "Outcome" = "#944C4C",
      "Confounder (Adjusted)" = "#ADC7DA", #
      "Mechanism (Unadjusted)" = "#D9D9D9"
    ), drop = TRUE, name = NULL)+
    geom_label(aes(label = label),
               color = "black",
               vjust = 0,
               nudge_x = 0, nudge_y = 0.075,
               family = "Arial Narrow", size = 4,
               show.legend = NA) +
    
    theme_void() +
    theme(
      legend.position     = "bottom"
      # plot.margin         = unit(c(0,1.5,0,0), "lines")
    ))

saveRDS(plot_dag, paste0(outputdir, "ukb_24h_mh_dag", ".RDS"))

grDevices::png(
  file = paste0(outputdir, "ukb_24h_mh_dag", ".png"),
  width = 9000,
  height = 6500,
  res = 700
)
plot_dag
dev.off()
