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
  `short` = "#978787", #D2ABA3
  `med` = "#8AAFCA",
  `long` = "#456691" 
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
    aes(xintercept = mean(phq_2023, na.rm = TRUE)),
    color = "#999999",
    linetype = "dashed",
    linewidth = 0.75
  ) +
    ggbreak::scale_y_break(breaks = c(1000, 5000, 20000), scales = c(3, 0.5)) +
    # scale_y_break(c(10000, 35000), breaks = c(10000, 20000, 30000)) +
    
    # ggforce::facet_zoom(xlim = 10:27, ylim = 0:1000, horizontal = FALSE, zoom.size = 0.5) +
    # ggforce::facet_zoom(x = phq_2023 >= 10) +
  # scale_y_sqrt(breaks = c(0, 100, 1000, 5000, 10000, 20000)) +
  labs(x = "PHQ9") +
  theme_minimal() +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
)
saveRDS(phq_2023_hist, paste0(outputdir, "phq_2023_hist", ".RDS"))

(gad_2023_hist <- 
  ggplot(histd, aes(x = gad_2023)) +
  geom_bar(color = "#456691", fill = "#ADC7DA") +
  geom_vline(
    aes(xintercept = mean(gad_2023, na.rm = TRUE)),
    color = "#999999",
    linetype = "dashed",
    linewidth = 0.75
  ) +
    ggbreak::scale_y_break(breaks = c(5000, 10000, 30000), scales = c(3, 0.5)) +
    # ggforce::facet_zoom(xlim = 10:27, ylim = 0:1000, horizontal = FALSE, zoom.size = 0.5) +
  labs(x = "GAD7") +
  theme_minimal() +
  theme(
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )
)
saveRDS(gad_2023_hist, paste0(outputdir, "gad_2023_hist", ".RDS"))

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
    geom_density(color = "grey30", fill = "#FAF7F3",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
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
    scale_colour_manual(values = col_hist) +
    labs(x = "Sleep") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      legend.position   = "none",
      aspect.ratio      = 1
    )
)

saveRDS(sleep_hist, paste0(outputdir, "sleep_hist", ".RDS"))

(mvpa_hist <- 
    ggplot(histd, aes(x = mvpa)) +
    geom_density(color = "grey30", fill = "#FAF7F3",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
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
    scale_colour_manual(values = col_hist) +
    labs(x = "MVPA") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      legend.position   = "none",
      aspect.ratio      = 1
    )
)

saveRDS(mvpa_hist, paste0(outputdir, "mvpa_hist", ".RDS"))

(lpa_hist <- 
    ggplot(histd, aes(x = lpa)) +
    geom_density(color = "grey30", fill = "#FAF7F3",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
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
    scale_colour_manual(values = col_hist) +
    labs(x = "LPA") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      legend.position   = "none",
      aspect.ratio      = 1
    )
)
saveRDS(lpa_hist, paste0(outputdir, "lpa_hist", ".RDS"))

(sb_hist <- 
    ggplot(histd, aes(x = sb)) +
    geom_density(color = "grey30", fill = "#FAF7F3",  linewidth = 0.25, alpha = 0.8, linetype = "dashed") +
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
    scale_colour_manual(values = col_hist) +
    labs(x = "SB") +
    theme_minimal() +
    theme(
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_blank(),
      axis.text.y       = element_blank(),
      legend.position   = "none",
      plot.margin       = unit(c(0,0,0,2), "lines")
    )
)
saveRDS(sb_hist, paste0(outputdir, "sb_hist", ".RDS"))


