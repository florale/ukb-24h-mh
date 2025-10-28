source("ukb-24h-mh-setup.R")
source(paste0(redir, "ukb_utils.R"))

# plot add ins ----------------
col <- c(
  `Short Sleep Opportunity  (Q1)` = "#333333", #D2ABA3 
  `Medium Sleep Opportunity  (Q2 and Q3)` = "#456691",
  `Long Sleep Opportunity  (Q4)` = "#8AAFCA" 
)

colf <- c(
  `Short Sleep Opportunity  (Q1)` = "#8399AE",
  `Medium Sleep Opportunity  (Q2 and Q3)` = "#ADC7DA",
  `Long Sleep Opportunity  (Q4)` = "#DCD5CE"
)

alpha <- 2/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA", "SB")
part_labels    <- c("Sleep", "MVPA", "LPA", "SB")

# by sleep opportunity  and insomnia symptoms -------
## read models ---------
m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_bl_99ci", ".RDS"))

## with and without -------
sub_models_resp   <- c("m_unadj_phq_2023_gam_sub_sleep", "m_unadj_gad_2023_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep_bl_99ci", "_insomnia_bl_99ci")

sub_models_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Opportunity  (Q1)") else if (j == 2) ("Medium Sleep Opportunity  (Q2 and Q3)") else ("Long Sleep Opportunity  (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("No") else ("Yes")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = c(-30:30), level = "aggregate", digits = "asis"))
      
      model_tmp[, SleepPeriod := sleep_period]
      model_tmp[, Insomnia := insomnia]
      
      model_tmp[, From := ifelse(From == "mvpa_comp", "MVPA", From)]
      model_tmp[, From := ifelse(From == "lpa_comp", "LPA ", From)]
      model_tmp[, From := ifelse(From == "sb_comp", " SB ", From)]
      model_tmp[, From := ifelse(From == "sleep_comp", "Sleep", From)]
      model_tmp[, From := factor(From, ordered = TRUE,
                                 levels = c("Sleep",
                                            "MVPA",
                                            "LPA ",
                                            " SB "))]
      
      
      model_tmp[, To := ifelse(To == "mvpa_comp", "MVPA", To)]
      model_tmp[, To := ifelse(To == "lpa_comp", "LPA ", To)]
      model_tmp[, To := ifelse(To == "sb_comp", " SB ", To)]
      model_tmp[, To := ifelse(To == "sleep_comp", "Sleep", To)]
      model_tmp[, To := factor(To, ordered = TRUE,
                               levels = c("Sleep",
                                          "MVPA",
                                          "LPA ",
                                          " SB "))]
      
      model_tmp[, Insomnia := factor(Insomnia, ordered = TRUE,
                                     levels = c("Yes",
                                                "No"))]
      
      
      model_tmp[, Insomnia_label := NA]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Yes", "SleepProblem+", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "No", "SleepProblem-", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("SleepProblem+",
                                                      "SleepProblem-"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-28, 28), "*", "")]
      
      sub_models_sleep_i_type[[k]] <- model_tmp
    }
    sub_models_sleep_i_type_k <- rbindlist(sub_models_sleep_i_type)
    sub_models_sleep_q_type[[j]] <- sub_models_sleep_i_type_k
  }
  sub_models_sleep_q_type_i  <- rbindlist(sub_models_sleep_q_type)
  sub_models_sleep_q_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                    levels = c("Short Sleep Opportunity  (Q1)",
                                                               "Medium Sleep Opportunity  (Q2 and Q3)",
                                                               "Long Sleep Opportunity  (Q4)"))]
  sub_models_insomnia[[i]] <- sub_models_sleep_q_type_i
}
names(sub_models_insomnia) <- (sub_models_resp)

# make a grid to loop plots
parts                   <- c("Sleep", "MVPA", "LPA ", " SB ")
part_labels             <- c("Sleep", "MVPA", "LPA ", " SB ")
sub_models_phq_insomnia <- grep("phq", names(sub_models_insomnia), value = T)
phq                     <- "Depressive Symptoms"
rg_phq                  <- expand.grid.df(data.frame(sub_models_phq_insomnia, phq), 
                                          data.frame(parts, part_labels))
to <- parts
rg_phq_exp <- as.data.table(expand.grid.df(data.frame(to), rg_phq))
rg_phq_exp <- as.data.frame(rg_phq_exp[to != parts])

## phq only 30 min reallocations ------------------
d_tmp_dep <- sub_models_insomnia[[1]][Delta == 30]
d_tmp_dep[, Sig := ifelse(sig == FALSE, "*", "")]
d_tmp_dep[, by := paste0(SleepPeriod, " - ",Insomnia)]
d_tmp_dep[, SleepPeriodID := 1:.N, by = .(From, To)]
d_tmp_dep <- d_tmp_dep[order(Insomnia, From, To, SleepPeriod)]

## final phq by insomnia ----------
plot_unadj_24h_phq_by_sleep_period_insomnia <- foreach(i = seq_along(parts),
                                                       .packages = "multilevelcoda") %dopar% {
                                                         
                                                         part <- parts[i]
                                                         
                                                         ggplot(d_tmp_dep[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                           geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                           geom_pointrange(aes(ymin = CI_low,
                                                                               ymax = CI_high), size = .5, linewidth = 0.5,
                                                                           position = position_dodge2(width = 0.5)
                                                           ) +
                                                           geom_text(aes(y = CI_high + 0.1, label = Sig, colour = SleepPeriod),
                                                                     size = 6,
                                                                     position = position_dodge2(width = 0.5),
                                                                     show.legend = FALSE) +
                                                           facet_wrap(ggplot2::vars(From, To),
                                                                      labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                                      strip.position = "bottom") +
                                                           scale_colour_manual(values = col) +
                                                           scale_y_continuous(limits = c(-1, 2.2),
                                                                              breaks = c(-1, 0, 1)
                                                           ) +
                                                           scale_x_discrete(position = "top") +
                                                           # scale_x_continuous(
                                                           #   breaks = unique(d_tmp_dep$SleepPeriodID), 
                                                           #   labels = d_tmp_dep[1:12]$SleepPeriod,
                                                           #   sec.axis = sec_axis(~.,
                                                           #                       breaks = unique(d_tmp_dep$SleepPeriodID),
                                                           #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
                                                           #   )
                                                           # ) +
                                                           # labs(x = "", y = "", colour = "", tag = " Insomnia\nSymptoms") +
                                                           labs(x = "Insomnia Symptoms", y = "", colour = "", tag = "") +
                                                           # coord_flip() +
                                                           theme_ipsum() +
                                                           theme(
                                                             axis.ticks          = element_blank(),
                                                             panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                                             plot.background     = element_rect(fill = "transparent", colour = NA),
                                                             panel.border        = element_blank(),
                                                             panel.grid.major.y  = element_blank(),
                                                             panel.grid.minor    = element_blank(),
                                                             panel.spacing       = unit(0.75, "lines"),
                                                             strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
                                                             axis.text.x         = element_text(size = 11, hjust = 0.5, margin = margin(t = 10, unit = "cm")),
                                                             axis.title.x        = element_blank(),
                                                             legend.text         = element_text(size = 12, face = "bold"),
                                                             legend.position     = "bottom",
                                                             plot.tag.position   = c(1, 0),
                                                             # plot.tag            = element_text(size = 11, vjust = -0.75, hjust = -0.25, face = "bold")
                                                             # plot.tag            = element_text(size = 11, vjust = -0.5, hjust = 1, face = "bold", angle = 270)
                                                             ,
                                                             plot.margin         = unit(c(0.5,0.5,0.5,0), "lines")
                                                           )
                                                       }

plot_unadj_24h_phq_by_sleep_period_insomnia[[1]] / plot_unadj_24h_phq_by_sleep_period_insomnia[[2]] / plot_unadj_24h_phq_by_sleep_period_insomnia[[3]] / plot_unadj_24h_phq_by_sleep_period_insomnia[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_phq_by_sleep_period_insomnia", ".pdf"),
  width = 9,
  height = 12,
)

figure <- ggarrange(plot_unadj_24h_phq_by_sleep_period_insomnia[[2]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[3]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[4]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[1]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::png(
  file = paste0(outputdir, "plot_unadj_24h_phq_by_sleep_period_insomnia", ".png"),
  width = 9000,
  height = 12000,
  res = 900
)

figure <- ggarrange(plot_unadj_24h_phq_by_sleep_period_insomnia[[2]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[3]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[4]],
                    plot_unadj_24h_phq_by_sleep_period_insomnia[[1]],
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

## gad only 30 min reallocations ------------------
d_tmp_anx <- sub_models_insomnia[[2]][Delta == 30]
d_tmp_anx[, Sig := ifelse(sig == FALSE, "*", "")]
d_tmp_anx[, by := paste0(SleepPeriod, " - ",Insomnia)]
d_tmp_anx[, SleepPeriodID := 1:.N, by = .(From, To)]
d_tmp_anx <- d_tmp_anx[order(Insomnia, From, To, SleepPeriod)]

## final gad by insomnia ----------
plot_unadj_24h_gad_by_sleep_period_insomnia <- foreach(i = seq_along(parts),
                                                       .packages = "multilevelcoda") %dopar% {
                                                         
                                                         part <- parts[i]
                                                         
                                                         ggplot(d_tmp_anx[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                           geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                           geom_pointrange(aes(ymin = CI_low,
                                                                               ymax = CI_high, colour = SleepPeriod), size = .5, linewidth = 0.5,
                                                                           position = position_dodge2(width = 0.5)
                                                           ) +
                                                           geom_text(aes(y = CI_high + 0.1, label = Sig, colour = SleepPeriod),
                                                                     size = 6,
                                                                     position = position_dodge2(width = 0.5),
                                                                     show.legend = FALSE) +
                                                           facet_wrap(ggplot2::vars(From, To),
                                                                      labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                                      strip.position = "bottom") +
                                                           scale_colour_manual(values = col) +
                                                           scale_y_continuous(limits = c(-1, 2),
                                                                              breaks = c(-1, 0, 1)
                                                           ) +
                                                           scale_x_discrete(position = "top") +
                                                           # scale_x_continuous(
                                                           #   breaks = unique(d_tmp_dep$SleepPeriodID), 
                                                           #   labels = d_tmp_dep[1:12]$SleepPeriod,
                                                           #   sec.axis = sec_axis(~.,
                                                           #                       breaks = unique(d_tmp_dep$SleepPeriodID),
                                                           #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
                                                           #   )
                                                           # ) +
                                                           # labs(x = "", y = "", colour = "", tag = " Insomnia\nSymptoms") +
                                                           labs(x = "Insomnia Symptoms", y = "", colour = "", tag = "") +
                                                           # coord_flip() +
                                                           theme_ipsum() +
                                                           theme(
                                                             axis.ticks          = element_blank(),
                                                             panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                                             plot.background     = element_rect(fill = "transparent", colour = NA),
                                                             panel.border        = element_blank(),
                                                             panel.grid.major.y  = element_blank(),
                                                             panel.grid.minor    = element_blank(),
                                                             panel.spacing       = unit(0.75, "lines"),
                                                             strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
                                                             axis.text.x         = element_text(size = 11, hjust = 0.5, margin = margin(t = 10, unit = "cm")),
                                                             axis.title.x        = element_blank(),
                                                             legend.text         = element_text(size = 12, face = "bold"),
                                                             legend.position     = "bottom",
                                                             plot.tag.position   = c(1, 0),
                                                             # plot.tag            = element_text(size = 11, vjust = -0.75, hjust = -0.25, face = "bold")
                                                             # plot.tag            = element_text(size = 11, vjust = -0.5, hjust = 1, face = "bold", angle = 270)
                                                             ,
                                                             plot.margin         = unit(c(0.5,0.5,0.5,0), "lines")
                                                           )
                                                       }
plot_unadj_24h_gad_by_sleep_period_insomnia[[1]]

plot_unadj_24h_gad_by_sleep_period_insomnia[[1]] / plot_unadj_24h_gad_by_sleep_period_insomnia[[2]] / plot_unadj_24h_gad_by_sleep_period_insomnia[[3]] / plot_unadj_24h_gad_by_sleep_period_insomnia[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_gad_by_sleep_period_insomnia", ".pdf"),
  width = 9,
  height = 12,
)

figure <- ggarrange(plot_unadj_24h_gad_by_sleep_period_insomnia[[2]],
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[3]], 
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[4]],
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[1]],
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::png(
  file = paste0(outputdir, "plot_unadj_24h_gad_by_sleep_period_insomnia", ".png"),
  width = 9000,
  height = 12000,
  res = 900
)

figure <- ggarrange(plot_unadj_24h_gad_by_sleep_period_insomnia[[2]],
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[3]], 
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[4]],
                    plot_unadj_24h_gad_by_sleep_period_insomnia[[1]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()
