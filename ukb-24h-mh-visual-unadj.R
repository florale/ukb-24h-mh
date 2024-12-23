source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results ----------------
m_unadj_phq_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_99ci", ".RDS"))

# prep data ------------------
sub_models_resp   <- c("m_unadj_phq_2023_gam_sub_sleep",
                       "m_unadj_gad_2023_gam_sub_sleep"
)
sleep_type <- c("_q1_99ci", "_q2_99ci", "_q3_99ci")

sub_models <- list()
sub_models_sleepg_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = c(-20:20), level = "aggregate", digits = "asis"))
    
    model_tmp[, SleepPeriod := sleep_period]
    
    model_tmp[, From := ifelse(From == "mvpa_comp", "MVPA", From)]
    model_tmp[, From := ifelse(From == "lpa_comp", "LPA", From)]
    model_tmp[, From := ifelse(From == "sb_comp", "SB", From)]
    model_tmp[, From := ifelse(From == "sleep_comp", "Sleep", From)]
    model_tmp[, From := factor(From, ordered = TRUE,
                               levels = c("Sleep",
                                          "MVPA",
                                          "LPA",
                                          "SB"))]
    
    
    model_tmp[, To := ifelse(To == "mvpa_comp", "MVPA", To)]
    model_tmp[, To := ifelse(To == "lpa_comp", "LPA", To)]
    model_tmp[, To := ifelse(To == "sb_comp", "SB", To)]
    model_tmp[, To := ifelse(To == "sleep_comp", "Sleep", To)]
    model_tmp[, To := factor(To, ordered = TRUE,
                             levels = c("Sleep",
                                        "MVPA",
                                        "LPA",
                                        "SB"))]
    
    model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
    model_tmp[, Sig := NA]
    model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-18, 18), "*", "")]
    
    # model_tmp <- model_tmp[order(From, To, SleepPeriod)]
    
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleep Period (Q1)",
                                                              "Medium Sleep Period (Q2 and Q3)",
                                                              "Long Sleep Period (Q4)"))]
  sub_models[[i]] <- sub_models_sleepg_type_i
}
names(sub_models) <- (sub_models_resp)

# plot add ins ----------------
col <- c(
  `Short Sleep Period (Q1)` = "#333333", #D2ABA3 
  `Medium Sleep Period (Q2 and Q3)` = "#456691",
  `Long Sleep Period (Q4)` = "#8AAFCA" 
)

colf <- c(
  `Short Sleep Period (Q1)` = "#8399AE",
  `Medium Sleep Period (Q2 and Q3)` = "#ADC7DA",
  `Long Sleep Period (Q4)` = "#DCD5CE"
)

alpha <- 2/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA", "SB")
part_labels    <- c("Sleep", "MVPA", "LPA", "SB")

# # phq by sleep period----------------
sub_models_phq <- grep("phq", names(sub_models), value = T)
phq            <- "Depressive Symptoms"
rg_phq <- expand.grid.df(data.frame(sub_models_phq, phq),
                         data.frame(parts, part_labels))
# 
# plot_unadj_24h_phq_2023_99ci <- foreach(i = seq_len(nrow(rg_phq)),
#                              .packages = "multilevelcoda") %dopar% {
#                                
#                                ggplot(sub_models[[rg_phq[i, "sub_models_phq"]]][To == eval(rg_phq[i, "parts"])], aes(x = Delta, y = Mean)) +
#                                  geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
#                                  geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
#                                  geom_ribbon(aes(ymin = CI_low,
#                                                  ymax = CI_high, fill = SleepPeriod),
#                                              alpha = alpha, show.legend = TRUE) +
#                                  geom_line(aes(colour = SleepPeriod), linewidth = 1, show.legend = TRUE) +
#                                  geom_text(aes(label = Sig, colour = SleepPeriod),
#                                            size = 6, 
#                                            position = ggpp::position_nudge_center(center_x = 0, x = 3, 
#                                                                                   y = 0.005),
#                                            show.legend = FALSE) +
#                                  # facet_wrap(~ From, strip.position = "left") +
#                                  facet_wrap(ggplot2::vars(From, To),
#                                             labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryvery) %->% .(as.character(To))),
#                                             strip.position = "bottom") +
#                                  labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
#                                       y = paste0("Difference in ", rg_phq[i, "phq"])) +
#                                  scale_x_continuous(limits = c(-23, 23),
#                                                     breaks = c(-20, 0, 20)) +
#                                  scale_y_continuous(limits = c(-0.31, 0.55),
#                                                     breaks = c(-0.25, 0, 0.25)) +
#                                  scale_colour_manual(values = col,
#                                                      drop = FALSE) +
#                                  scale_fill_manual(values = colf,
#                                                    drop = FALSE) +
#                                  hrbrthemes::theme_ipsum(grid="Y") +
#                                  theme(
#                                    axis.ticks        = element_blank(),
#                                    panel.background  = element_blank(),
#                                    # panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
#                                    panel.border      = element_blank(),
#                                    panel.grid.major  = element_blank(),
#                                    panel.grid.minor  = element_blank(),
#                                    plot.background   = element_rect(fill = "transparent", colour = NA),
#                                    strip.background  = element_rect(fill = "transparent", colour = NA),
#                                    strip.text        = element_text(size = 12, face = "bold", hjust = .5),
#                                    strip.placement   = "outside",
#                                    axis.title.x      = element_blank(),
#                                    axis.text.x       = element_text(size = 11),
#                                    axis.title.y      = element_blank(),
#                                    # plot.margin       = margin(.5, .5, .5, .5, "cm"),
#                                    legend.title      = element_blank(),
#                                    legend.text       = element_text(size = 13, face = "bold", hjust = .5),
#                                    legend.position   = "bottom",
#                                    plot.margin         = unit(c(0,0.5,1.5,0.5), "lines")
#                                  )
#                                
#                              }
# 
# names(plot_unadj_24h_phq_2023_99ci) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
#   paste0("Reallocation of Time between ", rg_phq[i, "parts"], " and ", rg_phq[i, "phq"])
# }
# plot_unadj_24h_phq_2023_99ci
# 
# saveRDS(plot_unadj_24h_phq_2023_99ci, paste0(outputdir, "plot_unadj_24h_phq_2023_99ci", ".RDS"))
# 
# grDevices::cairo_pdf(
#   file = paste0(outputdir, "plot_unadj_24h_phq_2023_99ci", ".pdf"),
#   width = 9,
#   height = 12,
# )
# 
# figure <- ggarrange(plot_unadj_24h_phq_2023_99ci[[1]], plot_unadj_24h_phq_2023_99ci[[2]], 
#                     plot_unadj_24h_phq_2023_99ci[[3]], plot_unadj_24h_phq_2023_99ci[[4]], 
#                     nrow = 4,
#                     common.legend = TRUE,
#                     legend   = "bottom"
# )
# annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
# dev.off()
# 
# # gad by sleep period----------------
sub_models_gad <- grep("gad", names(sub_models), value = T)
gad            <- "Anxiety Symptoms"
rg_gad <- expand.grid.df(data.frame(sub_models_gad, gad),
                         data.frame(parts, part_labels))
# 
# plot_unadj_24h_gad_2023_99ci <- foreach(i = seq_len(nrow(rg_gad)),
#                              .packages = "multilevelcoda") %dopar% {
#                                
#                                ggplot(sub_models[[rg_gad[i, "sub_models_gad"]]][To == eval(rg_gad[i, "parts"])], aes(x = Delta, y = Mean)) +
#                                  geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
#                                  geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
#                                  geom_ribbon(aes(ymin = CI_low,
#                                                  ymax = CI_high, fill = SleepPeriod),
#                                              alpha = alpha, show.legend = TRUE) +
#                                  geom_line(aes(colour = SleepPeriod), linewidth = 1, show.legend = TRUE) +
#                                  geom_text(aes(label = Sig, colour = SleepPeriod),
#                                            size = 6, 
#                                            position = ggpp::position_nudge_center(center_x = 0, x = 3, 
#                                                                                   y = 0.005),
#                                            show.legend = FALSE) +
#                                  # facet_wrap(~ From, strip.position = "left") +
#                                  facet_wrap(ggplot2::vars(From, To),
#                                             labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryveryvery) %->% .(as.character(To))),
#                                             strip.position = "bottom") +
#                                  labs(x = bquote(Less ~ .(rg_gad[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_gad[i, "parts"])),
#                                       y = paste0("Difference in ", rg_gad[i, "gad"])) +
#                                  scale_x_continuous(limits = c(-23, 23),
#                                                     breaks = c(-20, 0, 20)) +
#                                  scale_y_continuous(limits = c(-0.25, 0.32),
#                                                     breaks = c(-0.25, 0, 0.25)) +
#                                  scale_colour_manual(values = col,
#                                                      drop = FALSE) +
#                                  scale_fill_manual(values = colf,
#                                                    drop = FALSE) +
#                                  hrbrthemes::theme_ipsum(grid="Y") +
#                                  theme(
#                                    axis.ticks        = element_blank(),
#                                    panel.background  = element_blank(),
#                                    # panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
#                                    panel.border      = element_blank(),
#                                    panel.grid.major  = element_blank(),
#                                    panel.grid.minor  = element_blank(),
#                                    plot.background   = element_rect(fill = "transparent", colour = NA),
#                                    strip.background  = element_rect(fill = "transparent", colour = NA),
#                                    strip.text        = element_text(size = 12, face = "bold", hjust = .5),
#                                    strip.placement   = "outside",
#                                    axis.title.x      = element_blank(),
#                                    axis.text.x       = element_text(size = 11),
#                                    axis.title.y      = element_blank(),
#                                    # plot.margin       = margin(.5, .5, .5, .5, "cm"),
#                                    legend.title      = element_blank(),
#                                    legend.text       = element_text(size = 13, face = "bold", hjust = .5),
#                                    legend.position   = "bottom",
#                                    plot.margin         = unit(c(0,0.5,1.5,0.5), "lines")
#                                  )
#                                
#                              }
# 
# names(plot_unadj_24h_gad_2023_99ci) <- foreach(i = seq_len(nrow(rg_gad))) %dopar% {
#   paste0("Reallocation of Time between ", rg_gad[i, "parts"], " and ", rg_gad[i, "gad"])
# }
# plot_unadj_24h_gad_2023_99ci
# 
# saveRDS(plot_unadj_24h_gad_2023_99ci, paste0(outputdir, "plot_unadj_24h_gad_2023_99ci", ".RDS"))
# 
# grDevices::cairo_pdf(
#   file = paste0(outputdir, "plot_unadj_24h_gad_2023_99ci", ".pdf"),
#   width = 9,
#   height = 12,
# )
# 
# figure <- ggarrange(plot_unadj_24h_gad_2023_99ci[[1]], plot_unadj_24h_gad_2023_99ci[[2]], 
#                     plot_unadj_24h_gad_2023_99ci[[3]], plot_unadj_24h_gad_2023_99ci[[4]], 
#                     nrow = 4,
#                     common.legend = TRUE,
#                     legend   = "bottom"
# )
# annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
# dev.off()

# final dep + anx ---------------
to <- parts
rg_phq_exp <- as.data.table(expand.grid.df(data.frame(to), rg_phq))
rg_phq_exp <- as.data.frame(rg_phq_exp[to != parts])

rg_gad_exp <- as.data.table(expand.grid.df(data.frame(to), rg_gad))
rg_gad_exp <- as.data.frame(rg_gad_exp[to != parts])

# phq
plot_unadj_phq_24h_2023_99ci_exp <- foreach(i = seq_len(nrow(rg_phq_exp)),
                                            .packages = "multilevelcoda") %dopar% {
                                              
                                              ggplot(sub_models[[rg_phq_exp[i, "sub_models_phq"]]][From == eval(rg_phq_exp[i, "parts"]) & To == eval(rg_phq_exp[i, "to"])], 
                                                     aes(x = Delta, y = Mean)) +
                                                geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_ribbon(aes(ymin = CI_low,
                                                                ymax = CI_high, fill = SleepPeriod),
                                                            alpha = alpha, show.legend = TRUE) +
                                                geom_line(aes(colour = SleepPeriod), linewidth = 1, show.legend = TRUE) +
                                                geom_text(aes(label = Sig, colour = SleepPeriod),
                                                          size = 6, 
                                                          position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                                 y = 0.005),
                                                          show.legend = FALSE) +
                                                # facet_wrap(~ From, strip.position = "left") +
                                                # facet_wrap(ggplot2::vars(From, To),
                                                #            labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryvery) %->% .(as.character(To))),
                                                #            strip.position = "bottom") +
                                                labs(x = bquote(.(rg_phq_exp[i, "parts"]) %<-% phantom(oooooooooooooo) %->%  .(rg_phq_exp[i, "to"])),
                                                     y = paste0("Difference in ", rg_phq_exp[i, "phq"])) +
                                                scale_x_continuous(limits = c(-23, 23),
                                                                   breaks = c(-20, 0, 20),
                                                                   labels = c("20min", "0", "20min")
                                                                   # sec.axis = sec_axis(~., name = "Depressive symptoms", label = NULL)
                                                ) +
                                                scale_y_continuous(limits = c(-0.3, 0.75),
                                                                   breaks = c(-0.25, 0, 0.25),
                                                                   position = "left",
                                                                   name = NULL) +
                                                scale_colour_manual(values = col,
                                                                    drop = FALSE) +
                                                scale_fill_manual(values = colf,
                                                                  drop = FALSE) +
                                                hrbrthemes::theme_ipsum(grid="Y") +
                                                theme(
                                                  axis.ticks        = element_blank(),
                                                  panel.background  = element_blank(),
                                                  # panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                                  panel.border      = element_blank(),
                                                  panel.grid.major  = element_blank(),
                                                  panel.grid.minor  = element_blank(),
                                                  plot.background   = element_rect(fill = "transparent", colour = NA),
                                                  # plot.background   = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),                                       strip.background  = element_rect(fill = "transparent", colour = NA),
                                                  strip.text        = element_text(size = 13, face = "bold", hjust = .5),
                                                  strip.placement   = "outside",
                                                  axis.title.x      = element_text(size = 12, hjust = 0.5),
                                                  axis.text.x       = element_text(size = 12),
                                                  axis.title.y      = element_blank(),
                                                  axis.text.y       = element_text(size = 12),
                                                  # plot.margin       = margin(.5, .5, .5, .5, "cm"),
                                                  legend.title      = element_blank(),
                                                  legend.text       = element_text(size = 14, face = "bold", hjust = .5),
                                                  legend.position   = "bottom",
                                                  plot.margin       = unit(c(0,0,0,0), "lines")
                                                )
                                              
                                            }
names(plot_unadj_phq_24h_2023_99ci_exp) <- foreach(i = seq_len(nrow(rg_phq_exp))) %dopar% {
  paste0("Reallocation from ", rg_phq_exp[i, "parts"], " to ",  rg_phq_exp[i, "to"], " and ", rg_phq_exp[i, "phq"])
}

# gad
plot_unadj_gad_24h_2023_99ci_exp <- foreach(i = seq_len(nrow(rg_gad_exp)),
                                            .packages = "multilevelcoda") %dopar% {
                                              
                                              ggplot(sub_models[[rg_gad_exp[i, "sub_models_gad"]]][From == eval(rg_gad_exp[i, "parts"]) & To == eval(rg_gad_exp[i, "to"])], 
                                                     aes(x = Delta, y = Mean)) +
                                                geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_ribbon(aes(ymin = CI_low,
                                                                ymax = CI_high, fill = SleepPeriod),
                                                            alpha = alpha, show.legend = TRUE) +
                                                geom_line(aes(colour = SleepPeriod), linewidth = 1, show.legend = TRUE) +
                                                geom_text(aes(label = Sig, colour = SleepPeriod),
                                                          size = 6, 
                                                          position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                                 y = 0.005),
                                                          show.legend = FALSE) +
                                                # facet_wrap(~ From, strip.position = "left") +
                                                # facet_wrap(ggplot2::vars(From, To),
                                                #            labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryvery) %->% .(as.character(To))),
                                                #            strip.position = "bottom") +
                                                labs(x = bquote(.(rg_gad_exp[i, "parts"]) %<-% phantom(oooooooooooooo) %->%  .(rg_gad_exp[i, "to"])),
                                                     y = paste0("Difference in ", rg_gad_exp[i, "phq"])) +
                                                scale_x_continuous(limits = c(-23, 23),
                                                                   breaks = c(-20, 0, 20),
                                                                   labels = c("20min", "0", "20min")
                                                                   # sec.axis = sec_axis(~., name = "Anxiety symptoms", label = NULL)
                                                ) +
                                                scale_y_continuous(limits = c(-0.27, 0.6),
                                                                   breaks = c(-0.25, 0, 0.25),
                                                                   position = "left",
                                                                   name = NULL) +
                                                scale_colour_manual(values = col,
                                                                    drop = FALSE) +
                                                scale_fill_manual(values = colf,
                                                                  drop = FALSE) +
                                                hrbrthemes::theme_ipsum(grid="Y") +
                                                theme(
                                                  axis.ticks        = element_blank(),
                                                  panel.background  = element_blank(),
                                                  # panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                                  panel.border      = element_blank(),
                                                  panel.grid.major  = element_blank(),
                                                  panel.grid.minor  = element_blank(),
                                                  plot.background   = element_rect(fill = "transparent", colour = NA),
                                                  # plot.background   = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),                                       strip.background  = element_rect(fill = "transparent", colour = NA),
                                                  strip.text        = element_text(size = 13, face = "bold", hjust = .5),
                                                  strip.placement   = "outside",
                                                  axis.title.x      = element_text(size = 12, hjust = 0.5),
                                                  axis.text.x       = element_text(size = 12),
                                                  axis.title.y      = element_blank(),
                                                  axis.text.y       = element_text(size = 12),
                                                  # plot.margin       = margin(.5, .5, .5, .5, "cm"),
                                                  legend.title      = element_blank(),
                                                  legend.text       = element_text(size = 14, face = "bold", hjust = .5),
                                                  legend.position   = "bottom",
                                                  plot.margin       = unit(c(0,0,0,0), "lines")
                                                )
                                              
                                            }

names(plot_unadj_gad_24h_2023_99ci_exp) <- foreach(i = seq_len(nrow(rg_gad_exp))) %dopar% {
  paste0("Reallocation from ", rg_gad_exp[i, "parts"], " to ",  rg_gad_exp[i, "to"], " and ", rg_gad_exp[i, "gad"])
}

## patch ------
# separate for dep and anx
p5 <- plot_unadj_phq_24h_2023_99ci_exp[[2]] + plot_unadj_phq_24h_2023_99ci_exp[[3]]  + plot_unadj_phq_24h_2023_99ci_exp[[12]] + plot_layout(guides = "collect", ncol = 3) & theme(legend.position = "none")
p6 <- plot_unadj_phq_24h_2023_99ci_exp[[8]] + plot_unadj_phq_24h_2023_99ci_exp[[11]] + plot_unadj_phq_24h_2023_99ci_exp[[1]] + plot_layout(guides = "collect", ncol = 3) & theme(legend.position = "none")

p7 <- plot_unadj_gad_24h_2023_99ci_exp[[2]] + plot_unadj_gad_24h_2023_99ci_exp[[3]]  + plot_unadj_gad_24h_2023_99ci_exp[[12]] + plot_layout(guides = "collect", ncol = 3) & theme(legend.position = "none")
p8 <- plot_unadj_gad_24h_2023_99ci_exp[[8]] + plot_unadj_gad_24h_2023_99ci_exp[[11]] + plot_unadj_gad_24h_2023_99ci_exp[[1]] + plot_layout(guides = "collect", ncol = 3) & theme(legend.position = "none")

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_phq_2023_99ci", ".pdf"),
  width = 8,
  height = 5,
)

dep_fig <- ggarrange(p5, p6,
                     nrow = 2,
                     common.legend = TRUE,
                     legend   = "none"
)
dep_fig <- annotate_figure(dep_fig, 
                           top = text_grob("A. Estimated Difference in Depressive Symptoms", size = 14, family = "Arial Narrow", face = "bold", hjust = 1)) #b4403f #658870
dep_fig

dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_gad_2023_99ci", ".pdf"),
  width = 8,
  height = 5,
)

anx_fig <- ggarrange(p7, p8,
                     nrow = 2,
                     common.legend = TRUE,
                     legend   = "bottom"
)
anx_fig <- annotate_figure(anx_fig, 
                           top = text_grob("B. Estimated Difference in Anxiety Symptoms     ", size = 14, family = "Arial Narrow", face = "bold", hjust = 1)) #b4403f #658870
anx_fig
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_phq_gad_2023_99ci", ".pdf"),
  width = 8,
  height = 11,
)
ggarrange(dep_fig, anx_fig,
          nrow = 2,
          common.legend = TRUE,
          legend   = "none"
)
dev.off()

grDevices::png(
  file = paste0(outputdir, "plot_unadj_24h_phq_gad_2023_99ci", ".png"),
  width = 8000,
  height = 11000,
  res = 900
)

ggarrange(dep_fig, anx_fig,
          nrow = 2,
          common.legend = TRUE,
          legend   = "none"
)
dev.off()

# by sleep period and insomnia symptoms -------
## read models ---------
m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_phq_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_goodsleep_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_goodsleep_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_mild_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_mild_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_mild_bl_99ci", ".RDS"))

m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q1_insomnia_persistent_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q2_insomnia_persistent_bl_99ci", ".RDS"))
m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci <- readRDS(paste0(outputdir, "m_unadj_gad_2023_gam_sub_sleep_q3_insomnia_persistent_bl_99ci", ".RDS"))

## mild and persistent data -------
sub_models_resp   <- c("m_unadj_phq_2023_gam_sub_sleep", "m_unadj_gad_2023_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep_bl_99ci", "_insomnia_mild_bl_99ci", "_insomnia_persistent_bl_99ci")

sub_models_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("None") else if (k == 2) ("Mild") else ("Moderate-Severe")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = c(-20:20), level = "aggregate", digits = "asis"))
      
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
                                     levels = c("None",
                                                "Mild",
                                                "Moderate-Severe"))]
      
      
      model_tmp[, Insomnia_label := NA]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "None", "Never/Rarely", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Mild", "Sometimes", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Moderate-Severe", "Usually", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("Never/Rarely",
                                                      "Sometimes",
                                                      "Usually"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-18, 18), "*", "")]
      
      sub_models_sleep_i_type[[k]] <- model_tmp
    }
    sub_models_sleep_i_type_k <- rbindlist(sub_models_sleep_i_type)
    sub_models_sleep_q_type[[j]] <- sub_models_sleep_i_type_k
  }
  sub_models_sleep_q_type_i  <- rbindlist(sub_models_sleep_q_type)
  sub_models_sleep_q_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                    levels = c("Short Sleep Period (Q1)",
                                                               "Medium Sleep Period (Q2 and Q3)",
                                                               "Long Sleep Period (Q4)"))]
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

## phq only 20 min reallocations ------------------
d_tmp_dep <- sub_models_insomnia[[1]][Delta == 20]
d_tmp_dep[, Sig := ifelse(sig == FALSE, "*", "")]
d_tmp_dep[, by := paste0(SleepPeriod, " - ",Insomnia)]
d_tmp_dep[, SleepPeriodID := 1:.N, by = .(From, To)]
d_tmp_dep <- d_tmp_dep[order(Insomnia, From, To, SleepPeriod)]

## final phq by insomnia ----------
plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci <- foreach(i = seq_along(parts),
                                                                 .packages = "multilevelcoda") %dopar% {
                                                                   
                                                                   part <- parts[i]
                                                                   
                                                                   ggplot(d_tmp_dep[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                                     geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                                     geom_pointrange(aes(ymin = CI_low,
                                                                                         ymax = CI_high), size = .5, linewidth = 0.5,
                                                                                     position = position_dodge2(width = 0.5)
                                                                     ) +
                                                                     geom_text(aes(y = CI_high + 0.2, label = Sig, colour = SleepPeriod),
                                                                               size = 6,
                                                                               position = position_dodge2(width = 0.5),
                                                                               show.legend = FALSE) +
                                                                     facet_wrap(ggplot2::vars(From, To),
                                                                                labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                                                strip.position = "bottom") +
                                                                     scale_colour_manual(values = col) +
                                                                     scale_y_continuous(limits = c(-1, 1.80),
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
                                                                       axis.title.x        = element_text(size = 12, hjust = 0.5, face = "bold"),
                                                                       legend.text         = element_text(size = 12, face = "bold"),
                                                                       legend.position     = "bottom",
                                                                       plot.tag.position   = c(1, 0),
                                                                       # plot.tag            = element_text(size = 11, vjust = -0.75, hjust = -0.25, face = "bold")
                                                                       # plot.tag            = element_text(size = 11, vjust = -0.5, hjust = 1, face = "bold", angle = 270)
                                                                       ,
                                                                       plot.margin         = unit(c(0.5,0.5,0.5,0), "lines")
                                                                     )
                                                                 }

plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[1]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[2]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[3]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci", ".pdf"),
  width = 9,
  height = 12,
)

# plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[1]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[2]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[3]] / plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[4]] + 
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")

figure <- ggarrange(plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[1]], plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[2]], 
                    plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[3]], plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::png(
  file = paste0(outputdir, "plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci", ".png"),
  width = 9000,
  height = 12000,
  res = 900
)

figure <- ggarrange(plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[1]], plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[2]], 
                    plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[3]], plot_unadj_24h_phq_by_sleep_period_insomnia_2023_99ci[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

## gad only 20 min reallocations ------------------
d_tmp_anx <- sub_models_insomnia[[2]][Delta == 20]
d_tmp_anx[, Sig := ifelse(sig == FALSE, "*", "")]
d_tmp_anx[, by := paste0(SleepPeriod, " - ",Insomnia)]
d_tmp_anx[, SleepPeriodID := 1:.N, by = .(From, To)]
d_tmp_anx <- d_tmp_anx[order(Insomnia, From, To, SleepPeriod)]
d_tmp_anx[, text_position := min(CI_high), by = From]

## final gad by insomnia ----------
plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci <- foreach(i = seq_along(parts),
                                                                 .packages = "multilevelcoda") %dopar% {
                                                                   
                                                                   part <- parts[i]
                                                                   
                                                                   ggplot(d_tmp_anx[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                                     geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                                     geom_pointrange(aes(ymin = CI_low,
                                                                                         ymax = CI_high, colour = SleepPeriod), size = .5, linewidth = 0.5,
                                                                                     position = position_dodge2(width = 0.5)
                                                                     ) +
                                                                     geom_text(aes(y = CI_high + 0.2, label = Sig, colour = SleepPeriod),
                                                                               size = 6,
                                                                               position = position_dodge2(width = 0.5),
                                                                               show.legend = FALSE) +
                                                                     facet_wrap(ggplot2::vars(From, To),
                                                                                labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                                                strip.position = "bottom") +
                                                                     scale_colour_manual(values = col) +
                                                                     scale_y_continuous(limits = c(-1, 1.1),
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
                                                                       axis.title.x        = element_text(size = 12, hjust = 0.5, face = "bold"),
                                                                       legend.text         = element_text(size = 12, face = "bold"),
                                                                       legend.position     = "bottom",
                                                                       plot.tag.position   = c(1, 0),
                                                                       # plot.tag            = element_text(size = 11, vjust = -0.75, hjust = -0.25, face = "bold")
                                                                       # plot.tag            = element_text(size = 11, vjust = -0.5, hjust = 1, face = "bold", angle = 270)
                                                                       ,
                                                                       plot.margin         = unit(c(0.5,0.5,0.5,0), "lines")
                                                                     )
                                                                 }
plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[1]]

plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[1]] / plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[2]] / plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[3]] / plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci", ".pdf"),
  width = 9,
  height = 12,
)

figure <- ggarrange(plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[1]], plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[2]], 
                    plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[3]], plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::png(
  file = paste0(outputdir, "plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci", ".png"),
  width = 9000,
  height = 12000,
  res = 900
)

figure <- ggarrange(plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[1]], plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[2]], 
                    plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[3]], plot_unadj_24h_gad_by_sleep_period_insomnia_2023_99ci[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

