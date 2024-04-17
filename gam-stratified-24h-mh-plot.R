source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results ----------------
m_phq_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1", ".RDS"))
m_phq_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2", ".RDS"))
m_phq_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3", ".RDS"))

m_phq_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_short", ".RDS"))
m_phq_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_normal", ".RDS"))
m_phq_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_long", ".RDS"))

m_gad_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1", ".RDS"))
m_gad_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2", ".RDS"))
m_gad_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3", ".RDS"))

m_gad_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_short", ".RDS"))
m_gad_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_normal", ".RDS"))
m_gad_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_long", ".RDS"))

m_wellbeing_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_q1", ".RDS"))
m_wellbeing_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_q2", ".RDS"))
m_wellbeing_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_q3", ".RDS"))

m_wellbeing_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_short", ".RDS"))
m_wellbeing_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_normal", ".RDS"))
m_wellbeing_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub_sleep_long", ".RDS"))

m_dep_lifetime_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q1", ".RDS"))
m_dep_lifetime_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q2", ".RDS"))
m_dep_lifetime_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_q3", ".RDS"))

m_dep_lifetime_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_short", ".RDS"))
m_dep_lifetime_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_normal", ".RDS"))
m_dep_lifetime_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub_sleep_long", ".RDS"))

m_anx_lifetime_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q1", ".RDS"))
m_anx_lifetime_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q2", ".RDS"))
m_anx_lifetime_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_q3", ".RDS"))

m_anx_lifetime_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_short", ".RDS"))
m_anx_lifetime_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_normal", ".RDS"))
m_anx_lifetime_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub_sleep_long", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q1", ".RDS"))
m_psychosis_lifetime_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q2", ".RDS"))
m_psychosis_lifetime_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_q3", ".RDS"))

m_psychosis_lifetime_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_short", ".RDS"))
m_psychosis_lifetime_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_normal", ".RDS"))
m_psychosis_lifetime_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub_sleep_long", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q1", ".RDS"))
m_bipolar_lifetime_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q2", ".RDS"))
m_bipolar_lifetime_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_q3", ".RDS"))

m_bipolar_lifetime_gam_sub_sleep_short <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_short", ".RDS"))
m_bipolar_lifetime_gam_sub_sleep_normal <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_normal", ".RDS"))
m_bipolar_lifetime_gam_sub_sleep_long <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub_sleep_long", ".RDS"))

# prep data ------------------
sub_models_sleepg   <- c("m_phq_gam_sub_sleep_short", "m_phq_gam_sub_sleep_normal", "m_phq_gam_sub_sleep_long",
                         "m_gad_gam_sub_sleep_short", "m_gad_gam_sub_sleep_normal", "m_gad_gam_sub_sleep_long",
                         "m_wellbeing_gam_sub_sleep_short", "m_wellbeing_gam_sub_sleep_normal", "m_wellbeing_gam_sub_sleep_long",
                         "m_dep_lifetime_gam_sub_sleep_short", "m_dep_lifetime_gam_sub_sleep_normal", "m_dep_lifetime_gam_sub_sleep_long",
                         "m_anx_lifetime_gam_sub_sleep_short", "m_anx_lifetime_gam_sub_sleep_normal", "m_anx_lifetime_gam_sub_sleep_long",
                         "m_psychosis_lifetime_gam_sub_sleep_short", "m_psychosis_lifetime_gam_sub_sleep_normal", "m_psychosis_lifetime_gam_sub_sleep_long"
)
sub_models_resp   <- c("m_phq_gam_sub_sleep",
                       "m_gad_gam_sub_sleep",
                       "m_wellbeing_gam_sub_sleep",
                       "m_dep_lifetime_gam_sub_sleep",
                       "m_anx_lifetime_gam_sub_sleep",
                       "m_psychosis_lifetime_gam_sub_sleep"
)
sleep_type <- c("_short", "_normal", "_long")

sub_models_all <- list()
sub_models_sleepg_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleepers") else if (j == 2) ("Normal Sleepers") else ("Long Sleepers")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = c(-20:20), level = "combined", digits = "asis"))
    
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
    
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleepers",
                                                              "Normal Sleepers",
                                                              "Long Sleepers"))]
  sub_models_all[[i]] <- sub_models_sleepg_type_i
}
names(sub_models_all) <- (sub_models_resp)

# other plot add ins ----------------
extrafont::loadfonts()

# scales::show_col(tvthemes:::brooklyn99_palette$Dark)
# scales::show_col(tvthemes:::hilda_palette)

col <- c(
  `Sleep` = "#5A6367",
  `MVPA` = "#708885",
  `LPA` = "#978787",
  `SB` = "#FAD899"
)

colf <- c(
  `Sleep` = "#83A192",
  `MVPA` = "#AFC7BB",
  `LPA` = "#DCD5CE",
  `SB` = "#FAD899"
)

# labels <- c("Sleep" = bquote(Sleep %<-% phantom() %->% .(rg_phq[i, "parts"])), 
#             "MVPA" = bquote(MVPA %<-% phantom() %->% .(rg_phq[i, "parts"])), 
#             "LPA" = bquote(LPA %<-% phantom() %->% .(rg_phq[i, "parts"])), 
#             "SB" = bquote(SB %<-% phantom() %->% .(rg_phq[i, "parts"]))
# )

labeller <- function(variable, value) {
  return(names[value])
}
alpha <- 2/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA", "SB")
part_labels    <- c("Sleep", "MVPA", 
                    "LPA", "SB")

# phq ----------------
sub_models_phq <- grep("phq", names(sub_models_all), value = T)
phq            <- "Depressive Symptoms"
rg_phq <- expand.grid.df(data.frame(sub_models_phq, phq), 
                         data.frame(parts, part_labels))

phq_24h <- foreach(i = seq_len(nrow(rg_phq)),
                   .packages = "multilevelcoda") %dopar% {

                     ggplot(sub_models_all[[rg_phq[i, "sub_models_phq"]]][To == eval(rg_phq[i, "parts"])], aes(x = Delta, y = Mean)) +
                       geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                       geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                       geom_ribbon(aes(ymin = CI_low,
                                       ymax = CI_high, fill = From),
                                   alpha = 1/10, show.legend = TRUE) +
                       geom_line(aes(colour = From), linewidth = 1, show.legend = TRUE) +
                       geom_text(aes(label = Sig, colour = From),
                                 size = 5.5, 
                                 position = ggpp::position_nudge_center(
                                   center_x = 0, x = 3, 
                                   y = 0),
                                 show.legend = FALSE) +
                       facet_wrap(~ SleepPeriod) +
                       labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                            y = paste0("Difference in ", rg_phq[i, "phq"])) +
                       scale_x_continuous(limits = c(-23, 23),
                                          breaks = c(-20, -10, 0, 10, 20)) +
                       scale_y_continuous(limits = c(-0.35, 1),
                                          breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1)) +
                       scale_colour_manual(values = col,
                                           name = "At the expense of",
                                           drop = FALSE) +
                       scale_fill_manual(values = colf,
                                         name = "At the expense of",
                                         drop = FALSE) +
                       hrbrthemes::theme_ipsum(grid="Y") +
                       theme(
                         axis.ticks        = element_blank(),
                         panel.background  = element_blank(),
                         panel.border      = element_blank(),
                         panel.grid.major  = element_blank(),
                         panel.grid.minor  = element_blank(),
                         plot.background   = element_rect(fill = "transparent", colour = NA),
                         strip.background  = element_rect(fill = "transparent", colour = NA),
                         strip.text        = if (i == 1) (element_text(size = 14, face = "bold", hjust = .5)) else (element_blank()),
                         axis.title.x      = element_text(size = 14, face = "bold", hjust = .5),
                         axis.title.y      = element_blank(),
                         plot.margin       = margin(.5, .5, .5, .5, "cm"),
                         legend.title      = element_text(size = 13, hjust = .5),
                         legend.text       = element_text(size = 13, hjust = .5),
                         legend.position   = "bottom"
                       )
                     
                   }

names(phq_24h) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h

saveRDS(phq_24h, paste0(outputdir, "phq_24h", ".RDS"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h", ".pdf"),
  width = 9,
  height = 13,
)

figure <- ggarrange(phq_24h[[1]], phq_24h[[2]], 
                    phq_24h[[3]], phq_24h[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))

dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_mvpa", ".pdf"),
  width = 9,
  height = 12,
)
phq_24h[[2]]
dev.off()

# gad ----------------
sub_models_gad <- grep("gad", names(sub_models_all), value = T)
gad            <- "Anxiety Symptoms"
rg_gad <- expand.grid.df(data.frame(sub_models_gad, gad), 
                         data.frame(parts, part_labels))

gad_24h <- foreach(i = seq_len(nrow(rg_gad)),
                   .packages = "multilevelcoda") %dopar% {
                     
                     ggplot(sub_models_all[[rg_gad[i, "sub_models_gad"]]][To == eval(rg_gad[i, "parts"])], aes(x = Delta, y = Mean)) +
                       geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                       geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                       geom_ribbon(aes(ymin = CI_low,
                                       ymax = CI_high, fill = From),
                                   alpha = alpha) +
                       geom_line(aes(colour = From), linewidth = 1) +
                       geom_text(aes(label = Sig, colour = From),
                                 size = 6, nudge_x = 0.05, nudge_y = 0.1,
                                 show.legend = FALSE) +
                       facet_wrap(ggplot2::vars(SleepPeriod, From)) +
                       scale_colour_manual(values = col) +
                       scale_fill_manual(values = colf) +
                       labs(x = paste0("Reallocation of Time between ", rg_gad[i, "part_labels"], " and Other Behaviours"),
                            y = paste0("Difference in ", rg_gad[i, "gad"])) +
                       scale_x_continuous(limits = c(-20, 20),
                                          breaks = c(-20, 0, 20)) +
                       scale_y_continuous(limits = c(-0.75, 0.75),
                                          breaks = c(-0.5, 0, 0.5)) +
                       hrbrthemes::theme_ipsum() +
                       theme(
                         axis.ticks        = element_blank(),
                         panel.background  = element_blank(),
                         panel.border      = element_blank(),
                         panel.grid.major  = element_blank(),
                         panel.grid.minor  = element_blank(),
                         plot.background   = element_rect(fill = "transparent", colour = NA),
                         # strip.text = element_blank(),
                         axis.title.x      = element_text(size = 14, face = "bold", hjust = .5),
                         axis.title.y      = element_text(size = 14, face = "bold", hjust = .5),
                         plot.margin = margin(.5, .5, .5, .5, "cm"),
                         legend.position = "none"
                       )
                   }

names(gad_24h) <- foreach(i = seq_len(nrow(rg_gad))) %dopar% {
  paste0("Reallocation of Time between ", rg_gad[i, "part_labels"], " and ", rg_gad[i, "gad"])
}
gad_24h

saveRDS(gad_24h, paste0(outputdir, "gad_24h", ".RDS"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "gad_24h_sleep", ".pdf"),
  width = 9,
  height = 12,
)
gad_24h[[1]]
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "gad_24h_mvpa", ".pdf"),
  width = 9,
  height = 12,
)
gad_24h[[2]]
dev.off()