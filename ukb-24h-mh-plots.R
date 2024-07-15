source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results ----------------
m_phq_2016_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q1", ".RDS"))
m_phq_2016_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q2", ".RDS"))
m_phq_2016_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_2016_gam_sub_sleep_q3", ".RDS"))

# prep data ------------------
sub_models_resp   <- c("m_phq_2016_gam_sub_sleep"
)
sleep_type <- c("_q1", "_q2", "_q3")

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
    
    model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
    model_tmp[, Sig := NA]
    model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-18, 18), "*", "")]
    
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
# col <- c(
#   `Short Sleep Period (Q1)` = "#03396c",
#   `Medium Sleep Period (Q2 and Q3)` = "#978787",
#   `Long Sleep Period (Q4)` = "#8AAFCA" 
# )
# colf <- c(
#   `Short Sleep Period (Q1)` = "#6497b1",
#   `Medium Sleep Period (Q2 and Q3)` = "#DCD5CE",
#   `Long Sleep Period (Q4)` = "#b3cde0"
# )

col <- c(
  `Short Sleep Period (Q1)` = "#456691",
  `Medium Sleep Period (Q2 and Q3)` = "#978787",
  `Long Sleep Period (Q4)` = "#8AAFCA" 
)
colf <- c(
  `Short Sleep Period (Q1)` = "#8399AE",
  `Medium Sleep Period (Q2 and Q3)` = "#DCD5CE",
  `Long Sleep Period (Q4)` = "#A1B2C2"
)

alpha <- 2/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA ", " SB ")
part_labels    <- c("Sleep", "MVPA", 
                    "LPA ", " SB ")

# phq by sleep period----------------
sub_models_phq <- grep("phq", names(sub_models), value = T)
phq            <- "Depressive Symptoms"
rg_phq <- expand.grid.df(data.frame(sub_models_phq, phq), 
                         data.frame(parts, part_labels))

phq_24h <- foreach(i = seq_len(nrow(rg_phq)),
                            .packages = "multilevelcoda") %dopar% {
                              
                              ggplot(sub_models[[rg_phq[i, "sub_models_phq"]]][To == eval(rg_phq[i, "parts"])], aes(x = Delta, y = Mean)) +
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
                                facet_wrap(ggplot2::vars(From, To),
                                           labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryveryvery) %->% .(as.character(To))),
                                           strip.position = "bottom") +
                                labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                     y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                scale_x_continuous(limits = c(-23, 23),
                                                   breaks = c(-20, 0, 20)) +
                                scale_y_continuous(limits = c(-0.4, 0.7),
                                                   breaks = c(-0.25, 0, 0.25, 0.5)) +
                                scale_colour_manual(values = col,
                                                    drop = FALSE) +
                                scale_fill_manual(values = colf,
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
                                  strip.text        = element_text(size = 12, face = "bold", hjust = .5),
                                  strip.placement   = "outside",
                                  axis.title.x      = element_blank(),
                                  axis.title.y      = element_blank(),
                                  plot.margin       = margin(.5, .5, .5, .5, "cm"),
                                  legend.title      = element_blank(),
                                  legend.text       = element_text(size = 13, face = "bold", hjust = .5),
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
  height = 12.5,
)

figure <- ggarrange(phq_24h[[1]], phq_24h[[2]], 
                    phq_24h[[3]], phq_24h[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# gad by sleep period----------------
sub_models_gad <- grep("gad", names(sub_models), value = T)
gad            <- "Depressive Symptoms"
rg_gad <- expand.grid.df(data.frame(sub_models_gad, gad), 
                         data.frame(parts, part_labels))

gad_24h <- foreach(i = seq_len(nrow(rg_gad)),
                            .packages = "multilevelcoda") %dopar% {
                              
                              ggplot(sub_models[[rg_gad[i, "sub_models_gad"]]][To == eval(rg_gad[i, "parts"])], aes(x = Delta, y = Mean)) +
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
                                facet_wrap(ggplot2::vars(From, To),
                                           labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryveryvery) %->% .(as.character(To))),
                                           strip.position = "bottom") +
                                labs(x = bquote(Less ~ .(rg_gad[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_gad[i, "parts"])),
                                     y = paste0("Difference in ", rg_gad[i, "gad"])) +
                                scale_x_continuous(limits = c(-23, 23),
                                                   breaks = c(-20, 0, 20)) +
                                scale_y_continuous(limits = c(-0.3, 0.5),
                                                   breaks = c(-0.25, 0, 0.25, 0.5)) +
                                scale_colour_manual(values = col,
                                                    drop = FALSE) +
                                scale_fill_manual(values = colf,
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
                                  strip.text        = element_text(size = 12, face = "bold", hjust = .5),
                                  strip.placement   = "outside",
                                  axis.title.x      = element_blank(),
                                  axis.title.y      = element_blank(),
                                  plot.margin       = margin(.5, .5, .5, .5, "cm"),
                                  legend.title      = element_blank(),
                                  legend.text       = element_text(size = 13, face = "bold", hjust = .5),
                                  legend.position   = "bottom"
                                )
                              
                            }

names(gad_24h) <- foreach(i = seq_len(nrow(rg_gad))) %dopar% {
  paste0("Reallocation of Time between ", rg_gad[i, "part_labels"], " and ", rg_gad[i, "gad"])
}
gad_24h

saveRDS(gad_24h, paste0(outputdir, "gad_24h", ".RDS"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "gad_24h", ".pdf"),
  width = 9,
  height = 12.5,
)

figure <- ggarrange(gad_24h[[1]], gad_24h[[2]], 
                    gad_24h[[3]], gad_24h[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()
