source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# PHQ by sleep period and 4 insomnia types ----------------
## substitution results -----------------
m_phq_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

sub_models_resp   <- c("m_phq_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep", "_insomnia_mild", "_insomnia_moderate", "_insomnia_severe")

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
      insomnia <- if (k == 1) ("Good Sleep") else if (k == 2) ("Mild Insomnia") else if (k == 3) ("Moderate Insomnia") else ("Severe Insomnia")
      
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
                                     levels = c("Good Sleep",
                                                "Mild Insomnia",
                                                "Moderate Insomnia",
                                                "Severe Insomnia"))]
      
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
## plot add ins ----------------
col_sleepq <- c(
  `Short Sleep Period (Q1)` = "#456691",
  `Medium Sleep Period (Q2 and Q3)` = "#978787",
  `Long Sleep Period (Q4)` = "#8AAFCA" 
)
colf_sleepq <- c(
  `Short Sleep Period (Q1)` = "#8399AE",
  `Medium Sleep Period (Q2 and Q3)` = "#DCD5CE",
  `Long Sleep Period (Q4)` = "#A1B2C2"
)

alpha <- 1/10

### phq good sleep by sleep period ---------------
phq_24h_goodsleep <- foreach(i = seq_len(nrow(rg_phq)),
                             .packages = "multilevelcoda") %dopar% {
                               
                               ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Good Sleep"], aes(x = Delta, y = Mean)) +
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
                                 facet_wrap(ggplot2::vars(From, To, Insomnia),
                                            labeller = label_bquote(cols = .(as.character(From)) %<-%   ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                            strip.position = "bottom") +
                                 labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                      y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                 scale_x_continuous(limits = c(-23, 23),
                                                    breaks = c(-20, 0, 20)) +
                                 scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
                                 scale_colour_manual(values = col_sleepq,
                                                     drop = FALSE) +
                                 scale_fill_manual(values = colf_sleepq,
                                                   drop = FALSE) +
                                 hrbrthemes::theme_ipsum() +
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

names(phq_24h_goodsleep) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_goodsleep
saveRDS(phq_24h_goodsleep, paste0(outputdir, "phq_24h_goodsleep", ".RDS"))

### phq mild insomnia by sleep period ---------------
phq_24h_insomnia_mild <- foreach(i = seq_len(nrow(rg_phq)),
                                 .packages = "multilevelcoda") %dopar% {
                                   
                                   ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Mild Insomnia"], aes(x = Delta, y = Mean)) +
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
                                     facet_wrap(ggplot2::vars(From, To, Insomnia),
                                                labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                                strip.position = "bottom") +
                                     labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                          y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                     scale_x_continuous(limits = c(-23, 23),
                                                        breaks = c(-20, 0, 20)) +
                                     scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4)) +
                                     scale_colour_manual(values = col_sleepq,
                                                         drop = FALSE) +
                                     scale_fill_manual(values = colf_sleepq,
                                                       drop = FALSE) +
                                     hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_mild) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_mild
saveRDS(phq_24h_insomnia_mild, paste0(outputdir, "phq_24h_insomnia_mild", ".RDS"))

### phq moderate insomnia by sleep period ---------------
phq_24h_insomnia_moderate <- foreach(i = seq_len(nrow(rg_phq)),
                                     .packages = "multilevelcoda") %dopar% {
                                       
                                       ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Moderate Insomnia"], aes(x = Delta, y = Mean)) +
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
                                         facet_wrap(ggplot2::vars(From, To, Insomnia),
                                                    labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                                    strip.position = "bottom") +
                                         labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                              y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                         scale_x_continuous(limits = c(-23, 23),
                                                            breaks = c(-20, 0, 20)) +
                                         scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1)) +
                                         scale_colour_manual(values = col_sleepq,
                                                             drop = FALSE) +
                                         scale_fill_manual(values = colf_sleepq,
                                                           drop = FALSE) +
                                         hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_moderate) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_moderate
saveRDS(phq_24h_insomnia_moderate, paste0(outputdir, "phq_24h_insomnia_moderate", ".RDS"))

### phq severe insomnia by sleep period ---------------
phq_24h_insomnia_severe <- foreach(i = seq_len(nrow(rg_phq)),
                                   .packages = "multilevelcoda") %dopar% {
                                     
                                     ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Severe Insomnia"], aes(x = Delta, y = Mean)) +
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
                                       facet_wrap(ggplot2::vars(From, To, Insomnia),
                                                  labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                                  strip.position = "bottom") +
                                       labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                            y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                       scale_x_continuous(limits = c(-23, 23),
                                                          breaks = c(-20, 0, 20)) +
                                       scale_y_continuous(breaks = c(-1, 0, 1, 2)) +
                                       scale_colour_manual(values = col_sleepq,
                                                           drop = FALSE) +
                                       scale_fill_manual(values = colf_sleepq,
                                                         drop = FALSE) +
                                       hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_severe) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_severe
saveRDS(phq_24h_insomnia_severe, paste0(outputdir, "phq_24h_insomnia_severe", ".RDS"))

## save -----
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_goodsleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_goodsleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_goodsleep[[2]], 
                                     phq_24h_goodsleep[[3]], phq_24h_goodsleep[[4]], 
                                     nrow = 4,
                                     common.legend = TRUE,
                                     legend   = "bottom"
)
annotate_figure(patch_phq_24h_goodsleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_mild", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_mild <- ggarrange(phq_24h_insomnia_mild[[1]], phq_24h_insomnia_mild[[2]], 
                                         phq_24h_insomnia_mild[[3]], phq_24h_insomnia_mild[[4]], 
                                         nrow = 4,
                                         common.legend = TRUE,
                                         legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_mild, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_moderate", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_moderate <- ggarrange(phq_24h_insomnia_moderate[[1]], phq_24h_insomnia_moderate[[2]], 
                                             phq_24h_insomnia_moderate[[3]], phq_24h_insomnia_moderate[[4]], 
                                             nrow = 4,
                                             common.legend = TRUE,
                                             legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_moderate, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_severe", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_severe <- ggarrange(phq_24h_insomnia_severe[[1]], phq_24h_insomnia_severe[[2]], 
                                           phq_24h_insomnia_severe[[3]], phq_24h_insomnia_severe[[4]], 
                                           nrow = 4,
                                           common.legend = TRUE,
                                           legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_severe, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sleep reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_sleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_insomnia_mild[[1]], 
                                    phq_24h_insomnia_moderate[[1]], phq_24h_insomnia_severe[[1]], 
                                    nrow = 4,
                                    common.legend = TRUE,
                                    legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# mvpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_mvpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_mvpa <- ggarrange(phq_24h_goodsleep[[2]], phq_24h_insomnia_mild[[2]], 
                                   phq_24h_insomnia_moderate[[2]], phq_24h_insomnia_severe[[2]], 
                                   nrow = 4,
                                   common.legend = TRUE,
                                   legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_mvpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# lpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_lpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_lpa <- ggarrange(phq_24h_goodsleep[[3]], phq_24h_insomnia_mild[[3]], 
                                  phq_24h_insomnia_moderate[[3]], phq_24h_insomnia_severe[[3]], 
                                  nrow = 4,
                                  common.legend = TRUE,
                                  legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_lpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sb reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sb", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_sb <- ggarrange(phq_24h_goodsleep[[4]], phq_24h_insomnia_mild[[4]], 
                                 phq_24h_insomnia_moderate[[4]], phq_24h_insomnia_severe[[4]], 
                                 nrow = 4,
                                 common.legend = TRUE,
                                 legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sb, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# PHQ by 4 insomnia types and sleep period
## substitution results -----------------
m_phq_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

sub_models_resp   <- c("m_phq_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep", "_insomnia_mild", "_insomnia_moderate", "_insomnia_severe")

sub_models_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period") else if (j == 2) ("Medium Sleep Period") else ("Long Sleep Period")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("Good Sleep") else if (k == 2) ("Mild Insomnia") else if (k == 3) ("Moderate Insomnia") else ("Severe Insomnia")
      
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
                                     levels = c("Good Sleep",
                                                "Mild Insomnia",
                                                "Moderate Insomnia",
                                                "Severe Insomnia"))]
      
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

## plot add ins ----------------
col_sleepq <- c(
  `Short Sleep Period (Q1)` = "#456691",
  `Medium Sleep Period (Q2 and Q3)` = "#978787",
  `Long Sleep Period (Q4)` = "#8AAFCA" 
)
colf_sleepq <- c(
  `Short Sleep Period (Q1)` = "#8399AE",
  `Medium Sleep Period (Q2 and Q3)` = "#DCD5CE",
  `Long Sleep Period (Q4)` = "#A1B2C2"
)

alpha <- 1/10
### phq insomnia by short sleep ---------------
phq_24h_insomnia_q1 <- foreach(i = seq_len(nrow(rg_phq)),
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Short Sleep Period (Q1)"], aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = Insomnia),
                                               alpha = alpha, show.legend = TRUE) +
                                   geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                   geom_text(aes(label = Sig, colour = Insomnia),
                                             size = 6, 
                                             position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                    y = 0.005),
                                             show.legend = FALSE) +
                                   # facet_wrap(~ From, strip.position = "left") +
                                   facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                              labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                              strip.position = "bottom") +
                                   labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                        y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                   scale_x_continuous(limits = c(-23, 23),
                                                      breaks = c(-20, 0, 20)) +
                                   # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
                                   scale_colour_manual(values = col_sleepq,
                                                       drop = FALSE) +
                                   scale_fill_manual(values = colf_sleepq,
                                                     drop = FALSE) +
                                   hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_q1) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_q1
saveRDS(phq_24h_insomnia_q1, paste0(outputdir, "phq_24h_insomnia_q1", ".RDS"))


### phq insomnia by medium sleep ---------------
phq_24h_insomnia_q2 <- foreach(i = seq_len(nrow(rg_phq)),
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Medium Sleep Period (Q2 and Q3)"], aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = Insomnia),
                                               alpha = alpha, show.legend = TRUE) +
                                   geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                   geom_text(aes(label = Sig, colour = Insomnia),
                                             size = 6, 
                                             position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                    y = 0.005),
                                             show.legend = FALSE) +
                                   # facet_wrap(~ From, strip.position = "left") +
                                   facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                              labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                              strip.position = "bottom") +
                                   labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                        y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                   scale_x_continuous(limits = c(-23, 23),
                                                      breaks = c(-20, 0, 20)) +
                                   # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
                                   scale_colour_manual(values = col_sleepq,
                                                       drop = FALSE) +
                                   scale_fill_manual(values = colf_sleepq,
                                                     drop = FALSE) +
                                   hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_q2) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_q2
saveRDS(phq_24h_insomnia_q2, paste0(outputdir, "phq_24h_insomnia_q2", ".RDS"))

### phq insomnia by long sleep ---------------
phq_24h_insomnia_q3 <- foreach(i = seq_len(nrow(rg_phq)),
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Long Sleep Period (Q4)"], aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = Insomnia),
                                               alpha = alpha, show.legend = TRUE) +
                                   geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                   geom_text(aes(label = Sig, colour = Insomnia),
                                             size = 6, 
                                             position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                    y = 0.005),
                                             show.legend = FALSE) +
                                   # facet_wrap(~ From, strip.position = "left") +
                                   facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                              labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                              strip.position = "bottom") +
                                   labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                        y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                   scale_x_continuous(limits = c(-23, 23),
                                                      breaks = c(-20, 0, 20)) +
                                   # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
                                   scale_colour_manual(values = col_sleepq,
                                                       drop = FALSE) +
                                   scale_fill_manual(values = colf_sleepq,
                                                     drop = FALSE) +
                                   hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_q3) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_q3
saveRDS(phq_24h_insomnia_q3, paste0(outputdir, "phq_24h_insomnia_q3", ".RDS"))

# PHQ by sleep period and 3 insomnia types ----------------
## substitution results -----------------
m_phq_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

sub_models_resp   <- c("m_phq_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep", "_insomnia_mild", "_insomnia_persistent")

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
      insomnia <- if (k == 1) ("Good Sleep") else if (k == 2) ("Acute Insomnia") else ("Chronic Insomnia")
      
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
                                     levels = c("Good Sleep",
                                                "Acute Insomnia",
                                                "Chronic Insomnia"))]
      
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
## plot add ins ----------------
col_sleepq <- c(
  `Short Sleep Period (Q1)` = "#456691",
  `Medium Sleep Period (Q2 and Q3)` = "#978787",
  `Long Sleep Period (Q4)` = "#8AAFCA" 
)
colf_sleepq <- c(
  `Short Sleep Period (Q1)` = "#8399AE",
  `Medium Sleep Period (Q2 and Q3)` = "#DCD5CE",
  `Long Sleep Period (Q4)` = "#A1B2C2"
)

col_insomnia3 <- c(
  `Good Sleep` = "#978787", 
  `Acute Insomnia` = "#8AAFCA",
  `Chronic Insomnia` = "#456691" 
)
colf_insomnia3 <- c(
  `Good Sleep` = "#DCD5CE",
  `Acute Insomnia` = "#A1B2C2",
  `Chronic Insomnia` = "#8399AE" 
)

alpha <- 1/10

### phq good sleep by sleep period ---------------
phq_24h_goodsleep <- foreach(i = seq_len(nrow(rg_phq)),
                             .packages = "multilevelcoda") %dopar% {
                               
                               ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Good Sleep"], aes(x = Delta, y = Mean)) +
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
                                 facet_wrap(ggplot2::vars(From, To, Insomnia),
                                            labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                            strip.position = "bottom") +
                                 labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                      y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                 scale_x_continuous(limits = c(-23, 23),
                                                    breaks = c(-20, 0, 20)) +
                                 scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
                                 scale_colour_manual(values = col_sleepq,
                                                     drop = FALSE) +
                                 scale_fill_manual(values = colf_sleepq,
                                                   drop = FALSE) +
                                 hrbrthemes::theme_ipsum() +
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

names(phq_24h_goodsleep) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_goodsleep
saveRDS(phq_24h_goodsleep, paste0(outputdir, "phq_24h_goodsleep", ".RDS"))

### phq acute insomnia by sleep period ---------------
phq_24h_insomnia_acute <- foreach(i = seq_len(nrow(rg_phq)),
                                  .packages = "multilevelcoda") %dopar% {
                                    
                                    ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Acute Insomnia"], aes(x = Delta, y = Mean)) +
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
                                      facet_wrap(ggplot2::vars(From, To, Insomnia),
                                                 labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                                 strip.position = "bottom") +
                                      labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                           y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                      scale_x_continuous(limits = c(-23, 23),
                                                         breaks = c(-20, 0, 20)) +
                                      # scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4)) +
                                      scale_colour_manual(values = col_sleepq,
                                                          drop = FALSE) +
                                      scale_fill_manual(values = colf_sleepq,
                                                        drop = FALSE) +
                                      hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_acute) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_acute
saveRDS(phq_24h_insomnia_acute, paste0(outputdir, "phq_24h_insomnia_acute", ".RDS"))

### phq chronic insomnia by sleep period ---------------
phq_24h_insomnia_chronic <- foreach(i = seq_len(nrow(rg_phq)),
                                    .packages = "multilevelcoda") %dopar% {
                                      
                                      ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & Insomnia == "Chronic Insomnia"], aes(x = Delta, y = Mean)) +
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
                                        facet_wrap(ggplot2::vars(From, To, Insomnia),
                                                   labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(Insomnia)) ~ phantom() %->% .(as.character(To))),
                                                   strip.position = "bottom") +
                                        labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                             y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                        scale_x_continuous(limits = c(-26, 26),
                                                           breaks = c(-25, 0, 25)) +
                                        # scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1)) +
                                        scale_colour_manual(values = col_sleepq,
                                                            drop = FALSE) +
                                        scale_fill_manual(values = colf_sleepq,
                                                          drop = FALSE) +
                                        hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia_chronic) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_chronic
saveRDS(phq_24h_insomnia_chronic, paste0(outputdir, "phq_24h_insomnia_chronic", ".RDS"))

## save -----
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_goodsleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_goodsleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_goodsleep[[2]], 
                                     phq_24h_goodsleep[[3]], phq_24h_goodsleep[[4]], 
                                     nrow = 4,
                                     common.legend = TRUE,
                                     legend   = "bottom"
)
annotate_figure(patch_phq_24h_goodsleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_mild", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_mild <- ggarrange(phq_24h_insomnia_mild[[1]], phq_24h_insomnia_mild[[2]], 
                                         phq_24h_insomnia_mild[[3]], phq_24h_insomnia_mild[[4]], 
                                         nrow = 4,
                                         common.legend = TRUE,
                                         legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_mild, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_moderate", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_moderate <- ggarrange(phq_24h_insomnia_moderate[[1]], phq_24h_insomnia_moderate[[2]], 
                                             phq_24h_insomnia_moderate[[3]], phq_24h_insomnia_moderate[[4]], 
                                             nrow = 4,
                                             common.legend = TRUE,
                                             legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_moderate, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_severe", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_severe <- ggarrange(phq_24h_insomnia_severe[[1]], phq_24h_insomnia_severe[[2]], 
                                           phq_24h_insomnia_severe[[3]], phq_24h_insomnia_severe[[4]], 
                                           nrow = 4,
                                           common.legend = TRUE,
                                           legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_severe, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sleep reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_sleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_insomnia_mild[[1]], 
                                    phq_24h_insomnia_moderate[[1]], phq_24h_insomnia_severe[[1]], 
                                    nrow = 4,
                                    common.legend = TRUE,
                                    legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# mvpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_mvpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_mvpa <- ggarrange(phq_24h_goodsleep[[2]], phq_24h_insomnia_mild[[2]], 
                                   phq_24h_insomnia_moderate[[2]], phq_24h_insomnia_severe[[2]], 
                                   nrow = 4,
                                   common.legend = TRUE,
                                   legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_mvpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# lpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_lpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_lpa <- ggarrange(phq_24h_goodsleep[[3]], phq_24h_insomnia_mild[[3]], 
                                  phq_24h_insomnia_moderate[[3]], phq_24h_insomnia_severe[[3]], 
                                  nrow = 4,
                                  common.legend = TRUE,
                                  legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_lpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sb reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sb", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_to_sb <- ggarrange(phq_24h_goodsleep[[4]], phq_24h_insomnia_mild[[4]], 
                                 phq_24h_insomnia_moderate[[4]], phq_24h_insomnia_severe[[4]], 
                                 nrow = 4,
                                 common.legend = TRUE,
                                 legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sb, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# PHQ by 3 insomnia types and sleep period ----------------
## substitution results -----------------
m_phq_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

sub_models_resp   <- c("m_phq_gam_sub_sleep")
sleep_q_type <- c("_q1", "_q2", "_q3")
sleep_insomnia_type <- c("_goodsleep", "_insomnia_mild", "_insomnia_persistent")

sub_models_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period") else if (j == 2) ("Medium Sleep Period") else ("Long Sleep Period")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("Good Sleep") else if (k == 2) ("Acute Insomnia") else ("Chronic Insomnia")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = c(-22:22), level = "aggregate", digits = "asis"))
      
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
                                     levels = c("Good Sleep",
                                                "Acute Insomnia",
                                                "Chronic Insomnia"))]
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-20, 20), "*", "")]
      
      sub_models_sleep_i_type[[k]] <- model_tmp
    }
    sub_models_sleep_i_type_k <- rbindlist(sub_models_sleep_i_type)
    sub_models_sleep_q_type[[j]] <- sub_models_sleep_i_type_k
  }
  sub_models_sleep_q_type_i  <- rbindlist(sub_models_sleep_q_type)
  sub_models_sleep_q_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                    levels = c("Short Sleep Period",
                                                               "Medium Sleep Period",
                                                               "Long Sleep Period"))]
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
## plot add ins ----------------
col_sleepq <- c(
  `Short Sleep Period` = "#456691",
  `Medium Sleep Period` = "#978787",
  `Long Sleep Period` = "#8AAFCA" 
)
colf_sleepq <- c(
  `Short Sleep Period` = "#8399AE",
  `Medium Sleep Period` = "#DCD5CE",
  `Long Sleep Period` = "#A1B2C2"
)

col_insomnia3 <- c(
  `Good Sleep` = "#978787", 
  `Acute Insomnia` = "#8AAFCA",
  `Chronic Insomnia` = "#456691" 
)
colf_insomnia3 <- c(
  `Good Sleep` = "#DCD5CE",
  `Acute Insomnia` = "#A1B2C2",
  `Chronic Insomnia` = "#8399AE" 
)

alpha <- 1/10
### phq insomnia 3 by short sleep ---------------
phq_24h_insomnia3_q1 <- foreach(i = seq_len(nrow(rg_phq)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Short Sleep Period"], aes(x = Delta, y = Mean)) +
                                    geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_ribbon(aes(ymin = CI_low,
                                                    ymax = CI_high, fill = Insomnia),
                                                alpha = alpha, show.legend = TRUE) +
                                    geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                    geom_text(aes(label = Sig, colour = Insomnia),
                                              size = 6, 
                                              position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                     y = 0.005),
                                              show.legend = FALSE) +
                                    # facet_wrap(~ From, strip.position = "left") +
                                    facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                               labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                               strip.position = "bottom") +
                                    labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    scale_y_continuous(limits = c(-1, 2.5),
                                                       breaks = c(-1, 0, 1, 2)) +
                                    scale_colour_manual(values = col_insomnia3,
                                                        drop = FALSE) +
                                    scale_fill_manual(values = colf_insomnia3,
                                                      drop = FALSE) +
                                    hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia3_q1) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"], ", ", "Short Sleep Period")
}
phq_24h_insomnia3_q1
saveRDS(phq_24h_insomnia3_q1, paste0(outputdir, "phq_24h_insomnia3_q1", ".RDS"))

### phq insomnia 3 by medium sleep ---------------
phq_24h_insomnia3_q2 <- foreach(i = seq_len(nrow(rg_phq)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Medium Sleep Period"], aes(x = Delta, y = Mean)) +
                                    geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_ribbon(aes(ymin = CI_low,
                                                    ymax = CI_high, fill = Insomnia),
                                                alpha = alpha, show.legend = TRUE) +
                                    geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                    geom_text(aes(label = Sig, colour = Insomnia),
                                              size = 6, 
                                              position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                     y = 0.005),
                                              show.legend = FALSE) +
                                    # facet_wrap(~ From, strip.position = "left") +
                                    facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                               labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                               strip.position = "bottom") +
                                    labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    scale_y_continuous(limits = c(-1, 2.5),
                                                       breaks = c(-1, 0, 1, 2)) +
                                    scale_colour_manual(values = col_insomnia3,
                                                        drop = FALSE) +
                                    scale_fill_manual(values = colf_insomnia3,
                                                      drop = FALSE) +
                                    hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia3_q2) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"], ", ", "Medium Sleep Period")
}
phq_24h_insomnia3_q2
saveRDS(phq_24h_insomnia3_q2, paste0(outputdir, "phq_24h_insomnia3_q2", ".RDS"))

### phq insomnia 3 by long sleep ---------------
phq_24h_insomnia3_q3 <- foreach(i = seq_len(nrow(rg_phq)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"]) & SleepPeriod == "Long Sleep Period"], aes(x = Delta, y = Mean)) +
                                    geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_ribbon(aes(ymin = CI_low,
                                                    ymax = CI_high, fill = Insomnia),
                                                alpha = alpha, show.legend = TRUE) +
                                    geom_line(aes(colour = Insomnia), linewidth = 1, show.legend = TRUE) +
                                    geom_text(aes(label = Sig, colour = Insomnia),
                                              size = 6, 
                                              position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                     y = 0.005),
                                              show.legend = FALSE) +
                                    facet_wrap(ggplot2::vars(From, To, SleepPeriod),
                                               labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ .(as.character(SleepPeriod)) ~ phantom() %->% .(as.character(To))),
                                               strip.position = "bottom") +
                                    labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    scale_y_continuous(limits = c(-1, 2.5),
                                                       breaks = c(-1, 0, 1, 2)) +
                                    scale_colour_manual(values = col_insomnia3,
                                                        drop = FALSE) +
                                    scale_fill_manual(values = colf_insomnia3,
                                                      drop = FALSE) +
                                    hrbrthemes::theme_ipsum() +
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

names(phq_24h_insomnia3_q3) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time of ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"], ", ", "Long Sleep Period")
}
phq_24h_insomnia3_q3
saveRDS(phq_24h_insomnia3_q3, paste0(outputdir, "phq_24h_insomnia3_q3", ".RDS"))

## save -----
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_goodsleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_goodsleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_goodsleep[[2]], 
                                     phq_24h_goodsleep[[3]], phq_24h_goodsleep[[4]], 
                                     nrow = 4,
                                     common.legend = TRUE,
                                     legend   = "bottom"
)
annotate_figure(patch_phq_24h_goodsleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_acute", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_acute <- ggarrange(phq_24h_insomnia_acute[[1]], phq_24h_insomnia_acute[[2]], 
                                          phq_24h_insomnia_acute[[3]], phq_24h_insomnia_acute[[4]], 
                                          nrow = 4,
                                          common.legend = TRUE,
                                          legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_acute, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_chronic", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia_chronic <- ggarrange(phq_24h_insomnia_chronic[[1]], phq_24h_insomnia_chronic[[2]], 
                                            phq_24h_insomnia_chronic[[3]], phq_24h_insomnia_chronic[[4]], 
                                            nrow = 4,
                                            common.legend = TRUE,
                                            legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia_chronic, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia_severe", ".pdf"),
  width = 9,
  height = 12.5,
)

# sleep reallocation - by sleep period
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sleep_sleepq", ".pdf"),
  width = 9,
  height = 10,
)

patch_phq_24h_to_sleep <- ggarrange(phq_24h_goodsleep[[1]], phq_24h_insomnia_acute[[1]], phq_24h_insomnia_chronic[[1]], 
                                    nrow = 3,
                                    common.legend = TRUE,
                                    legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# mvpa reallocation - by sleep period
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_mvpa_sleepq", ".pdf"),
  width = 9,
  height = 10,
)

patch_phq_24h_to_mvpa <- ggarrange(phq_24h_goodsleep[[2]], phq_24h_insomnia_acute[[2]], phq_24h_insomnia_chronic[[2]], 
                                   nrow = 3,
                                   common.legend = TRUE,
                                   legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_mvpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# lpa reallocation - by sleep period
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_lpa_sleepq", ".pdf"),
  width = 9,
  height = 10,
)

patch_phq_24h_to_lpa <- ggarrange(phq_24h_goodsleep[[3]], phq_24h_insomnia_acute[[3]], phq_24h_insomnia_chronic[[3]], 
                                  nrow = 3,
                                  common.legend = TRUE,
                                  legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_lpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sb reallocation - by sleep period
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sb_sleepq", ".pdf"),
  width = 9,
  height = 10,
)

patch_phq_24h_to_sb <- ggarrange(phq_24h_goodsleep[[4]], phq_24h_insomnia_acute[[4]], phq_24h_insomnia_chronic[[4]], 
                                 nrow = 3,
                                 common.legend = TRUE,
                                 legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sb, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# insomnia q1
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia3_q1", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia3_q1 <- ggarrange(phq_24h_insomnia3_q1[[1]], phq_24h_insomnia3_q1[[2]], 
                                        phq_24h_insomnia3_q1[[3]], phq_24h_insomnia3_q1[[4]],
                                        nrow = 4,
                                        common.legend = TRUE,
                                        legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia3_q1, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# insomnia q2
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia3_q2", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia3_q2 <- ggarrange(phq_24h_insomnia3_q2[[1]], phq_24h_insomnia3_q2[[2]], 
                                        phq_24h_insomnia3_q2[[3]], phq_24h_insomnia3_q2[[4]],
                                        nrow = 4,
                                        common.legend = TRUE,
                                        legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia3_q2, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# insomnia q3
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_insomnia3_q3", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq_24h_insomnia3_q3 <- ggarrange(phq_24h_insomnia3_q3[[1]], phq_24h_insomnia3_q3[[2]], 
                                        phq_24h_insomnia3_q3[[3]], phq_24h_insomnia3_q3[[4]],
                                        nrow = 4,
                                        common.legend = TRUE,
                                        legend   = "bottom"
)
annotate_figure(patch_phq_24h_insomnia3_q3, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sleep reallocation - by insomnia
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sleep_insomnia3", ".pdf"),
  width = 9,
  height = 11,
)

patch_phq_24h_to_sleep <- ggarrange(phq_24h_insomnia3_q1[[1]], phq_24h_insomnia3_q2[[1]], phq_24h_insomnia3_q3[[1]], 
                                    nrow = 3,
                                    common.legend = TRUE,
                                    legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# mvpa reallocation - by insomnia
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_mvpa_insomnia3", ".pdf"),
  width = 9,
  height = 11,
)

patch_phq_24h_to_mvpa <- ggarrange(phq_24h_insomnia3_q1[[2]], phq_24h_insomnia3_q2[[2]], phq_24h_insomnia3_q3[[2]], 
                                   nrow = 3,
                                   common.legend = TRUE,
                                   legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_mvpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# lpa reallocation - by insomnia
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_lpa_insomnia3", ".pdf"),
  width = 9,
  height = 11,
)

patch_phq_24h_to_lpa <- ggarrange(phq_24h_insomnia3_q1[[3]], phq_24h_insomnia3_q2[[3]], phq_24h_insomnia3_q3[[3]], 
                                  nrow = 3,
                                  common.legend = TRUE,
                                  legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_lpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sb reallocation - by insomnia
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_to_sb_insomnia3", ".pdf"),
  width = 9,
  height = 11,
)

patch_phq_24h_to_sb <- ggarrange(phq_24h_insomnia3_q1[[4]], phq_24h_insomnia3_q2[[4]], phq_24h_insomnia3_q3[[4]], 
                                 nrow = 3,
                                 common.legend = TRUE,
                                 legend   = "bottom"
)
annotate_figure(patch_phq_24h_to_sb, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()