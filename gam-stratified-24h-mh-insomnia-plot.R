source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# PHQ ----------------------------------
# substitution results -----------------
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

col <- c(
  "#1C1718", "#2A3E59",
  "#456691", "#647F9A",
  "#8CAACB", "#DCD5CE",
  "#DAA5AE", "#b6485d",
  "#944C4C", "#C99696",
  "#bf5b4b", "#bb847a",
  "#A69188", "#EAD3BF",
  "#FAD899", "#8DA290",
  "#133A1B", "#6d765b",
  "#3b4031", "#3d251e"
)
colf <- c(
  "#1C1718", "#2A3E59",
  "#456691", "#647F9A",
  "#8CAACB", "#DCD5CE",
  "#DAA5AE", "#b6485d",
  "#944C4C", "#C99696",
  "#bf5b4b", "#bb847a",
  "#A69188", "#EAD3BF",
  "#FAD899", "#8DA290",
  "#133A1B", "#6d765b",
  "#3b4031", "#3d251e"
)
alpha <- 1/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA ", " SB ")
part_labels    <- c("Sleep", "MVPA", 
                    "LPA ", " SB ")

# phq by sleep period----------------
sub_models_phq_insomnia <- grep("phq", names(sub_models_insomnia), value = T)
phq            <- "Depressive Symptoms"
rg_phq <- expand.grid.df(data.frame(sub_models_phq_insomnia, phq), 
                         data.frame(parts, part_labels))

# dp <- sub_models_insomnia[[1]][Delta %in% c(-20, 20)]
# dp[, `SleepPeriod and Insomnia` := paste0(SleepPeriod, " ", Insomnia)]
# 
# ggplot(sub_models_insomnia[[1]][To == "Sleep"], aes(x = Delta, y = Mean)) +
#   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
#   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high,
#                   fill = factor(interaction(SleepPeriod, Insomnia))),
#               alpha = alpha, show.legend = TRUE) +
#   geom_line(aes(colour = factor(interaction(SleepPeriod, Insomnia))), linewidth = 1, show.legend = TRUE) +
#   geom_text(aes(label = Sig, colour = factor(interaction(SleepPeriod, Insomnia))),
#             size = 6, 
#             position = ggpp::position_nudge_center(center_x = 0, x = 3, 
#                                                    y = 0.005),
#             show.legend = FALSE) +
#   facet_wrap(ggplot2::vars(From, To, Insomnia),
#              scale = "free") +
#   scale_colour_manual(values = col,
#                       drop = FALSE) +
#   scale_fill_manual(values = colf,
#                     drop = FALSE) +
#   hrbrthemes::theme_ipsum() +
#   theme(
#     axis.ticks        = element_blank(),
#     panel.background  = element_blank(),
#     panel.border      = element_blank(),
#     panel.grid.major  = element_blank(),
#     panel.grid.minor  = element_blank(),
#     plot.background   = element_rect(fill = "transparent", colour = NA),
#     strip.background  = element_rect(fill = "transparent", colour = NA),
#     strip.text        = element_text(size = 12, face = "bold", hjust = .5),
#     strip.placement   = "outside",
#     axis.title.x      = element_blank(),
#     axis.title.y      = element_blank(),
#     plot.margin       = margin(.5, .5, .5, .5, "cm"),
#     legend.title      = element_blank(),
#     legend.text       = element_text(size = 13, face = "bold", hjust = .5),
#     legend.position   = "bottom"
#   )

phq_24h_insomnia_all <- foreach(i = seq_len(nrow(rg_phq)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq[i, "sub_models_phq_insomnia"]]][To == eval(rg_phq[i, "parts"])], aes(x = Delta, y = Mean)) +
                                    geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                    geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                    # geom_ribbon(aes(ymin = CI_low,
                                    #                 ymax = CI_high, fill = SleepPeriod),
                                    #             alpha = alpha, show.legend = TRUE) +
                                    geom_line(aes(colour = SleepPeriod), linewidth = 1, show.legend = TRUE) +
                                    geom_text(aes(label = Sig, colour = SleepPeriod),
                                              size = 6, 
                                              position = ggpp::position_nudge_center(center_x = 0, x = 3, 
                                                                                     y = 0.005),
                                              show.legend = FALSE) +
                                    # facet_wrap(~ From, strip.position = "left") +
                                    facet_wrap(ggplot2::vars(From, To, Insomnia),
                                               labeller = label_bquote(cols = .(as.character(From)) %<-% .(as.character(Insomnia)) %->% .(as.character(To))),
                                               strip.position = "bottom",
                                               scale = "free") +
                                    labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq[i, "phq"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    # scale_y_continuous(n.breaks = 3) +
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

names(phq_24h_insomnia_all) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_all
saveRDS(phq_24h_insomnia_all, paste0(outputdir, "phq_24h_insomnia_all", ".RDS"))

# good sleep ---------------
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

names(phq_24h_goodsleep) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_goodsleep
saveRDS(phq_24h_goodsleep, paste0(outputdir, "phq_24h_goodsleep", ".RDS"))

# mild insomnia ---------------
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

names(phq_24h_insomnia_mild) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_mild
saveRDS(phq_24h_insomnia_mild, paste0(outputdir, "phq_24h_insomnia_mild", ".RDS"))

# moderate insomnia ---------------
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

names(phq_24h_insomnia_moderate) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_moderate
saveRDS(phq_24h_insomnia_moderate, paste0(outputdir, "phq_24h_insomnia_moderate", ".RDS"))

# severe insomnia ---------------
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

names(phq_24h_insomnia_severe) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_insomnia_severe
saveRDS(phq_24h_insomnia_severe, paste0(outputdir, "phq_24h_insomnia_severe", ".RDS"))


# save -----

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

# PHQ8 --------------------------
# substitution results -----------------
m_phq8_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq8_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq8_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq8_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq8_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_phq8_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_phq8_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_phq8_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_phq8_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_phq8_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq8_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq8_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq8_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

sub_models_resp   <- c("m_phq8_gam_sub_sleep")
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

col <- c(
  `Good Sleep` = "#456691",
  `Mild Insomnia` = "#978787",
  `Moderate Insomnia` = "#DCD5CE",
  `Severe Insomnia` = "#8AAFCA" 
)
colf <- c(
  `Good Sleep` = "#456691",
  `Mild Insomnia` = "#978787",
  `Moderate Insomnia` = "#DCD5CE",
  `Severe Insomnia` = "#8AAFCA" 
)
alpha <- 1/10

# make a grid to loop plots
parts          <- c("Sleep", "MVPA", "LPA ", " SB ")
part_labels    <- c("Sleep", "MVPA", 
                    "LPA ", " SB ")

# phq8 by sleep period and insomnia ----------------
sub_models_phq8_insomnia <- grep("phq8", names(sub_models_insomnia), value = T)
phq8            <- "Depressive Symptoms"
rg_phq8 <- expand.grid.df(data.frame(sub_models_phq8_insomnia, phq8), 
                          data.frame(parts, part_labels))

# test #
# dp <- sub_models_insomnia[[1]][Delta %in% c(-20, 20)]
# dp[, `SleepPeriod and Insomnia` := paste0(SleepPeriod, " ", Insomnia)]
# 
# ggplot(sub_models_insomnia[[1]][To == "Sleep"], aes(x = Delta, y = Mean)) +
#   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
#   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high,
#                   fill = factor(interaction(SleepPeriod, Insomnia))),
#               alpha = alpha, show.legend = TRUE) +
#   geom_line(aes(colour = factor(interaction(SleepPeriod, Insomnia))), linewidth = 1, show.legend = TRUE) +
#   geom_text(aes(label = Sig, colour = factor(interaction(SleepPeriod, Insomnia))),
#             size = 6, 
#             position = ggpp::position_nudge_center(center_x = 0, x = 3, 
#                                                    y = 0.005),
#             show.legend = FALSE) +
#   facet_wrap(ggplot2::vars(From, To, Insomnia),
#              scale = "free") +
#   scale_colour_manual(values = col,
#                       drop = FALSE) +
#   scale_fill_manual(values = colf,
#                     drop = FALSE) +
#   hrbrthemes::theme_ipsum() +
#   theme(
#     axis.ticks        = element_blank(),
#     panel.background  = element_blank(),
#     panel.border      = element_blank(),
#     panel.grid.major  = element_blank(),
#     panel.grid.minor  = element_blank(),
#     plot.background   = element_rect(fill = "transparent", colour = NA),
#     strip.background  = element_rect(fill = "transparent", colour = NA),
#     strip.text        = element_text(size = 12, face = "bold", hjust = .5),
#     strip.placement   = "outside",
#     axis.title.x      = element_blank(),
#     axis.title.y      = element_blank(),
#     plot.margin       = margin(.5, .5, .5, .5, "cm"),
#     legend.title      = element_blank(),
#     legend.text       = element_text(size = 13, face = "bold", hjust = .5),
#     legend.position   = "bottom"
#   )

phq8_24h_insomnia_all <- foreach(i = seq_len(nrow(rg_phq8)),
                                 .packages = "multilevelcoda") %dopar% {
                                   
                                   ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"])], aes(x = Delta, y = Mean)) +
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
                                                labeller = label_bquote(cols = .(as.character(From)) %<-% .(as.character(Insomnia)) %->% .(as.character(To))),
                                                strip.position = "bottom",
                                                scale = "free") +
                                     labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                          y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                     scale_x_continuous(limits = c(-23, 23),
                                                        breaks = c(-20, 0, 20)) +
                                     # scale_y_continuous(n.breaks = 3) +
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

names(phq8_24h_insomnia_all) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_all
saveRDS(phq8_24h_insomnia_all, paste0(outputdir, "phq8_24h_insomnia_all", ".RDS"))

# sleep period by good sleep ---------------
phq8_24h_goodsleep <- foreach(i = seq_len(nrow(rg_phq8)),
                              .packages = "multilevelcoda") %dopar% {
                                
                                ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & Insomnia == "Good Sleep"], aes(x = Delta, y = Mean)) +
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
                                  labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                       y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                  scale_x_continuous(limits = c(-23, 23),
                                                     breaks = c(-20, 0, 20)) +
                                  scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
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

names(phq8_24h_goodsleep) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_goodsleep
saveRDS(phq8_24h_goodsleep, paste0(outputdir, "phq8_24h_goodsleep", ".RDS"))

# sleep period by mild insomnia ---------------
phq8_24h_insomnia_mild <- foreach(i = seq_len(nrow(rg_phq8)),
                                  .packages = "multilevelcoda") %dopar% {
                                    
                                    ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & Insomnia == "Mild Insomnia"], aes(x = Delta, y = Mean)) +
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
                                      labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                           y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                      scale_x_continuous(limits = c(-23, 23),
                                                         breaks = c(-20, 0, 20)) +
                                      scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4)) +
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

names(phq8_24h_insomnia_mild) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_mild
saveRDS(phq8_24h_insomnia_mild, paste0(outputdir, "phq8_24h_insomnia_mild", ".RDS"))

# sleep period by moderate insomnia ---------------
phq8_24h_insomnia_moderate <- foreach(i = seq_len(nrow(rg_phq8)),
                                      .packages = "multilevelcoda") %dopar% {
                                        
                                        ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & Insomnia == "Moderate Insomnia"], aes(x = Delta, y = Mean)) +
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
                                          labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                               y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                          scale_x_continuous(limits = c(-23, 23),
                                                             breaks = c(-20, 0, 20)) +
                                          scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1)) +
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

names(phq8_24h_insomnia_moderate) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_moderate
saveRDS(phq8_24h_insomnia_moderate, paste0(outputdir, "phq8_24h_insomnia_moderate", ".RDS"))

# sleep period by severe insomnia ---------------
phq8_24h_insomnia_severe <- foreach(i = seq_len(nrow(rg_phq8)),
                                    .packages = "multilevelcoda") %dopar% {
                                      
                                      ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & Insomnia == "Severe Insomnia"], aes(x = Delta, y = Mean)) +
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
                                        labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                             y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                        scale_x_continuous(limits = c(-23, 23),
                                                           breaks = c(-20, 0, 20)) +
                                        scale_y_continuous(breaks = c(-1, 0, 1, 2)) +
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

names(phq8_24h_insomnia_severe) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_severe
saveRDS(phq8_24h_insomnia_severe, paste0(outputdir, "phq8_24h_insomnia_severe", ".RDS"))


# save -----

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_goodsleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_goodsleep <- ggarrange(phq8_24h_goodsleep[[1]], phq8_24h_goodsleep[[2]], 
                                      phq8_24h_goodsleep[[3]], phq8_24h_goodsleep[[4]], 
                                      nrow = 4,
                                      common.legend = TRUE,
                                      legend   = "bottom"
)
annotate_figure(patch_phq8_24h_goodsleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_insomnia_mild", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_insomnia_mild <- ggarrange(phq8_24h_insomnia_mild[[1]], phq8_24h_insomnia_mild[[2]], 
                                          phq8_24h_insomnia_mild[[3]], phq8_24h_insomnia_mild[[4]], 
                                          nrow = 4,
                                          common.legend = TRUE,
                                          legend   = "bottom"
)
annotate_figure(patch_phq8_24h_insomnia_mild, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_insomnia_moderate", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_insomnia_moderate <- ggarrange(phq8_24h_insomnia_moderate[[1]], phq8_24h_insomnia_moderate[[2]], 
                                              phq8_24h_insomnia_moderate[[3]], phq8_24h_insomnia_moderate[[4]], 
                                              nrow = 4,
                                              common.legend = TRUE,
                                              legend   = "bottom"
)
annotate_figure(patch_phq8_24h_insomnia_moderate, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_insomnia_severe", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_insomnia_severe <- ggarrange(phq8_24h_insomnia_severe[[1]], phq8_24h_insomnia_severe[[2]], 
                                            phq8_24h_insomnia_severe[[3]], phq8_24h_insomnia_severe[[4]], 
                                            nrow = 4,
                                            common.legend = TRUE,
                                            legend   = "bottom"
)
annotate_figure(patch_phq8_24h_insomnia_severe, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sleep reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_to_sleep", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_to_sleep <- ggarrange(phq8_24h_goodsleep[[1]], phq8_24h_insomnia_mild[[1]], 
                                     phq8_24h_insomnia_moderate[[1]], phq8_24h_insomnia_severe[[1]], 
                                     nrow = 4,
                                     common.legend = TRUE,
                                     legend   = "bottom"
)
annotate_figure(patch_phq8_24h_to_sleep, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# mvpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_to_mvpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_to_mvpa <- ggarrange(phq8_24h_goodsleep[[2]], phq8_24h_insomnia_mild[[2]], 
                                    phq8_24h_insomnia_moderate[[2]], phq8_24h_insomnia_severe[[2]], 
                                    nrow = 4,
                                    common.legend = TRUE,
                                    legend   = "bottom"
)
annotate_figure(patch_phq8_24h_to_mvpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# lpa reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_to_lpa", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_to_lpa <- ggarrange(phq8_24h_goodsleep[[3]], phq8_24h_insomnia_mild[[3]], 
                                   phq8_24h_insomnia_moderate[[3]], phq8_24h_insomnia_severe[[3]], 
                                   nrow = 4,
                                   common.legend = TRUE,
                                   legend   = "bottom"
)
annotate_figure(patch_phq8_24h_to_lpa, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# sb reallocation
grDevices::cairo_pdf(
  file = paste0(outputdir, "phq8_24h_to_sb", ".pdf"),
  width = 9,
  height = 12.5,
)

patch_phq8_24h_to_sb <- ggarrange(phq8_24h_goodsleep[[4]], phq8_24h_insomnia_mild[[4]], 
                                  phq8_24h_insomnia_moderate[[4]], phq8_24h_insomnia_severe[[4]], 
                                  nrow = 4,
                                  common.legend = TRUE,
                                  legend   = "bottom"
)
annotate_figure(patch_phq8_24h_to_sb, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# insomnia by short sleep ---------------
phq8_24h_insomnia_q1 <- foreach(i = seq_len(nrow(rg_phq8)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & SleepPeriod == "Short Sleep Period (Q1)"], aes(x = Delta, y = Mean)) +
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
                                    labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
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

names(phq8_24h_insomnia_q1) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_q1
saveRDS(phq8_24h_insomnia_q1, paste0(outputdir, "phq8_24h_insomnia_q1", ".RDS"))


# insomnia by medium sleep ---------------
phq8_24h_insomnia_q2 <- foreach(i = seq_len(nrow(rg_phq8)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & SleepPeriod == "Medium Sleep Period (Q2 and Q3)"], aes(x = Delta, y = Mean)) +
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
                                    labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
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

names(phq8_24h_insomnia_q2) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_q2
saveRDS(phq8_24h_insomnia_q2, paste0(outputdir, "phq8_24h_insomnia_q2", ".RDS"))

# insomnia by long sleep ---------------
phq8_24h_insomnia_q3 <- foreach(i = seq_len(nrow(rg_phq8)),
                                .packages = "multilevelcoda") %dopar% {
                                  
                                  ggplot(sub_models_insomnia[[rg_phq8[i, "sub_models_phq8_insomnia"]]][To == eval(rg_phq8[i, "parts"]) & SleepPeriod == "Long Sleep Period (Q4)"], aes(x = Delta, y = Mean)) +
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
                                    labs(x = bquote(Less ~ .(rg_phq8[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq8[i, "parts"])),
                                         y = paste0("Difference in ", rg_phq8[i, "phq8"])) +
                                    scale_x_continuous(limits = c(-23, 23),
                                                       breaks = c(-20, 0, 20)) +
                                    # scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2)) +
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

names(phq8_24h_insomnia_q3) <- foreach(i = seq_len(nrow(rg_phq8))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq8[i, "part_labels"], " and ", rg_phq8[i, "phq8"])
}
phq8_24h_insomnia_q3
saveRDS(phq8_24h_insomnia_q3, paste0(outputdir, "phq8_24h_insomnia_q3", ".RDS"))


