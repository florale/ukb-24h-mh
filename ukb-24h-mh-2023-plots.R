source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results ----------------
m_phq_2023_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1", ".RDS"))
m_phq_2023_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2", ".RDS"))
m_phq_2023_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3", ".RDS"))

m_gad_2023_gam_sub_sleep_q1 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1", ".RDS"))
m_gad_2023_gam_sub_sleep_q2 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2", ".RDS"))
m_gad_2023_gam_sub_sleep_q3 <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3", ".RDS"))

# prep data ------------------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep",
                       "m_gad_2023_gam_sub_sleep"
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
  `Short Sleep Period (Q1)` = "#FAD899",
  `Medium Sleep Period (Q2 and Q3)` = "#83A192",
  `Long Sleep Period (Q4)` = "#5A6367" 
)

colf <- c(
  `Short Sleep Period (Q1)` = "#DCD5CE",
  `Medium Sleep Period (Q2 and Q3)` = "#AFC7BB",
  `Long Sleep Period (Q4)` = "#9DB3A8"
)

col_dep <- c(
  `Short Sleep Period (Q1)` = "#944C4C",
  `Medium Sleep Period (Q2 and Q3)` = "#8AAFCA",
  `Long Sleep Period (Q4)` = "#456691" 
)

colf_dep <- c(
  `Short Sleep Period (Q1)` = "#DCD5CE",
  `Medium Sleep Period (Q2 and Q3)` = "#A1B2C2",
  `Long Sleep Period (Q4)` = "#8399AE"
)


col_anx <- c(
  `Short Sleep Period (Q1)` = "#FAD899",
  `Medium Sleep Period (Q2 and Q3)` = "#83A192",
  `Long Sleep Period (Q4)` = "#465A3D" 
)

colf_anx <- c(
  `Short Sleep Period (Q1)` = "#DCD5CE",
  `Medium Sleep Period (Q2 and Q3)` = "#AFC7BB",
  `Long Sleep Period (Q4)` = "#9DB3A8"
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

phq_24h_2023 <- foreach(i = seq_len(nrow(rg_phq)),
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
                                       labeller = label_bquote(cols = .(as.character(From)) %<-% phantom(veryveryvery) %->% .(as.character(To))),
                                       strip.position = "top") +
                            labs(x = bquote(Less ~ .(rg_phq[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_phq[i, "parts"])),
                                 y = paste0("Difference in ", rg_phq[i, "phq"])) +
                            scale_x_continuous(limits = c(-23, 23),
                                               breaks = c(-20, 0, 20)) +
                            scale_y_continuous(limits = c(-0.25, 0.5),
                                               breaks = c(-0.25, 0, 0.25)) +
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
                              strip.background  = element_rect(fill = "transparent", colour = NA),
                              strip.text        = element_text(size = 12, face = "bold", hjust = .5),
                              strip.placement   = "outside",
                              axis.title.x      = element_blank(),
                              axis.text.x       = element_text(size = 11),
                              axis.title.y      = element_blank(),
                              # plot.margin       = margin(.5, .5, .5, .5, "cm"),
                              legend.title      = element_blank(),
                              legend.text       = element_text(size = 13, face = "bold", hjust = .5),
                              legend.position   = "bottom",
                              plot.margin         = unit(c(0,0.5,1.5,0.5), "lines")
                            )
                          
                        }

names(phq_24h_2023) <- foreach(i = seq_len(nrow(rg_phq))) %dopar% {
  paste0("Reallocation of Time between ", rg_phq[i, "part_labels"], " and ", rg_phq[i, "phq"])
}
phq_24h_2023

saveRDS(phq_24h_2023, paste0(outputdir, "phq_24h_2023", ".RDS"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_24h_2023", ".pdf"),
  width = 9,
  height = 12,
)

figure <- ggarrange(phq_24h_2023[[1]], phq_24h_2023[[2]], 
                    phq_24h_2023[[3]], phq_24h_2023[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# gad by sleep period----------------
sub_models_gad <- grep("gad", names(sub_models), value = T)
gad            <- "Anxiety Symptoms"
rg_gad <- expand.grid.df(data.frame(sub_models_gad, gad), 
                         data.frame(parts, part_labels))

gad_24h_2023 <- foreach(i = seq_len(nrow(rg_gad)),
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
                                       strip.position = "top") +
                            labs(x = bquote(Less ~ .(rg_gad[i, "parts"]) %<-% phantom(veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryverylong) %->% More ~ .(rg_gad[i, "parts"])),
                                 y = paste0("Difference in ", rg_gad[i, "gad"])) +
                            scale_x_continuous(limits = c(-23, 23),
                                               breaks = c(-20, 0, 20)) +
                            scale_y_continuous(limits = c(-0.25, 0.25),
                                               breaks = c(-0.25, 0, 0.25)) +
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
                              strip.background  = element_rect(fill = "transparent", colour = NA),
                              strip.text        = element_text(size = 12, face = "bold", hjust = .5),
                              strip.placement   = "outside",
                              axis.title.x      = element_blank(),
                              axis.text.x       = element_text(size = 11),
                              axis.title.y      = element_blank(),
                              # plot.margin       = margin(.5, .5, .5, .5, "cm"),
                              legend.title      = element_blank(),
                              legend.text       = element_text(size = 13, face = "bold", hjust = .5),
                              legend.position   = "bottom",
                              plot.margin         = unit(c(0,0.5,1.5,0.5), "lines")
                            )
                          
                        }

names(gad_24h_2023) <- foreach(i = seq_len(nrow(rg_gad))) %dopar% {
  paste0("Reallocation of Time between ", rg_gad[i, "part_labels"], " and ", rg_gad[i, "gad"])
}
gad_24h_2023

saveRDS(gad_24h_2023, paste0(outputdir, "gad_24h_2023", ".RDS"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "gad_24h_2023", ".pdf"),
  width = 9,
  height = 12,
)

figure <- ggarrange(gad_24h_2023[[1]], gad_24h_2023[[2]], 
                    gad_24h_2023[[3]], gad_24h_2023[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

# by sleep period and insomnia symptoms -------
## read models ---------
m_phq_2023_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_goodsleep", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_goodsleep", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_goodsleep <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_goodsleep", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_mild", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_mild", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_mild <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_mild", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_moderate", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_moderate", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_moderate <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_moderate", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_severe", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_severe", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_severe <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_severe", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_insomnia_persistent", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_insomnia_persistent", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_insomnia_persistent <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_insomnia_persistent", ".RDS"))

## mild moderate severe data ----------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep", "m_gad_2023_gam_sub_sleep")
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

## only 20 min reallocations ------------------
d_tmp_dep <- sub_models_insomnia[[1]][Delta == 20]
d_tmp_dep[, Sig := ifelse(sig == FALSE, "*", "")]
d_tmp_dep[, by := paste0(SleepPeriod, " - ",Insomnia)]
d_tmp_dep[, SleepPeriodID := 1:.N, by = .(From, To)]
d_tmp_dep <- d_tmp_dep[order(From, To, SleepPeriod, Insomnia)]
d_tmp_dep[, text_position := min(CI_high), by = From]

### other variations -----
ggplot(d_tmp_dep, aes(x = SleepPeriodID, y = Mean, colour = by)) +
  geom_pointrange(aes(ymin = CI_low,
                      ymax = CI_high, colour = by), size = .5, linewidth = 0.5,
                  position = position_dodge2(width = .75)
  ) +
  geom_text(aes(y = text_position, label = Sig),
            size = 6, 
            # nudge_x = 0.2,
            show.legend = FALSE) +
  # facet_wrap(~From, scale = "free") +
  facet_wrap(ggplot2::vars(From, To), scale = "free") +
  scale_colour_manual(values = col) +
  scale_x_continuous(
    breaks = unique(d_tmp_dep$SleepPeriodID), 
    labels = d_tmp_dep[1:12]$SleepPeriod,
    sec.axis = sec_axis(~.,
                        breaks = unique(d_tmp_dep$SleepPeriodID),
                        labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
    )
  ) +
  labs(x = "", y = "", colour = "") +
  coord_flip() +
  theme_ipsum() +
  theme(
    axis.ticks          = element_blank(),
    panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
    plot.background     = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
    legend.position     = "none"
  )

ggplot(d_tmp_dep[From == "MVPA"], aes(x = SleepPeriodID, y = Mean, colour = by)) +
  geom_pointrange(aes(ymin = CI_low,
                      ymax = CI_high, colour = by), size = .5, linewidth = 0.5,
                  position = position_dodge2(width = .75)
  ) +
  geom_text(aes(y = text_position, label = Sig),
            size = 6, 
            # nudge_x = 0.2,
            show.legend = FALSE) +
  # facet_wrap(~From, scale = "free") +
  facet_wrap(ggplot2::vars(From, To), scale = "free") +
  scale_colour_manual(values = col) +
  scale_x_continuous(
    breaks = unique(d_tmp_dep$SleepPeriodID), 
    labels = d_tmp_dep[1:12]$SleepPeriod,
    sec.axis = sec_axis(~.,
                        breaks = unique(d_tmp_dep$SleepPeriodID),
                        labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
    )
  ) +
  labs(x = "", y = "", colour = "") +
  coord_flip() +
  theme_ipsum() +
  theme(
    axis.ticks          = element_blank(),
    panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
    plot.background     = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
    legend.position     = "none"
  )

ggplot(d_tmp_dep, aes(x = SleepPeriod, y = Mean, colour = Insomnia)) +
  geom_pointrange(aes(ymin = CI_low,
                      ymax = CI_high, colour = Insomnia), size = .5, linewidth = 0.5,
                  position = position_dodge2(width = .75)
  ) +
  geom_text(aes(y = text_position, label = Sig),
            size = 6, 
            # nudge_x = 0.2,
            show.legend = FALSE) +
  # facet_wrap(~From, scale = "free") +
  facet_wrap(ggplot2::vars(From, To), scale = "free") +
  scale_colour_manual(values = col) +
  # scale_x_continuous(
  #   breaks = unique(d_tmp_dep$SleepPeriodID), 
  #   labels = d_tmp_dep[1:12]$SleepPeriod,
  #   sec.axis = sec_axis(~.,
  #                       breaks = unique(d_tmp_dep$SleepPeriodID),
  #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
  #   )
  # ) +
  labs(x = "", y = "", colour = "") +
  coord_flip() +
  theme_ipsum() +
  theme(
    axis.ticks          = element_blank(),
    panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
    plot.background     = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
    # axis.text.y        = element_text(angle = 90),
    legend.position     = "bottom"
  )

phq_by_insomnia_sleep_period <- foreach(i = seq_along(parts),
                                        .packages = "multilevelcoda") %dopar% {
                                          
                                          part <- parts[i]
                                          
                                          ggplot(d_tmp_dep[From == eval(part)], aes(x = SleepPeriod, y = Mean, colour = Insomnia)) +
                                            geom_pointrange(aes(ymin = CI_low,
                                                                ymax = CI_high, colour = Insomnia), size = .5, linewidth = 0.5,
                                                            position = position_dodge2(width = .75)
                                            ) +
                                            geom_text(aes(y = text_position, label = Sig),
                                                      size = 6, 
                                                      # nudge_x = 0.2,
                                                      show.legend = FALSE) +
                                            # facet_wrap(~From, scale = "free") +
                                            facet_wrap(ggplot2::vars(From, To), scale = "free") +
                                            scale_colour_manual(values = col) +
                                            # scale_x_continuous(
                                            #   breaks = unique(d_tmp_dep$SleepPeriodID), 
                                            #   labels = d_tmp_dep[1:12]$SleepPeriod,
                                            #   sec.axis = sec_axis(~.,
                                            #                       breaks = unique(d_tmp_dep$SleepPeriodID),
                                            #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
                                            #   )
                                            # ) +
                                            labs(x = "", y = "", colour = "") +
                                            coord_flip() +
                                            theme_ipsum() +
                                            theme(
                                              axis.ticks          = element_blank(),
                                              panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                              plot.background     = element_rect(fill = "transparent", colour = NA),
                                              panel.grid.major.x  = element_blank(),
                                              panel.grid.minor    = element_blank(),
                                              strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
                                              legend.position     = "bottom"
                                            )
                                        }

# grDevices::cairo_pdf(
#   file = paste0(outputdir, "phq_by_insomnia_sleep_period", ".pdf"),
#   width = 9,
#   height = 12,
# )
# 
# figure <- ggarrange(phq_by_insomnia_sleep_period[[1]], phq_by_insomnia_sleep_period[[2]], 
#                     phq_by_insomnia_sleep_period[[3]], phq_by_insomnia_sleep_period[[4]], 
#                     nrow = 4,
#                     common.legend = TRUE,
#                     legend   = "bottom"
# )
# annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
# dev.off()


ggplot(d_tmp_dep, aes(x = Insomnia, y = Mean, colour = SleepPeriod)) +
  geom_pointrange(aes(ymin = CI_low,
                      ymax = CI_high, colour = SleepPeriod), size = .5, linewidth = 0.5,
                  position = position_dodge2(width = .75)
  ) +
  geom_text(aes(y = text_position, label = Sig),
            size = 6, 
            # nudge_x = 0.2,
            show.legend = FALSE) +
  # facet_wrap(~From, scale = "free") +
  facet_wrap(ggplot2::vars(From, To), scale = "free") +
  scale_colour_manual(values = col) +
  # scale_x_continuous(
  #   breaks = unique(d_tmp_dep$SleepPeriodID), 
  #   labels = d_tmp_dep[1:12]$SleepPeriod,
  #   sec.axis = sec_axis(~.,
  #                       breaks = unique(d_tmp_dep$SleepPeriodID),
  #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
  #   )
  # ) +
  labs(x = "", y = "", colour = "") +
  coord_flip() +
  theme_ipsum() +
  theme(
    axis.ticks          = element_blank(),
    panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
    plot.background     = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
    # axis.text.y        = element_text(angle = 90),
    legend.position     = "bottom"
  )

### going with this -----------
phq_by_sleep_period_insomnia_2023 <- foreach(i = seq_along(parts),
                                             .packages = "multilevelcoda") %dopar% {
                                               
                                               part <- parts[i]
                                               
                                               ggplot(d_tmp_dep[From == eval(part)], aes(x = Insomnia, y = Mean, colour = SleepPeriod, group = by)) +
                                                 geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                 geom_pointrange(aes(ymin = CI_low,
                                                                     ymax = CI_high, colour = SleepPeriod), size = .5, linewidth = 0.5,
                                                                 position = position_dodge2(width = .75)
                                                 ) +
                                                 # geom_text(aes(y = text_position - 0.2, label = Sig),
                                                 #           size = 6, 
                                                 #           # nudge_x = 0.2,
                                                 #           show.legend = FALSE) +
                                                 # facet_wrap(~From, scale = "free") +
                                                 facet_wrap(ggplot2::vars(From, To),
                                                            labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                            strip.position = "top") +
                                                 scale_colour_manual(values = col) +
                                                 scale_y_continuous(limits = c(-1, 2),
                                                                    breaks = c(-1, 0, 1, 2)
                                                 ) +
                                                 # scale_x_continuous(
                                                 #   breaks = unique(d_tmp_dep$SleepPeriodID), 
                                                 #   labels = d_tmp_dep[1:12]$SleepPeriod,
                                                 #   sec.axis = sec_axis(~.,
                                                 #                       breaks = unique(d_tmp_dep$SleepPeriodID),
                                                 #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
                                                 #   )
                                                 # ) +
                                                 labs(x = "", y = "", colour = "") +
                                                 coord_flip() +
                                                 theme_ipsum() +
                                                 theme(
                                                   axis.ticks          = element_blank(),
                                                   panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
                                                   plot.background     = element_rect(fill = "transparent", colour = NA),
                                                   panel.grid.major.y  = element_blank(),
                                                   panel.grid.minor    = element_blank(),
                                                   strip.text          = element_text(size = 12, hjust = .5, face = "bold"),
                                                   legend.position     = "bottom",
                                                   plot.margin = unit(c(0,0,0,1), "lines")
                                                 )
                                             }
phq_by_sleep_period_insomnia_2023

phq_by_sleep_period_insomnia_2023[[1]] / phq_by_sleep_period_insomnia_2023[[2]] / phq_by_sleep_period_insomnia_2023[[3]] / phq_by_sleep_period_insomnia_2023[[4]] + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_by_sleep_period_insomnia_2023", ".pdf"),
  width = 9,
  height = 12,
)

phq_by_sleep_period_insomnia_2023[[1]] / phq_by_sleep_period_insomnia_2023[[2]] / phq_by_sleep_period_insomnia_2023[[3]] / phq_by_sleep_period_insomnia_2023[[4]] + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# annotate_figure(figure, left = text_grob("Estimated Difference in Depressive Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

## acute and persistent data -------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep", "m_gad_2023_gam_sub_sleep")
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
      insomnia <- if (k == 1) ("Good Sleep") else if (k == 2) ("Acute Insomnia") else ("Persistent Insomnia")
      
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
                                                "Persistent Insomnia"))]
      
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
d_tmp_dep[, text_position := min(CI_high), by = From]

d_tmp_dep[, Insomnia_label := NA]
d_tmp_dep[, Insomnia_label := ifelse(Insomnia == "Good Sleep", "Good\nSleep", Insomnia_label)]
d_tmp_dep[, Insomnia_label := ifelse(Insomnia == "Acute Insomnia", "Acute\nInsomnia", Insomnia_label)]
d_tmp_dep[, Insomnia_label := ifelse(Insomnia == "Persistent Insomnia", "Persistent\nInsomnia", Insomnia_label)]

d_tmp_dep[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                     levels = c("Good\nSleep",
                                                "Acute\nInsomnia",
                                                "Persistent\nInsomnia"))]

## final plot ----------
phq_by_sleep_period_insomnia_2023 <- foreach(i = seq_along(parts),
                                             .packages = "multilevelcoda") %dopar% {
                                               
                                               part <- parts[i]
                                               
                                               ggplot(d_tmp_dep[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                 geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                 geom_pointrange(aes(ymin = CI_low,
                                                                     ymax = CI_high), size = .5, linewidth = 0.5,
                                                                 position = position_dodge2(width = 0.5)
                                                 ) +
                                                 # geom_text(aes(y = text_position - 0.2, label = Sig),
                                                 #           size = 6, 
                                                 #           # nudge_x = 0.2,
                                                 #           show.legend = FALSE) +
                                                 # facet_wrap(~From, scale = "free") +
                                                 facet_wrap(ggplot2::vars(From, To),
                                                            labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                            strip.position = "top") +
                                                 scale_colour_manual(values = col) +
                                                 scale_y_continuous(limits = c(-0.85, 1.56),
                                                                    breaks = c(-0.5, 0, 0.5, 1)
                                                 ) +
                                                 scale_x_discrete(position = "bottom") +
                                                 # scale_x_continuous(
                                                 #   breaks = unique(d_tmp_dep$SleepPeriodID), 
                                                 #   labels = d_tmp_dep[1:12]$SleepPeriod,
                                                 #   sec.axis = sec_axis(~.,
                                                 #                       breaks = unique(d_tmp_dep$SleepPeriodID),
                                                 #                       labels = rep(levels(d_tmp_dep$Insomnia), length.out = length(unique(d_tmp_dep$SleepPeriodID)))
                                                 #   )
                                                 # ) +
                                                 labs(x = "", y = "", colour = "") +
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
                                                   strip.text          = element_text(size = 13, hjust = .5, face = "bold"),
                                                   axis.text.x         = element_text(size = 11, hjust = 0.5),
                                                   legend.text         = element_text(size = 12),
                                                   legend.position     = "bottom",
                                                   plot.margin         = unit(c(0,0.5,0.5,0.5), "lines")
                                                 )
                                             }

phq_by_sleep_period_insomnia_2023[[1]] / phq_by_sleep_period_insomnia_2023[[2]] / phq_by_sleep_period_insomnia_2023[[3]] / phq_by_sleep_period_insomnia_2023[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "phq_by_sleep_period_insomnia_2023", ".pdf"),
  width = 9,
  height = 12,
)

# phq_by_sleep_period_insomnia_2023[[1]] / phq_by_sleep_period_insomnia_2023[[2]] / phq_by_sleep_period_insomnia_2023[[3]] / phq_by_sleep_period_insomnia_2023[[4]] + 
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")

figure <- ggarrange(phq_by_sleep_period_insomnia_2023[[1]], phq_by_sleep_period_insomnia_2023[[2]], 
                    phq_by_sleep_period_insomnia_2023[[3]], phq_by_sleep_period_insomnia_2023[[4]], 
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

d_tmp_anx[, Insomnia_label := NA]
d_tmp_anx[, Insomnia_label := ifelse(Insomnia == "Good Sleep", "Good\nSleep", Insomnia_label)]
d_tmp_anx[, Insomnia_label := ifelse(Insomnia == "Acute Insomnia", "Acute\nInsomnia", Insomnia_label)]
d_tmp_anx[, Insomnia_label := ifelse(Insomnia == "Persistent Insomnia", "Persistent\nInsomnia", Insomnia_label)]

d_tmp_anx[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                     levels = c("Good\nSleep",
                                                "Acute\nInsomnia",
                                                "Persistent\nInsomnia"))]

## final plot ----------
gad_by_sleep_period_insomnia_2023 <- foreach(i = seq_along(parts),
                                             .packages = "multilevelcoda") %dopar% {
                                               
                                               part <- parts[i]
                                               
                                               ggplot(d_tmp_anx[From == eval(part)], aes(x = Insomnia_label, y = Mean, colour = SleepPeriod, group = SleepPeriod)) +
                                                 geom_hline(aes(yintercept = 0), linewidth = 0.5, linetype = 2, colour = "#a8a8a8") +
                                                 geom_pointrange(aes(ymin = CI_low,
                                                                     ymax = CI_high, colour = SleepPeriod), size = .5, linewidth = 0.5,
                                                                 position = position_dodge2(width = 0.5)
                                                 ) +
                                                 # geom_text(aes(y = text_position - 0.2, label = Sig),
                                                 #           size = 6, 
                                                 #           # nudge_x = 0.2,
                                                 #           show.legend = FALSE) +
                                                 # facet_wrap(~From, scale = "free") +
                                                 facet_wrap(ggplot2::vars(From, To),
                                                            labeller = label_bquote(cols = .(as.character(From)) ~ phantom(lalalala) %->% phantom(lalalala) ~ .(as.character(To))),
                                                            strip.position = "top") +
                                                 scale_colour_manual(values = col) +
                                                 scale_y_continuous(limits = c(-0.75, 0.85),
                                                                    breaks = c(-0.5, 0, 0.5, 1)
                                                 ) +
                                                 scale_x_discrete(position = "bottom") +
                                                 # scale_x_continuous(
                                                 #   breaks = unique(d_tmp_anx$SleepPeriodID), 
                                                 #   labels = d_tmp_anx[1:12]$SleepPeriod,
                                                 #   sec.axis = sec_axis(~.,
                                                 #                       breaks = unique(d_tmp_anx$SleepPeriodID),
                                                 #                       labels = rep(levels(d_tmp_anx$Insomnia), length.out = length(unique(d_tmp_anx$SleepPeriodID)))
                                                 #   )
                                                 # ) +
                                                 labs(x = "", y = "", colour = "") +
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
                                                   strip.text          = element_text(size = 13, hjust = .5, face = "bold"),
                                                   axis.text.x         = element_text(size = 11, hjust = 0.5),
                                                   legend.text         = element_text(size = 12),
                                                   legend.position     = "bottom",
                                                   plot.margin         = unit(c(0,0.5,0.5,0.5), "lines")
                                                 )
                                             }

gad_by_sleep_period_insomnia_2023[[1]] / gad_by_sleep_period_insomnia_2023[[2]] / gad_by_sleep_period_insomnia_2023[[3]] / gad_by_sleep_period_insomnia_2023[[4]]  + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

grDevices::cairo_pdf(
  file = paste0(outputdir, "gad_by_sleep_period_insomnia_2023", ".pdf"),
  width = 9,
  height = 12,
)

# gad_by_sleep_period_insomnia_2023[[1]] / gad_by_sleep_period_insomnia_2023[[2]] / gad_by_sleep_period_insomnia_2023[[3]] / gad_by_sleep_period_insomnia_2023[[4]] + 
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")

figure <- ggarrange(gad_by_sleep_period_insomnia_2023[[1]], gad_by_sleep_period_insomnia_2023[[2]], 
                    gad_by_sleep_period_insomnia_2023[[3]], gad_by_sleep_period_insomnia_2023[[4]], 
                    nrow = 4,
                    common.legend = TRUE,
                    legend   = "bottom"
)
annotate_figure(figure, left = text_grob("Estimated Difference in Anxiety Symptoms", size = 14, rot = 90, family = "Arial Narrow", face = "bold"))
dev.off()

