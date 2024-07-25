source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results
m_phq_gam_sub <- readRDS(paste0(outputdir, "m_phq_gam_sub", ".RDS"))
m_gad_gam_sub <- readRDS(paste0(outputdir, "m_gad_gam_sub", ".RDS"))
m_pcl_gam_sub <- readRDS(paste0(outputdir, "m_pcl_gam_sub", ".RDS"))
m_selfharm_gam_sub <- readRDS(paste0(outputdir, "m_selfharm_gam_sub", ".RDS"))
m_wellbeing_gam_sub <- readRDS(paste0(outputdir, "m_wellbeing_gam_sub", ".RDS"))

m_dep_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_dep_lifetime_gam_sub", ".RDS"))
m_anx_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_anx_lifetime_gam_sub", ".RDS"))
m_bipolar_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_bipolar_lifetime_gam_sub", ".RDS"))
m_psychosis_lifetime_gam_sub <- readRDS(paste0(outputdir, "m_psychosis_lifetime_gam_sub", ".RDS"))

# set up plots ----------------
col <- c(
  `sleep_comp` = "#5A6367",
  `mvpa_comp` = "#708885",
  `lpa_comp` = "#BEACA2",
  `sb_comp` = "#C99696"
)

colf <- c(
  `sleep_comp` = "#5A6367",
  `mvpa_comp` = "#708885",
  `lpa_comp` = "#BEACA2",
  `sb_comp` = "#C99696"
)

names <- c(
  `sleep_comp` = "Sleep",
  `mvpa_comp` = "Moderate-to-Vigorous Physical Activity",
  `lpa_comp` = "Light Physical Activity",
  `sb_comp` = "Sedentary Behaviour"
)
names <- c(
  `sleep_comp` = "Sleep",
  `mvpa_comp` = "MVPA",
  `lpa_comp` = "LPA",
  `sb_comp` = "SB"
)
labeller <- function(variable, value) {
  return(names[value])
}

# make a grid to loop plots
sub_models   <- c("m_phq_gam_sub", "m_gad_gam_sub", "m_pcl_gam_sub", "m_wellbeing_gam_sub",
                  "m_selfharm_gam_sub", "m_dep_lifetime_gam_sub", "m_anx_lifetime_gam_sub", "m_bipolar_lifetime_gam_sub", "m_psychosis_lifetime_gam_sub")
resp         <- c("Depressive Symptoms", "Anxiety Symptoms", "Post-traumatic Stress", "Wellbeing",
                  "Selfharm", "Lifetime Depression", "Lifetime Anxiety", "Lifetime Bipolar", "Lifetime Psychosis")
parts        <- c("sleep_comp" , "mvpa_comp", "lpa_comp", "sb_comp")
part_labels  <- c("Sleep", "Moderate-to-Vigorous Physical Activity", "Light Physical Activity", "Sedentary Behaviour")

rg <- expand.grid.df(data.frame(sub_models, resp), 
                     data.frame(parts, part_labels))
rg <- rg[order(rg$sub_models), ]

# plots ----------------
plot_mhq <- foreach(i = seq_len(nrow(rg)),
                    .packages = "multilevelcoda") %dopar% {
                      plot(get(rg[i, "sub_models"]), to = rg[i, "parts"], ref = "grandmean", level = "combined") +
                        scale_colour_manual(values = col) +
                        scale_fill_manual(values = colf) +
                        labs(x = paste0("Difference in ", rg[i, "part_labels"]),
                             y = paste0("Difference in ", rg[i, "resp"])) +
                        facet_grid( ~ From, labeller = labeller) +
                        scale_x_continuous(breaks = c(-30, 0, 30)) +
                        # scale_y_continuous(limits = c(-0.75, 0.75),
                        #                    breaks = c(-1, -0.5, 0, 0.5, 1)) +
                        hrbrthemes::theme_ipsum() +
                        theme(
                          axis.ticks        = element_blank(),
                          panel.background  = element_blank(),
                          panel.border      = element_blank(),
                          panel.grid.major  = element_blank(),
                          panel.grid.minor  = element_blank(),
                          plot.background   = element_rect(fill = "transparent", colour = NA),
                          # strip.text = element_blank(),
                          axis.title.x      = element_text(size = 14),
                          axis.title.y      = element_text(size = 14, hjust = .5),
                          plot.margin = margin(.5, .5, .5, .5, "cm"),
                          legend.position = "none"
                        )
                    }

names(plot_mhq) <- foreach(i = seq_len(nrow(rg))) %dopar% {
  paste0("Reallocation of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
plot_mhq

saveRDS(plot_mhq, paste0(outputdir, "plot_mhq", ".RDS"))

