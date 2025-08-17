source("ukb-24h-mh-setup.R")
source(paste0(redir, "ukb_utils.R"))

m_adj_phq_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_phq_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_phq_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_phq_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_goodsleep_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_goodsleep_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_goodsleep_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_goodsleep_bl", ".RDS"))

m_adj_gad_2023_gam_sleep_q1_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q1_insomnia_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q2_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q2_insomnia_bl", ".RDS"))
m_adj_gad_2023_gam_sleep_q3_insomnia_bl <- readRDS(paste0(outputdir, "m_adj_gad_2023_gam_sleep_q3_insomnia_bl", ".RDS"))

# rhat
rhat_phq_q1_insomnia <- extract_rhat(m_adj_phq_2023_gam_sleep_q1_insomnia_bl, "PHQ Q1 Insomnia")
rhat_phq_q2_insomnia <- extract_rhat(m_adj_phq_2023_gam_sleep_q2_insomnia_bl, "PHQ Q2 Insomnia")
rhat_phq_q3_insomnia <- extract_rhat(m_adj_phq_2023_gam_sleep_q3_insomnia_bl, "PHQ Q3 Insomnia")

rhat_phq_q1_goodsleep <- extract_rhat(m_adj_phq_2023_gam_sleep_q1_goodsleep_bl, "PHQ Q1 Good Sleep")
rhat_phq_q2_goodsleep <- extract_rhat(m_adj_phq_2023_gam_sleep_q2_goodsleep_bl, "PHQ Q2 Good Sleep")
rhat_phq_q3_goodsleep <- extract_rhat(m_adj_phq_2023_gam_sleep_q3_goodsleep_bl, "PHQ Q3 Good Sleep")

rhat_gad_q1_insomnia <- extract_rhat(m_adj_gad_2023_gam_sleep_q1_insomnia_bl, "GAD Q1 Insomnia")
rhat_gad_q2_insomnia <- extract_rhat(m_adj_gad_2023_gam_sleep_q2_insomnia_bl, "GAD Q2 Insomnia")
rhat_gad_q3_insomnia <- extract_rhat(m_adj_gad_2023_gam_sleep_q3_insomnia_bl, "GAD Q3 Insomnia")

rhat_gad_q1_goodsleep <- extract_rhat(m_adj_gad_2023_gam_sleep_q1_goodsleep_bl, "GAD Q1 Good Sleep")
rhat_gad_q2_goodsleep <- extract_rhat(m_adj_gad_2023_gam_sleep_q2_goodsleep_bl, "GAD Q2 Good Sleep")
rhat_gad_q3_goodsleep <- extract_rhat(m_adj_gad_2023_gam_sleep_q3_goodsleep_bl, "GAD Q3 Good Sleep")

# Combine R-hat summaries
rhat_summary <- rbind(
  rhat_phq_q1_goodsleep$summary,
  rhat_phq_q2_goodsleep$summary,
  rhat_phq_q3_goodsleep$summary,
  rhat_phq_q1_insomnia$summary,
  rhat_phq_q2_insomnia$summary,
  rhat_phq_q3_insomnia$summary,
  rhat_gad_q1_goodsleep$summary,
  rhat_gad_q2_goodsleep$summary,
  rhat_gad_q3_goodsleep$summary,
  rhat_gad_q1_insomnia$summary,
  rhat_gad_q2_insomnia$summary,
  rhat_gad_q3_insomnia$summary
)
rhat_summary

# ess
ess_phq_q1_goodsleep <- extract_ess(m_adj_phq_2023_gam_sleep_q1_insomnia_bl, "PHQ Q1 Good Sleep")
ess_phq_q2_goodsleep <- extract_ess(m_adj_phq_2023_gam_sleep_q2_insomnia_bl, "PHQ Q2 Good Sleep")
ess_phq_q3_goodsleep <- extract_ess(m_adj_phq_2023_gam_sleep_q3_insomnia_bl, "PHQ Q3 Good Sleep")

ess_phq_q1_insomnia <- extract_ess(m_adj_phq_2023_gam_sleep_q1_goodsleep_bl, "PHQ Q1 Insomnia")
ess_phq_q2_insomnia <- extract_ess(m_adj_phq_2023_gam_sleep_q2_goodsleep_bl, "PHQ Q2 Insomnia")
ess_phq_q3_insomnia <- extract_ess(m_adj_phq_2023_gam_sleep_q3_goodsleep_bl, "PHQ Q3 Insomnia")

ess_gad_q1_goodsleep <- extract_ess(m_adj_gad_2023_gam_sleep_q1_insomnia_bl, "GAD Q1 Good Sleep")
ess_gad_q2_goodsleep <- extract_ess(m_adj_gad_2023_gam_sleep_q2_insomnia_bl, "GAD Q2 Good Sleep")
ess_gad_q3_goodsleep <- extract_ess(m_adj_gad_2023_gam_sleep_q3_insomnia_bl, "GAD Q3 Good Sleep")

ess_gad_q1_insomnia <- extract_ess(m_adj_gad_2023_gam_sleep_q1_goodsleep_bl, "GAD Q1 Insomnia")
ess_gad_q2_insomnia <- extract_ess(m_adj_gad_2023_gam_sleep_q2_goodsleep_bl, "GAD Q2 Insomnia")
ess_gad_q3_insomnia <- extract_ess(m_adj_gad_2023_gam_sleep_q3_goodsleep_bl, "GAD Q3 Insomnia")

# Combine ESS summaries
ess_summary <- rbind(
  ess_phq_q1_goodsleep$bulk_summary,
  ess_phq_q2_goodsleep$bulk_summary,
  ess_phq_q3_goodsleep$bulk_summary,
  ess_phq_q1_insomnia$bulk_summary,
  ess_phq_q2_insomnia$bulk_summary,
  ess_phq_q3_insomnia$bulk_summary,
  ess_gad_q1_goodsleep$bulk_summary,
  ess_gad_q2_goodsleep$bulk_summary,
  ess_gad_q3_goodsleep$bulk_summary,
  ess_gad_q1_insomnia$bulk_summary,
  ess_gad_q2_insomnia$bulk_summary,
  ess_gad_q3_insomnia$bulk_summary
)
ess_summary

