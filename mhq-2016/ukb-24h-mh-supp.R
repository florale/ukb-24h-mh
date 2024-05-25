source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# substitution results ----------------
## bmi -------------------------------
m_phq_gam_sub_sleep_q1_healthy <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_healthy", ".RDS"))
m_phq_gam_sub_sleep_q2_healthy <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_healthy", ".RDS"))
m_phq_gam_sub_sleep_q3_healthy <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_healthy", ".RDS"))

m_phq_gam_sub_sleep_q1_overweight <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_overweight", ".RDS"))
m_phq_gam_sub_sleep_q2_overweight <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_overweight", ".RDS"))
m_phq_gam_sub_sleep_q3_overweight <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_overweight", ".RDS"))

m_phq_gam_sub_sleep_q1_obese <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_obese", ".RDS"))
m_phq_gam_sub_sleep_q2_obese <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_obese", ".RDS"))
m_phq_gam_sub_sleep_q3_obese <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_obese", ".RDS"))

m_gad_gam_sub_sleep_q1_healthy <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1_healthy", ".RDS"))
m_gad_gam_sub_sleep_q2_healthy <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2_healthy", ".RDS"))
m_gad_gam_sub_sleep_q3_healthy <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3_healthy", ".RDS"))

m_gad_gam_sub_sleep_q1_overweight <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1_overweight", ".RDS"))
m_gad_gam_sub_sleep_q2_overweight <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2_overweight", ".RDS"))
m_gad_gam_sub_sleep_q3_overweight <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3_overweight", ".RDS"))

m_gad_gam_sub_sleep_q1_obese <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1_obese", ".RDS"))
m_gad_gam_sub_sleep_q2_obese <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2_obese", ".RDS"))
m_gad_gam_sub_sleep_q3_obese <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3_obese", ".RDS"))

## life time mh -------------------------------
# m_phq_gam_sub_sleep_q1_mh1 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_mh1", ".RDS"))
# m_phq_gam_sub_sleep_q2_mh1 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_mh1", ".RDS"))
# m_phq_gam_sub_sleep_q3_mh1 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_mh1", ".RDS"))
# 
# m_phq_gam_sub_sleep_q1_mh0 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q1_mh0", ".RDS"))
# m_phq_gam_sub_sleep_q2_mh0 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q2_mh0", ".RDS"))
# m_phq_gam_sub_sleep_q3_mh0 <- readRDS(paste0(outputdir, "m_phq_gam_sub_sleep_q3_mh0", ".RDS"))
# 
# m_gad_gam_sub_sleep_q1_mh1 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1_mh1", ".RDS"))
# m_gad_gam_sub_sleep_q2_mh1 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2_mh1", ".RDS"))
# m_gad_gam_sub_sleep_q3_mh1 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3_mh1", ".RDS"))
# 
# m_gad_gam_sub_sleep_q1_mh0 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q1_mh0", ".RDS"))
# m_gad_gam_sub_sleep_q2_mh0 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q2_mh0", ".RDS"))
# m_gad_gam_sub_sleep_q3_mh0 <- readRDS(paste0(outputdir, "m_gad_gam_sub_sleep_q3_mh0", ".RDS"))

# plots --------------------
phq_24h_healthy <- readRDS(paste0(outputdir, "phq_24h_healthy", ".RDS"))
phq_24h_overweight <- readRDS(paste0(outputdir, "phq_24h_overweight", ".RDS"))
phq_24h_obese <- readRDS(paste0(outputdir, "phq_24h_obese", ".RDS"))

gad_24h_healthy <- readRDS(paste0(outputdir, "gad_24h_healthy", ".RDS"))
gad_24h_overweight <- readRDS(paste0(outputdir, "gad_24h_overweight", ".RDS"))
gad_24h_obese <- readRDS(paste0(outputdir, "gad_24h_obese", ".RDS"))