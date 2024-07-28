source("ukb-24h-mh-utils.R")
source(paste0(redir, "ukb_utils.R"))

# output ---------------------
## histogram
phq_2023_hist <- readRDS(paste0(outputdir, "phq_2023_hist", ".RDS"))
gad_2023_hist <- readRDS(paste0(outputdir, "gad_2023_hist", ".RDS"))

## dep and anx incidences
m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_dep_anx_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_dep_anx_insomnia_persistent_99ci", ".RDS"))

## bmi
### healthy
m_phq_2023_gam_sub_sleep_q1_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_healthy_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_healthy_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_healthy_insomnia_persistent_99ci", ".RDS"))

### overweight
m_phq_2023_gam_sub_sleep_q1_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_overweight_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_overweight_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_overweight_insomnia_persistent_99ci", ".RDS"))

### obese
m_phq_2023_gam_sub_sleep_q1_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))
m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_phq_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_goodsleep_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_goodsleep_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_mild_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_mild_99ci", ".RDS"))

m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q1_obese_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q2_obese_insomnia_persistent_99ci", ".RDS"))
m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci <- readRDS(paste0(outputdir, "m_gad_2023_gam_sub_sleep_q3_obese_insomnia_persistent_99ci", ".RDS"))

# format ------------------------------
## dep anx models ----------------------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep",
                       "m_gad_2023_gam_sub_sleep"
)
sleep_type <- c("_q1_dep_anx_99ci", "_q2_dep_anx_99ci", "_q3_dep_anx_99ci")
sleep_q_type <- c("_q1_dep_anx", "_q2_dep_anx", "_q3_dep_anx")
sleep_insomnia_type <- c("_goodsleep_99ci", "_insomnia_mild_99ci", "_insomnia_persistent_99ci")

sub_models_dep_anx <- list()
sub_models_sleepg_type <- list()

sub_models_dep_anx_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
    
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
    model_tmp[, Sig := ifelse(sig == FALSE & Delta == 20, "*", "")]
    
    model_tmp <- model_tmp[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Sig)]
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleep Period (Q1)",
                                                              "Medium Sleep Period (Q2 and Q3)",
                                                              "Long Sleep Period (Q4)"))]
  sub_models_dep_anx[[i]] <- sub_models_sleepg_type_i
}
names(sub_models_dep_anx) <- (sub_models_resp)

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("None") else if (k == 2) ("Mild") else ("Moderate-Severe")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
      
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
      model_tmp[, Insomnia_label := ifelse(Insomnia == "None", "None", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Mild", "Mild", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Moderate-Severe", "Moderate-Severe", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("None",
                                                      "Mild",
                                                      "Moderate-Severe"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% 20, "*", "")]
      
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
  sub_models_sleep_q_type_i <- sub_models_sleep_q_type_i[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Insomnia, Sig)]
  
  sub_models_dep_anx_insomnia[[i]] <- sub_models_sleep_q_type_i
  
  
}
names(sub_models_dep_anx_insomnia) <- (sub_models_resp)


## healthy models ----------------------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep",
                       "m_gad_2023_gam_sub_sleep"
)
sleep_type <- c("_q1_healthy_99ci", "_q2_healthy_99ci", "_q3_healthy_99ci")
sleep_q_type <- c("_q1_healthy", "_q2_healthy", "_q3_healthy")
sleep_insomnia_type <- c("_goodsleep_99ci", "_insomnia_mild_99ci", "_insomnia_persistent_99ci")

sub_models_healthy <- list()
sub_models_sleepg_type <- list()

sub_models_healthy_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
    
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
    model_tmp[, Sig := ifelse(sig == FALSE & Delta == 20, "*", "")]
    
    model_tmp <- model_tmp[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Sig)]
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleep Period (Q1)",
                                                              "Medium Sleep Period (Q2 and Q3)",
                                                              "Long Sleep Period (Q4)"))]
  sub_models_healthy[[i]] <- sub_models_sleepg_type_i
}
names(sub_models_healthy) <- (sub_models_resp)

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("None") else if (k == 2) ("Mild") else ("Moderate-Severe")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
      
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
      model_tmp[, Insomnia_label := ifelse(Insomnia == "None", "None", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Mild", "Mild", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Moderate-Severe", "Moderate-Severe", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("None",
                                                      "Mild",
                                                      "Moderate-Severe"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% 20, "*", "")]
      
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
  sub_models_sleep_q_type_i <- sub_models_sleep_q_type_i[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Insomnia, Sig)]
  
  sub_models_healthy_insomnia[[i]] <- sub_models_sleep_q_type_i
  
  
}
names(sub_models_healthy_insomnia) <- (sub_models_resp)

## overweight models ----------------------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep",
                       "m_gad_2023_gam_sub_sleep"
)
sleep_type <- c("_q1_overweight_99ci", "_q2_overweight_99ci", "_q3_overweight_99ci")
sleep_q_type <- c("_q1_overweight", "_q2_overweight", "_q3_overweight")
sleep_insomnia_type <- c("_goodsleep_99ci", "_insomnia_mild_99ci", "_insomnia_persistent_99ci")

sub_models_overweight <- list()
sub_models_sleepg_type <- list()

sub_models_overweight_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
    
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
    model_tmp[, Sig := ifelse(sig == FALSE & Delta == 20, "*", "")]
    
    model_tmp <- model_tmp[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Sig)]
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleep Period (Q1)",
                                                              "Medium Sleep Period (Q2 and Q3)",
                                                              "Long Sleep Period (Q4)"))]
  sub_models_overweight[[i]] <- sub_models_sleepg_type_i
}
names(sub_models_overweight) <- (sub_models_resp)

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("None") else if (k == 2) ("Mild") else ("Moderate-Severe")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = 20, level = "aggregate", digits = "asis"))
      
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
      model_tmp[, Insomnia_label := ifelse(Insomnia == "None", "None", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Mild", "Mild", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Moderate-Severe", "Moderate-Severe", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("None",
                                                      "Mild",
                                                      "Moderate-Severe"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% 20, "*", "")]
      
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
  sub_models_sleep_q_type_i <- sub_models_sleep_q_type_i[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Insomnia, Sig)]
  
  sub_models_overweight_insomnia[[i]] <- sub_models_sleep_q_type_i
  
  
}
names(sub_models_overweight_insomnia) <- (sub_models_resp)

## obese models ----------------------
sub_models_resp   <- c("m_phq_2023_gam_sub_sleep",
                       "m_gad_2023_gam_sub_sleep"
)
sleep_type <- c("_q1_obese_99ci", "_q2_obese_99ci", "_q3_obese_99ci")
sleep_q_type <- c("_q1_obese", "_q2_obese", "_q3_obese")
sleep_insomnia_type <- c("_goodsleep_99ci", "_insomnia_mild_99ci", "_insomnia_persistent_99ci")

sub_models_obese <- list()
sub_models_sleepg_type <- list()

sub_models_obese_insomnia <- list()
sub_models_sleep_q_type <- list()
sub_models_sleep_i_type <- list()

for(i in seq_along(sub_models_resp)) {
  
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_type)) {
    
    sleep_tye_j <- sleep_type[[j]]
    
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    model_tmp <- get(paste0(sub_model_i, sleep_tye_j))
    model_tmp <- as.data.table(summary(model_tmp, delta = 19, level = "aggregate", digits = "asis"))
    
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
    model_tmp[, Sig := ifelse(sig == FALSE & Delta == 19, "*", "")]
    
    model_tmp <- model_tmp[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Sig)]
    sub_models_sleepg_type[[j]] <- model_tmp
  }
  sub_models_sleepg_type_i  <- rbindlist(sub_models_sleepg_type)
  sub_models_sleepg_type_i[, SleepPeriod := factor(SleepPeriod, ordered = TRUE,
                                                   levels = c("Short Sleep Period (Q1)",
                                                              "Medium Sleep Period (Q2 and Q3)",
                                                              "Long Sleep Period (Q4)"))]
  sub_models_obese[[i]] <- sub_models_sleepg_type_i
}
names(sub_models_obese) <- (sub_models_resp)

for(i in seq_along(sub_models_resp)) {
  sub_model_i <- sub_models_resp[[i]]
  
  for (j in seq_along(sleep_q_type)) {
    sleep_type_j <- sleep_q_type[[j]]
    sleep_period <- if (j == 1) ("Short Sleep Period (Q1)") else if (j == 2) ("Medium Sleep Period (Q2 and Q3)") else ("Long Sleep Period (Q4)")
    
    for (k in seq_along(sleep_insomnia_type)){
      insomnia_k <- sleep_insomnia_type[[k]]
      insomnia <- if (k == 1) ("None") else if (k == 2) ("Mild") else ("Moderate-Severe")
      
      model_tmp <- get(paste0(sub_model_i, sleep_type_j, insomnia_k))
      model_tmp <- as.data.table(summary(model_tmp, delta = 19, level = "aggregate", digits = "asis"))
      
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
      model_tmp[, Insomnia_label := ifelse(Insomnia == "None", "None", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Mild", "Mild", Insomnia_label)]
      model_tmp[, Insomnia_label := ifelse(Insomnia == "Moderate-Severe", "Moderate-Severe", Insomnia_label)]
      
      model_tmp[, Insomnia_label := factor(Insomnia_label, ordered = TRUE,
                                           levels = c("None",
                                                      "Mild",
                                                      "Moderate-Severe"))]
      
      
      model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
      model_tmp[, Sig := NA]
      model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% 19, "*", "")]
      
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
  sub_models_sleep_q_type_i <- sub_models_sleep_q_type_i[, .(Mean = round(Mean, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2), From, To, SleepPeriod, Insomnia, Sig)]
  
  sub_models_obese_insomnia[[i]] <- sub_models_sleep_q_type_i
  
  
}
names(sub_models_obese_insomnia) <- (sub_models_resp)
