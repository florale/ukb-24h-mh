
redir <- paste0("/Users/", Sys.info()[["user"]], "/Library/CloudStorage/OneDrive-Personal/github/projects/ukbiobank/")
outputdir <- paste0("/Users/", Sys.info()[["user"]], "/Library/CloudStorage/OneDrive-Personal/monash/projects/ukbiobank/ukb-24h-mh/output/")

pal_type <- c("#4E2F26",
              "#944C4C", "#d65b5a", "#efcbcb", "#dfa398", "#9c6755", "#ea967c", "#f5c98e", 
              "#EAD3BF",
              "#8DA290", "#4F7375", "#8CAACB", "#4682b4", "#3b3960", "#717497",
              "#666666" 
)
pal_time <- c("#B49797", "#8AAFCA", "#456691")
pal_time <- c("#EAD3BF", "#8AAFCA", "#ba6c6e")
pal_time <- c("#EAD3BF", "#B49797", "#708885")
pal_time <- c("#EAD3BF", "#B49797", "#708885", "#EAD3BF", "#B49797", "#708885", "#EAD3BF", "#B49797", "#708885", "#EAD3BF", "#B49797", "#708885", "#EAD3BF", "#B49797", "#708885")
fcol <- c("#FAF7F3", "#999999", "#ADC7DA", "#3C5C76", "#D2E0EA", "#CE7870")

extract_rhat <- function(model, model_name) {
  # Get R-hat values
  rhat_values <- rhat(model$model, 
                      parameters = c("b_Intercept", 
                                     "ilr1", "ilr2", "ilr3"))
  # Create summary
  rhat_summary <- data.frame(
    Model = model_name,
    Min = min(rhat_values, na.rm = TRUE),
    Max = max(rhat_values, na.rm = TRUE),
    Mean = mean(rhat_values, na.rm = TRUE),
    Median = median(rhat_values, na.rm = TRUE),
    N_above_1.01 = sum(rhat_values > 1.01, na.rm = TRUE)
    #  N_above_1.1 = sum(rhat_values > 1.1, na.rm = TRUE),
    #  Total_params = length(rhat_values)
  )
  
  return(list(
    summary = rhat_summary,
    values = rhat_values
  ))
}

extract_ess <- function(model, model_name) {
  ess_values <- effective_sample(model$model, 
                                 parameters = c("b_Intercept", 
                                                "ilr1", "ilr2", "ilr3"))
  ess_bulk_summary <- data.frame(
    Model = model_name,
    Min = min(ess_values$ESS, na.rm = TRUE),
    Max = max(ess_values$ESS, na.rm = TRUE),
    Mean = mean(ess_values$ESS, na.rm = TRUE),
    Median = median(ess_values$ESS, na.rm = TRUE),
    N_below_400 = sum(ess_values$ESS < 400, na.rm = TRUE)
  )
  ess_tail_summary <- data.frame(
    Model = model_name,
    Min = min(ess_values$ESS_tail, na.rm = TRUE),
    Max = max(ess_values$ESS_tail, na.rm = TRUE),
    Mean = mean(ess_values$ESS_tail, na.rm = TRUE),
    Median = median(ess_values$ESS_tail, na.rm = TRUE),
    N_below_400 = sum(ess_values$ESS_tail < 400, na.rm = TRUE)
  )
  return(list(
    bulk_summary = ess_bulk_summary,
    tail_summary = ess_tail_summary,
    values = ess_values
  ))
}
