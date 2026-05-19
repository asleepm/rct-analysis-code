# 1. Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(marginaleffects)
library(flextable)
library(officer)

# 2. Read datasets and assign study arm and participant IDs
data1 <- read_excel("data/LungDiag.xlsx")
data2 <- read_excel("data/Control.xlsx")

data1$group <- "AI"
data1$sub_id <- paste0("AI_", 1:nrow(data1))
data2$group <- "Control"
data2$sub_id <- paste0("Ctrl_", 1:nrow(data2))

# 3. Identify item columns and classify item domains
# Column indices are based on the structure of the original spreadsheets.
all_cols <- colnames(data1)
acc_col_names <- all_cols[20:151]
risk_names <- all_cols[20:63]
diag_names <- all_cols[seq(64, 150, by = 2)]
triage_names <- all_cols[seq(65, 151, by = 2)]

# 4. Merge the two datasets and reshape from wide to long format
df_combined <- bind_rows(
  data1[, c("sub_id", "group", acc_col_names)],
  data2[, c("sub_id", "group", acc_col_names)]
)

df_long <- pivot_longer(
  df_combined,
  cols = all_of(acc_col_names),
  names_to = "item_id",
  values_to = "response"
)

# Clean data and derive analysis variables
# Raw response values in the source spreadsheets are stored in Chinese:
# "对" = correct; "错" = incorrect.
df_clean <- df_long %>%
  filter(!is.na(response), response %in% c("对", "错")) %>%
  rename(study_arm = group) %>%
  mutate(
    correct = ifelse(response == "对", 1, 0),
    study_arm = factor(study_arm, levels = c("Control", "AI")),
    item_type = case_when(
      item_id %in% risk_names ~ "Risk",
      item_id %in% diag_names ~ "Diagnostic",
      item_id %in% triage_names ~ "Triage",
      TRUE ~ "Unknown"
    )
  )

# Disable the default safety warning from marginaleffects for this workflow
options(marginaleffects_safe = FALSE)

# 5. Define a core function to fit the GLMM and extract marginal effects
run_glmm_analysis <- function(data_subset, type_name) {
  cat(paste0("Fitting the ", type_name, " model...\n"))
  
  # Fit a GLMM with random intercepts for participant and item
  model <- glmer(
    correct ~ study_arm + (1 | sub_id) + (1 | item_id),
    data = data_subset,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  # Estimate model-adjusted marginal mean accuracy for each study arm
  pred_means <- avg_predictions(model, variables = "study_arm", re.form = NA)
  acc_ctrl <- pred_means %>% filter(study_arm == "Control")
  acc_ai <- pred_means %>% filter(study_arm == "AI")
  
  # Estimate the risk difference (RD)
  rd <- avg_comparisons(
    model,
    variables = "study_arm",
    comparison = "difference",
    re.form = NA
  )
  
  # Estimate the risk ratio (RR)
  rr <- avg_comparisons(
    model,
    variables = "study_arm",
    comparison = "ratio",
    re.form = NA
  )
  
  # Return all key estimates in a structured list
  return(list(
    Type = type_name,
    Group1_Acc = acc_ai$estimate,
    Group1_CI_L = acc_ai$conf.low,
    Group1_CI_U = acc_ai$conf.high,
    Group2_Acc = acc_ctrl$estimate,
    Group2_CI_L = acc_ctrl$conf.low,
    Group2_CI_U = acc_ctrl$conf.high,
    Diff_Acc = rd$estimate,
    Diff_CI_L = rd$conf.low,
    Diff_CI_U = rd$conf.high,
    P_Value = rd$p.value,
    RR = rr$estimate,
    RR_CI_L = rr$conf.low,
    RR_CI_U = rr$conf.high
  ))
}

# 6. Run models for the total score and for each item domain
res_total <- run_glmm_analysis(df_clean, "Total")
res_risk <- run_glmm_analysis(df_clean %>% filter(item_type == "Risk"), "Risk behaviour")
res_diag <- run_glmm_analysis(df_clean %>% filter(item_type == "Diagnostic"), "Diagnostic")
res_triage <- run_glmm_analysis(df_clean %>% filter(item_type == "Triage"), "Triage")

# 7. Combine results and format them as a summary table
results_list <- list(res_total, res_risk, res_diag, res_triage)

table_data <- data.frame(
  Type = sapply(results_list, function(x) x$Type),
  
  Group1_Accuracy = sprintf("%.3f", sapply(results_list, function(x) x$Group1_Acc)),
  Group1_CI = sprintf(
    "(%.3f, %.3f)",
    sapply(results_list, function(x) x$Group1_CI_L),
    sapply(results_list, function(x) x$Group1_CI_U)
  ),
  
  Group2_Accuracy = sprintf("%.3f", sapply(results_list, function(x) x$Group2_Acc)),
  Group2_CI = sprintf(
    "(%.3f, %.3f)",
    sapply(results_list, function(x) x$Group2_CI_L),
    sapply(results_list, function(x) x$Group2_CI_U)
  ),
  
  Diff_Accuracy = sprintf("%.3f", sapply(results_list, function(x) x$Diff_Acc)),
  Diff_CI = sprintf(
    "(%.3f, %.3f)",
    sapply(results_list, function(x) x$Diff_CI_L),
    sapply(results_list, function(x) x$Diff_CI_U)
  ),
  
  RR = sprintf("%.2f", sapply(results_list, function(x) x$RR)),
  RR_CI = sprintf(
    "(%.2f, %.2f)",
    sapply(results_list, function(x) x$RR_CI_L),
    sapply(results_list, function(x) x$RR_CI_U)
  ),
  
  P_Value = sprintf("%.3f", sapply(results_list, function(x) x$P_Value))
)

# Display P values smaller than 0.001 as <0.001
table_data$P_Value <- ifelse(
  as.numeric(table_data$P_Value) < 0.001,
  "<0.001",
  table_data$P_Value
)

# 8. Create a booktabs-style flextable including RR
ft <- flextable(table_data)
ft <- set_header_labels(
  ft,
  Type = "Type",
  Group1_Accuracy = "Accuracy",
  Group1_CI = "95% CI",
  Group2_Accuracy = "Accuracy",
  Group2_CI = "95% CI",
  Diff_Accuracy = "Risk Difference (RD)",
  Diff_CI = "RD 95% CI",
  RR = "Risk Ratio (RR)",
  RR_CI = "RR 95% CI",
  P_Value = "P-Value"
)

# Add a grouped header row
ft <- add_header_row(
  ft,
  values = c("", "LungDiag Group", "", "Control Group", "", "Comparison", "", "", "", ""),
  top = TRUE
)
ft <- merge_h(ft, part = "header")
ft <- theme_booktabs(ft)
ft <- align(ft, align = "center", part = "all")
ft <- fontsize(ft, size = 10, part = "all")
ft <- set_table_properties(ft, width = 1)
ft <- padding(ft, padding = 2, part = "all")

# 9. Export the table to a Word document
doc <- read_docx()
doc <- body_add_par(
  doc,
  value = "Table 2. Model-based predicted accuracy and treatment effects from GLMM in the primary compliant-case analysis",
  style = "Normal"
)
doc <- body_add_flextable(doc, value = ft)

# Add a table note to the Word document
doc <- body_add_par(
  doc,
  value = "Note: Model-based marginal accuracies are predicted probabilities from the fitted GLMM and may differ from crude pooled proportions. Crude item-level proportions are reported in Supplementary Table S1. Abbreviations: CI, confidence interval; GLMM, generalized linear mixed-effects model; RD, risk difference; RR, risk ratio.",
  style = "Normal"
)

print(doc, target = "Table2.docx")

cat("\nGLMM analysis completed. Results were saved to Table2.docx\n")