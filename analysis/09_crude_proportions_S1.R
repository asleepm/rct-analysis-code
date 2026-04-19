# This script generates Supplementary Table S1 with crude pooled item-level
# accuracy proportions by study arm.

library(readxl)
library(binom)
library(flextable)
library(officer)

# Read source datasets
data1 <- read_excel("data/AI.xlsx")
data2 <- read_excel("data/Control.xlsx")

# Define column ranges
acc_cols <- 20:151
risk_cols <- 20:63
diag_triage_cols <- 64:151

# Check invalid non-missing values in accuracy columns
invalid_ai <- sum(!data1[acc_cols][!is.na(data1[acc_cols])] %in% c("对", "错"))
invalid_control <- sum(!data2[acc_cols][!is.na(data2[acc_cols])] %in% c("对", "错"))

cat("Number of invalid non-missing values in AI accuracy columns:", invalid_ai, "\n")
cat("Number of invalid non-missing values in Control accuracy columns:", invalid_control, "\n")

# Check the number of non-missing responses per row
non_na_per_row_ai <- rowSums(!is.na(data1[acc_cols]))
non_na_per_row_control <- rowSums(!is.na(data2[acc_cols]))

cat("\nAI group mean non-missing responses per row:", mean(non_na_per_row_ai),
    "(min:", min(non_na_per_row_ai), "max:", max(non_na_per_row_ai), ")\n")
cat("Control group mean non-missing responses per row:", mean(non_na_per_row_control),
    "(min:", min(non_na_per_row_control), "max:", max(non_na_per_row_control), ")\n")

# Check non-missing responses by item domain
non_na_risk_ai <- rowSums(!is.na(data1[risk_cols]))
non_na_diag_triage_ai <- rowSums(!is.na(data1[diag_triage_cols]))

cat("\nAI group mean non-missing risk items:", mean(non_na_risk_ai), "\n")
cat("AI group mean non-missing diagnostic/triage items:", mean(non_na_diag_triage_ai), "\n")

non_na_risk_control <- rowSums(!is.na(data2[risk_cols]))
non_na_diag_triage_control <- rowSums(!is.na(data2[diag_triage_cols]))

cat("Control group mean non-missing risk items:", mean(non_na_risk_control), "\n")
cat("Control group mean non-missing diagnostic/triage items:", mean(non_na_diag_triage_control), "\n")

# Calculate crude accuracy
calculate_accuracy <- function(data, selected_columns) {
  correct_answers <- data[, selected_columns] == "对"
  correct_count <- sum(correct_answers, na.rm = TRUE)
  incorrect_count <- sum(!correct_answers, na.rm = TRUE)
  total_count <- correct_count + incorrect_count
  accuracy <- correct_count / total_count
  return(list(
    accuracy = accuracy,
    correct_count = correct_count,
    total_count = total_count
  ))
}

# Overall and domain-specific crude accuracies
group1_result <- calculate_accuracy(data1, 20:151)
group2_result <- calculate_accuracy(data2, 20:151)

type1_columns <- 20:63
type2_columns <- seq(64, 150, by = 2)
type3_columns <- seq(65, 151, by = 2)

group1_type1 <- calculate_accuracy(data1, type1_columns)
group1_type2 <- calculate_accuracy(data1, type2_columns)
group1_type3 <- calculate_accuracy(data1, type3_columns)

group2_type1 <- calculate_accuracy(data2, type1_columns)
group2_type2 <- calculate_accuracy(data2, type2_columns)
group2_type3 <- calculate_accuracy(data2, type3_columns)

# Calculate Wilson 95% confidence intervals
calculate_wilson_ci <- function(correct, total) {
  ci <- binom.confint(correct, total, conf.level = 0.95, methods = "wilson")
  return(c(ci$lower, ci$upper))
}

group1_total_ci <- calculate_wilson_ci(group1_result$correct_count, group1_result$total_count)
group2_total_ci <- calculate_wilson_ci(group2_result$correct_count, group2_result$total_count)

group1_type1_ci <- calculate_wilson_ci(group1_type1$correct_count, group1_type1$total_count)
group1_type2_ci <- calculate_wilson_ci(group1_type2$correct_count, group1_type2$total_count)
group1_type3_ci <- calculate_wilson_ci(group1_type3$correct_count, group1_type3$total_count)

group2_type1_ci <- calculate_wilson_ci(group2_type1$correct_count, group2_type1$total_count)
group2_type2_ci <- calculate_wilson_ci(group2_type2$correct_count, group2_type2$total_count)
group2_type3_ci <- calculate_wilson_ci(group2_type3$correct_count, group2_type3$total_count)

# Calculate crude accuracy differences and approximate 95% confidence intervals
calculate_diff_ci <- function(p1, n1, p2, n2) {
  diff <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  ci_lower <- diff - 1.96 * se
  ci_upper <- diff + 1.96 * se
  return(c(diff, ci_lower, ci_upper))
}

total_diff_ci <- calculate_diff_ci(
  group1_result$accuracy, group1_result$total_count,
  group2_result$accuracy, group2_result$total_count
)

type1_diff_ci <- calculate_diff_ci(
  group1_type1$accuracy, group1_type1$total_count,
  group2_type1$accuracy, group2_type1$total_count
)

type2_diff_ci <- calculate_diff_ci(
  group1_type2$accuracy, group1_type2$total_count,
  group2_type2$accuracy, group2_type2$total_count
)

type3_diff_ci <- calculate_diff_ci(
  group1_type3$accuracy, group1_type3$total_count,
  group2_type3$accuracy, group2_type3$total_count
)

# Chi-square tests based on 2x2 contingency tables
chi_square_test <- function(correct1, total1, correct2, total2) {
  incorrect1 <- total1 - correct1
  incorrect2 <- total2 - correct2
  contingency_table <- matrix(
    c(correct1, incorrect1, correct2, incorrect2),
    nrow = 2,
    byrow = TRUE
  )
  test_result <- chisq.test(contingency_table, correct = FALSE)
  return(test_result$p.value)
}

total_p_value <- chi_square_test(
  group1_result$correct_count, group1_result$total_count,
  group2_result$correct_count, group2_result$total_count
)

type1_p_value <- chi_square_test(
  group1_type1$correct_count, group1_type1$total_count,
  group2_type1$correct_count, group2_type1$total_count
)

type2_p_value <- chi_square_test(
  group1_type2$correct_count, group1_type2$total_count,
  group2_type2$correct_count, group2_type2$total_count
)

type3_p_value <- chi_square_test(
  group1_type3$correct_count, group1_type3$total_count,
  group2_type3$correct_count, group2_type3$total_count
)

# Build the summary table
table_data <- data.frame(
  Type = c("Total", "Risk behaviour", "Diagnostic", "Triage"),
  Group1_Accuracy = c(
    group1_result$accuracy, group1_type1$accuracy, group1_type2$accuracy, group1_type3$accuracy
  ),
  Group1_CI = c(
    sprintf("(%.3f, %.3f)", group1_total_ci[1], group1_total_ci[2]),
    sprintf("(%.3f, %.3f)", group1_type1_ci[1], group1_type1_ci[2]),
    sprintf("(%.3f, %.3f)", group1_type2_ci[1], group1_type2_ci[2]),
    sprintf("(%.3f, %.3f)", group1_type3_ci[1], group1_type3_ci[2])
  ),
  Group2_Accuracy = c(
    group2_result$accuracy, group2_type1$accuracy, group2_type2$accuracy, group2_type3$accuracy
  ),
  Group2_CI = c(
    sprintf("(%.3f, %.3f)", group2_total_ci[1], group2_total_ci[2]),
    sprintf("(%.3f, %.3f)", group2_type1_ci[1], group2_type1_ci[2]),
    sprintf("(%.3f, %.3f)", group2_type2_ci[1], group2_type2_ci[2]),
    sprintf("(%.3f, %.3f)", group2_type3_ci[1], group2_type3_ci[2])
  ),
  Diff_Accuracy = c(
    total_diff_ci[1], type1_diff_ci[1], type2_diff_ci[1], type3_diff_ci[1]
  ),
  Diff_CI = c(
    sprintf("(%.3f, %.3f)", total_diff_ci[2], total_diff_ci[3]),
    sprintf("(%.3f, %.3f)", type1_diff_ci[2], type1_diff_ci[3]),
    sprintf("(%.3f, %.3f)", type2_diff_ci[2], type2_diff_ci[3]),
    sprintf("(%.3f, %.3f)", type3_diff_ci[2], type3_diff_ci[3])
  ),
  P_Value = c(total_p_value, type1_p_value, type2_p_value, type3_p_value)
)

# Format numeric values
table_data$Group1_Accuracy <- sprintf("%.3f", table_data$Group1_Accuracy)
table_data$Group2_Accuracy <- sprintf("%.3f", table_data$Group2_Accuracy)
table_data$Diff_Accuracy <- sprintf("%.3f", table_data$Diff_Accuracy)
table_data$P_Value <- sprintf("%.3f", table_data$P_Value)
table_data$P_Value <- ifelse(as.numeric(table_data$P_Value) < 0.001, "<0.001", table_data$P_Value)

# Create a booktabs-style flextable
ft <- flextable(table_data)
ft <- set_header_labels(
  ft,
  Type = "Type",
  Group1_Accuracy = "Accuracy",
  Group1_CI = "95% CI",
  Group2_Accuracy = "Accuracy",
  Group2_CI = "95% CI",
  Diff_Accuracy = "Accuracy Difference",
  Diff_CI = "Difference 95% CI",
  P_Value = "P-Value"
)

ft <- add_header_row(
  ft,
  values = c("", "AI Group", "", "Control Group", "", "Comparison", "", ""),
  top = TRUE
)
ft <- merge_h(ft, part = "header")
ft <- theme_booktabs(ft)
ft <- align(ft, align = "center", part = "all")
ft <- fontsize(ft, size = 11, part = "all")
ft <- set_table_properties(ft, width = 0.95)
ft <- padding(ft, padding = 2, part = "all")

# Export the table to a Word document
doc <- read_docx()
doc <- body_add_par(
  doc,
  value = "Supplementary Table S1. Crude item-level pooled proportions, and Wilson 95% confidence intervals in the primary compliant-case analysis.",
  style = "Normal"
)
doc <- body_add_flextable(doc, value = ft)
doc <- body_add_par(
  doc,
  value = "Note: Values are crude pooled item-level proportions calculated directly from observed responses, with Wilson 95% confidence intervals. Accuracy differences were calculated as crude between-group differences (AI minus Control), and P values were derived from chi-square tests without continuity correction. Blank cells indicate unasked items and were excluded from the denominator. These crude proportions are provided for descriptive comparison and may differ from the model-based marginal accuracies estimated from the GLMM in the primary and sensitivity analyses.",
  style = "Normal"
)

print(doc, target = "Supplementary Table S1.docx")