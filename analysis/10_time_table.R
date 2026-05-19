# This script generates a summary table of completion time by study arm.

# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)
library(ggsignif)
library(grid)
library(gridExtra)
library(stringr)
library(knitr)
library(kableExtra)
library(flextable)
library(tidyr)
library(officer)

# Read source datasets
data1 <- read_excel("data/LungDiag.xlsx") %>% mutate(Group = "AI Group")
data2 <- read_excel("data/Control.xlsx") %>% mutate(Group = "Control Group")
data_all <- bind_rows(data1, data2)

# Descriptive statistics: median and interquartile range
summary_data <- data_all %>%
  group_by(Group) %>%
  summarise(
    Median = median(TIME, na.rm = TRUE),
    Q1 = quantile(TIME, 0.25, na.rm = TRUE),
    Q3 = quantile(TIME, 0.75, na.rm = TRUE),
    IQR = IQR(TIME, na.rm = TRUE)
  )

# Normality assessment
cat("Shapiro-Wilk normality test results:\n")
print(shapiro.test(data1$TIME))
print(shapiro.test(data2$TIME))

# Wilcoxon rank-sum test
wilcox_result <- wilcox.test(TIME ~ Group, data = data_all)
print(wilcox_result)

# Format P value
p_value <- wilcox_result$p.value
p_value_formatted <- ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))

# Create table rows
median_row <- summary_data %>%
  select(Group, Median) %>%
  pivot_wider(names_from = Group, values_from = Median) %>%
  mutate(Variable = "Median (seconds)", .before = 1) %>%
  mutate(`P-value` = p_value_formatted)

iqr_row <- summary_data %>%
  select(Group, IQR) %>%
  pivot_wider(names_from = Group, values_from = IQR) %>%
  mutate(Variable = "IQR (seconds)", .before = 1) %>%
  mutate(`P-value` = "")

table_data <- bind_rows(median_row, iqr_row)

# Create a booktabs-style flextable
ft <- flextable(table_data)
ft <- set_header_labels(
  ft,
  Variable = "",
  `AI Group` = "LungDiag Group",
  `Control Group` = "Control Group",
  `P-value` = "P-value"
)
ft <- add_header_row(ft, values = c("", "Group", ""), colwidths = c(1, 2, 1))
ft <- theme_booktabs(ft)
ft <- fontsize(ft, size = 12, part = "all")
ft <- autofit(ft)
ft <- align(ft, align = "center", part = "all")

# Export the table to a Word document with title and note
doc <- read_docx()
doc <- body_add_par(
  doc,
  value = "Table 3: Median Task Completion Times and Interquartile Ranges (IQR) Between the LungDiag Group and Control Group, with Statistical Comparison",
  style = "Normal"
)
doc <- body_add_flextable(doc, value = ft)
doc <- body_add_par(
  doc,
  value = "Note: Completion time is presented as median and interquartile range (IQR) in seconds because the distribution was non-normal. Between-group comparisons were performed using a two-sided Wilcoxon rank-sum test. Shapiro–Wilk tests were used to assess normality and are not shown in the table. Analyses were based on observed completion-time data from the LungDiag and Control groups.",
  style = "Normal"
)

print(doc, target = "Table3.docx")