# ----------- Load Required Packages ------------
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")
if (!requireNamespace("tableone", quietly = TRUE)) install.packages("tableone")

library(tableone)
library(readxl)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# ----------- Define Constants ------------
num_vars <- c(
  "TIME", "Height", "Weight", "Age", "The_number_of_visits_of_doctors",
  "Number_of_emergency_room_visits", "Number_of_hospitalizations"
)

value_labels <- list(
  Sex = c("1" = "Male", "2" = "Female"),
  Partner = c("1" = "With Partner", "2" = "Without Partner"),
  Educational_attainment = c(
    "1" = "High School",
    "2" = "High School/GED",
    "3" = "College",
    "4" = "Bachelor's",
    "5" = "Postgraduate"
  ),
  Health_insurance_coverage = c(
    "1" = "Uninsured",
    "2" = "Medicare",
    "3" = "Medicaid",
    "4" = "Both Medicare and Medicaid",
    "5" = "Private/Employer",
    "6" = "Uncertain"
  ),
  Self_assessment_of_health_status = c(
    "1" = "Excellent",
    "2" = "Good",
    "3" = "Fair",
    "4" = "Poor"
  ),
  Work = c("1" = "Yes", "2" = "No"),
  Current_smoking_status = c("1" = "Never", "2" = "Former", "3" = "Current"),
  The_frequency_and_types_of_alcohol_consumption = c(
    "1" = "None",
    "2" = "Occasional (<4/month)",
    "3" = "Frequent (≥3/week)"
  ),
  Household_income = c(
    "1" = "<210k",
    "2" = "210–350k",
    "3" = "350–560k",
    "4" = "560–700k",
    "5" = "700–1050k",
    "6" = "1050–1400k",
    "7" = "≥1400k"
  ),
  The_number_of_chronic_diseases = c(
    "1" = "0",
    "2" = "1",
    "3" = "2",
    "4" = ">2",
    "5" = "Uncertain"
  ),
  Territory = c(
    "Northern China" = "Northern China",
    "Southern China" = "Southern China",
    "Eastern China" = "Eastern China",
    "Western China" = "Western China",
    "Central China" = "Central China"
  )
)

var_descriptions <- c(
  "Sex" = "Sex",
  "Partner" = "Partner status",
  "Educational_attainment" = "Education",
  "Health_insurance_coverage" = "Insurance type",
  "Self_assessment_of_health_status" = "Self-rated health",
  "Work" = "Employment status",
  "Current_smoking_status" = "Smoking status",
  "The_frequency_and_types_of_alcohol_consumption" = "Alcohol use",
  "Household_income" = "Annual household income (RMB)",
  "The_number_of_chronic_diseases" = "Chronic diseases",
  "Territory" = "Region",
  "Height" = "Height (cm)",
  "Weight" = "Weight (kg)",
  "Age" = "Age",
  "The_number_of_visits_of_doctors" = "Doctor visits (past 6 mo)",
  "Number_of_emergency_room_visits" = "ER visits (past 6 mo)",
  "Number_of_hospitalizations" = "Hospitalizations (past 6 mo)"
)

# ----------- Step 1: Read Data and Add Group Labels ------------
if (!file.exists("data/AI.xlsx")) stop("File not found: data/AI.xlsx")
if (!file.exists("data/Control.xlsx")) stop("File not found: data/Control.xlsx")

data1 <- read_excel("data/AI.xlsx") %>% dplyr::select(1:19)
data2 <- read_excel("data/Control.xlsx") %>% dplyr::select(1:19)

data1$group <- "AI Group"
data2$group <- "Control Group"
combined_data <- bind_rows(data1, data2)

for (var in names(value_labels)) {
  combined_data[[var]] <- factor(
    combined_data[[var]],
    levels = names(value_labels[[var]]),
    labels = value_labels[[var]]
  )
}

combined_data[num_vars] <- lapply(
  combined_data[num_vars],
  function(x) as.numeric(as.character(x))
)

if (any(is.na(combined_data[num_vars]))) {
  warning("NA introduced during numeric conversion")
}

# ----------- Data Quality Checks ------------
na_ai <- colSums(is.na(data1[num_vars]))
na_control <- colSums(is.na(data2[num_vars]))
cat("Missing values in AI group:\n"); print(na_ai)
cat("\nMissing values in Control group:\n"); print(na_control)

types_ai <- sapply(data1[num_vars], class)
cat("\nVariable classes in AI group:\n"); print(types_ai)
types_control <- sapply(data2[num_vars], class)
cat("\nVariable classes in Control group:\n"); print(types_control)

data1_temp <- data1
data1_temp[num_vars] <- lapply(data1_temp[num_vars], as.numeric)
outliers_ai <- sapply(num_vars, function(col) sum(data1_temp[[col]] < 0 | is.na(data1_temp[[col]])))
cat("\nPotential anomalies in AI group (negative values or NA):\n"); print(outliers_ai)

data2_temp <- data2
data2_temp[num_vars] <- lapply(data2_temp[num_vars], as.numeric)
outliers_control <- sapply(num_vars, function(col) sum(data2_temp[[col]] < 0 | is.na(data2_temp[[col]])))
cat("\nPotential anomalies in Control group (negative values or NA):\n"); print(outliers_control)

# ----------- Step 2: Generate Baseline Table ------------
generate_baseline_table <- function(data) {
  tryCatch({
    cat_vars <- names(value_labels)
    cont_vars <- setdiff(names(var_descriptions), cat_vars)
    
    tab1_normal <- CreateTableOne(
      vars = c(cat_vars, cont_vars),
      strata = "group",
      data = data,
      factorVars = cat_vars,
      test = FALSE,
      smd = TRUE
    )
    
    tab1_nonnormal <- CreateTableOne(
      vars = c(cat_vars, cont_vars),
      strata = "group",
      data = data,
      factorVars = cat_vars,
      test = FALSE,
      smd = TRUE
    )
    
    tab_mat_normal <- print(
      tab1_normal,
      showAllLevels = FALSE,
      smd = TRUE,
      quote = FALSE,
      noSpaces = FALSE,
      printToggle = FALSE
    )
    
    tab_mat_nonnormal <- print(
      tab1_nonnormal,
      nonnormal = cont_vars,
      showAllLevels = FALSE,
      smd = TRUE,
      quote = FALSE,
      noSpaces = FALSE,
      printToggle = FALSE
    )
    
    tab_df <- data.frame(
      `Category/Statistic` = rownames(tab_mat_normal),
      tab_mat_normal,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    colnames(tab_df) <- c("Category/Statistic", "AI Group", "Control Group", "SMD")
    tab_df <- tab_df[tab_df$`Category/Statistic` != "n", ]
    
    tab_df_non <- data.frame(
      `Category/Statistic` = rownames(tab_mat_nonnormal),
      tab_mat_nonnormal,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    colnames(tab_df_non) <- c("Category/Statistic", "AI Group", "Control Group", "SMD")
    tab_df_non <- tab_df_non[tab_df_non$`Category/Statistic` != "n", ]
    
    clean_cat <- sub("\\s*\\(.*$", "", trimws(tab_df_non$`Category/Statistic`))
    cont_rows_non <- tab_df_non[clean_cat %in% cont_vars, ]
    
    final_df <- data.frame(
      Variable = character(),
      `Category/Statistic` = character(),
      `AI Group` = character(),
      `Control Group` = character(),
      SMD = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(tab_df)) {
      row_text <- trimws(tab_df$`Category/Statistic`[i])
      cleaned_var <- sub("\\s*\\(.*$", "", row_text)
      
      if (cleaned_var %in% cont_vars) {
        var_row <- tab_df[i, ]
        var_row$`Category/Statistic` <- cleaned_var
        var_row$`AI Group` <- ""
        var_row$`Control Group` <- ""
        final_df <- rbind(final_df, var_row)
        
        mean_row <- tab_df[i, ]
        mean_row$`Category/Statistic` <- "Mean ± SD"
        mean_row$SMD <- ""
        final_df <- rbind(final_df, mean_row)
        
        median_row <- cont_rows_non[
          sub("\\s*\\(.*$", "", trimws(cont_rows_non$`Category/Statistic`)) == cleaned_var,
        ]
        
        if (nrow(median_row) > 0) {
          median_row$`Category/Statistic` <- "Median (IQR)"
          median_row$`AI Group` <- gsub("\\[", "", gsub("\\]", "", gsub(", ", " to ", median_row$`AI Group`)))
          median_row$`Control Group` <- gsub("\\[", "", gsub("\\]", "", gsub(", ", " to ", median_row$`Control Group`)))
          median_row$SMD <- ""
          final_df <- rbind(final_df, median_row)
        }
      } else {
        final_df <- rbind(final_df, tab_df[i, ])
      }
    }
    
    current_var <- ""
    final_df$Variable <- NA
    
    for (i in 1:nrow(final_df)) {
      row_text <- final_df$`Category/Statistic`[i]
      if (row_text == "" || is.na(row_text)) next
      
      if (grepl("^\\s+", row_text) || row_text %in% c("Mean ± SD", "Median (IQR)")) {
        final_df$Variable[i] <- current_var
        if (row_text %in% c("Mean ± SD", "Median (IQR)")) {
          final_df$`Category/Statistic`[i] <- row_text
        } else {
          final_df$`Category/Statistic`[i] <- trimws(row_text)
        }
      } else {
        cleaned_var <- sub("\\s*\\(.*$", "", trimws(row_text))
        if (cleaned_var == "" || !cleaned_var %in% names(var_descriptions)) next
        current_var <- var_descriptions[cleaned_var]
        final_df$Variable[i] <- current_var
        final_df$`Category/Statistic`[i] <- ""
      }
    }
    
    final_df <- final_df[!is.na(final_df$Variable), ]
    final_df$SMD <- as.numeric(final_df$SMD)
    final_df$SMD[is.na(final_df$SMD)] <- ""
    
    final_df <- final_df[, c("Variable", "Category/Statistic", "AI Group", "Control Group", "SMD")]
    return(final_df)
  }, error = function(e) {
    stop("Error in table generation: ", e$message)
  })
}

baseline_table <- generate_baseline_table(combined_data)

# ----------- Step 3: Export to Word ------------
ft <- flextable(baseline_table) %>%
  set_header_labels(
    Variable = "Variable",
    `Category/Statistic` = "Category/Statistic",
    `AI Group` = "AI Group",
    `Control Group` = "Control Group",
    SMD = "SMD"
  ) %>%
  theme_vanilla() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>%
  align(align = "left", j = 1, part = "body") %>%
  width(
    j = c("Variable", "Category/Statistic", "AI Group", "Control Group", "SMD"),
    width = c(2.5, 2.5, 2, 2, 1.2)
  ) %>%
  merge_v(j = "Variable") %>%
  add_header_lines("Table 1. Baseline characteristics of participants") %>%
  add_footer_lines(
    "Note: SMD = standardized mean difference; categorical variables are shown as counts (percentage); continuous variables are shown as mean ± SD and median (IQR)."
  ) %>%
  bg(
    i = which(baseline_table$`Category/Statistic` %in% c("Mean ± SD", "Median (IQR)")),
    bg = "#f6f6f6"
  )

doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_end_section_portrait()

print(doc, target = "Table1.docx")