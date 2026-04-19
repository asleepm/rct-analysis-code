# This script prepares the control-arm ITT dataset for Panel A analysis by
# imputing baseline variables and recoding selected unanswered questionnaire items.

# Load required packages
library(readxl)
library(dplyr)
library(mice)
library(writexl)

# ==========================================
# Step 1: Read the source dataset
# ==========================================
# Keep character content as imported from Excel and handle missing values explicitly.
file_path <- "data/control_corrected.xlsx"
df <- read_excel(file_path, na = "")

# ==========================================
# Step 2: Extract and process baseline variables (columns 2-19)
# ==========================================
df_baseline <- df[, 2:19]

# Convert the literal text string "NA" to a true missing value NA
df_baseline[df_baseline == "NA"] <- NA

# Convert columns to appropriate data types automatically
# Numeric-like columns will be converted to numeric, and text-like columns
# will be converted according to base R's type.convert behavior.
df_baseline <- df_baseline %>%
  mutate(across(everything(), ~ type.convert(.x, as.is = FALSE)))

# ==========================================
# Step 3: Perform multiple imputation for baseline variables
# ==========================================
print("Performing multiple imputation for baseline variables...")

imp <- mice(
  df_baseline,
  m = 5,
  method = "pmm",
  seed = 2026,
  printFlag = FALSE
)

# Extract the first completed dataset
df_baseline_imputed <- complete(imp, 1)

# ==========================================
# Step 4: Extract and process questionnaire outcome variables (columns 20-151)
# ==========================================
df_outcomes <- df[, 20:151]

# In the source spreadsheet, the text string "NA" indicates an item that was
# selected but left unanswered. According to the predefined rule for this ITT
# dataset variant, these entries are recoded as "对".
# True blank cells (actual missing values, NA) are kept unchanged.
df_outcomes[df_outcomes == "NA"] <- "对"

# ==========================================
# Step 5: Recombine the dataset and export
# ==========================================
df_final <- bind_cols(ID = df[[1]], df_baseline_imputed, df_outcomes)

# Export the processed dataset to a new Excel file
output_name <- "data/control_itt_imputed_na_as_correct.xlsx"
write_xlsx(df_final, output_name)

print(paste("Processing completed. Output file saved as:", output_name))