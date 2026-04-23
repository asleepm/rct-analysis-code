# ==========================================================
# Subgroup analyses and forest plot
# ==========================================================

# ---------- Load required packages ----------
library(tidyverse)
library(readxl)
library(lme4)
library(emmeans)
library(forestplot)
library(grid)
library(DHARMa)
library(Cairo)

make_plot_df <- function(model, subgroup_var, var_label,
                         summary_table, ctrl_lab, exp_lab) {
  # Build formula such as ~ Group | Sex
  fml <- as.formula(paste("~ Group |", subgroup_var))
  
  # Extract predicted probabilities on the response scale
  emm <- emmeans(model, fml, type = "response", regrid = "response")
  
  # Calculate the probability difference: Experimental - Control
  contrast_tbl <- contrast(emm, method = "revpairwise", adjust = "none") |>
    summary(infer = TRUE) |>
    as_tibble()
  
  if (!"contrast" %in% names(contrast_tbl)) {
    stop("The contrast table does not contain a 'contrast' column.")
  }
  
  filtered <- contrast_tbl |> filter(contrast == "Experimental - Control")
  if (nrow(filtered) == 0) {
    stop("No 'Experimental - Control' contrast was found. Please check the subgroup variable.")
  }
  
  diff_tbl <- filtered |>
    rename(
      label = !!sym(subgroup_var),
      mean  = estimate,
      lower = asymp.LCL,
      upper = asymp.UCL
    ) |>
    select(label, mean, lower, upper)
  
  # Append crude subgroup-specific accuracy summaries
  plot_df <- diff_tbl |>
    left_join(
      summary_table |>
        filter(Group == "Experimental") |>
        mutate(group1 = sprintf("%.1f%% (%d/%d)", correct / total * 100, correct, total)),
      by = c("label" = subgroup_var)
    ) |>
    left_join(
      summary_table |>
        filter(Group == "Control") |>
        mutate(group2 = sprintf("%.1f%% (%d/%d)", correct / total * 100, correct, total)),
      by = c("label" = subgroup_var)
    ) |>
    mutate(
      subgroup = sprintf("%s: %s", var_label, label)
    )
  
  return(plot_df)
}

# ==========================================================
# 1. Sex subgroup
# ==========================================================
exp_data  <- read_excel("data/AI.xlsx")      |> mutate(ID = paste0("Exp_", ID), Group = "Experimental")
ctrl_data <- read_excel("data/Control.xlsx") |> mutate(ID = paste0("Ctrl_", ID), Group = "Control")
data_combined <- bind_rows(exp_data, ctrl_data)

data_combined <- data_combined |>
  mutate(
    Sex   = factor(Sex, levels = c("1", "2"), labels = c("Male", "Female")),
    Group = factor(Group, levels = c("Control", "Experimental")),
    ID    = factor(ID)
  )

data_long <- data_combined |>
  select(ID, Group, Sex, `1`:`132`) |>
  pivot_longer(cols = `1`:`132`, names_to = "ItemID", values_to = "Response") |>
  filter(!is.na(Response)) |>
  mutate(
    Response = ifelse(Response == "对", 1, 0),
    ItemID   = factor(ItemID)
  )

model_sex <- glmer(
  Response ~ Sex * Group + (1 | ID) + (1 | ItemID),
  data    = data_long,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary_table_sex <- data_long |>
  group_by(Sex, Group) |>
  summarise(correct = sum(Response == 1), total = n(), .groups = "drop")

plot_df_sex <- make_plot_df(
  model_sex, "Sex", "Sex",
  summary_table_sex, "Control", "Experimental"
) |>
  mutate(group = "Sex")

# ==========================================================
# 2. Age subgroup (<30 vs 30+)
# ==========================================================
data_combined <- bind_rows(exp_data, ctrl_data) |>
  mutate(
    Age = as.numeric(str_remove_all(Age, "[^0-9\\.]+")),
    AgeGroup = cut(Age, breaks = c(-Inf, 30, Inf), labels = c("<30", "30+"), right = FALSE),
    Group = factor(Group, levels = c("Control", "Experimental")),
    ID    = factor(ID)
  ) |>
  filter(!is.na(AgeGroup))

data_long <- data_combined |>
  select(ID, AgeGroup, Group, `1`:`132`) |>
  pivot_longer(cols = `1`:`132`, names_to = "ItemID", values_to = "Response") |>
  filter(!is.na(Response)) |>
  mutate(
    Response = ifelse(Response == "对", 1, 0),
    ItemID   = factor(ItemID)
  )

model_age <- glmer(
  Response ~ AgeGroup * Group + (1 | ID) + (1 | ItemID),
  data    = data_long,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary_table_age <- data_long |>
  group_by(AgeGroup, Group) |>
  summarise(correct = sum(Response == 1), total = n(), .groups = "drop")

plot_df_age <- make_plot_df(
  model_age, "AgeGroup", "Age",
  summary_table_age, "Control", "Experimental"
) |>
  mutate(group = "Age Group")

# ==========================================================
# 3. Education subgroup
# ==========================================================
data_combined <- bind_rows(exp_data, ctrl_data) |>
  filter(!is.na(Educational_attainment)) |>
  mutate(
    EduGroup2 = case_when(
      Educational_attainment %in% c(1, 2, 3) ~ "Without bachelor's degree / current undergraduate",
      Educational_attainment %in% c(4, 5)    ~ "Bachelor's degree or higher",
      TRUE ~ NA_character_
    ),
    Group = factor(Group, levels = c("Control", "Experimental")),
    ID    = factor(ID)
  ) |>
  filter(!is.na(EduGroup2))

data_long <- data_combined |>
  select(ID, EduGroup2, Group, `1`:`132`) |>
  pivot_longer(cols = `1`:`132`, names_to = "ItemID", values_to = "Response") |>
  filter(!is.na(Response)) |>
  mutate(
    Response = ifelse(Response == "对", 1, 0),
    ItemID   = factor(ItemID)
  )

model_edu <- glmer(
  Response ~ EduGroup2 * Group + (1 | ID) + (1 | ItemID),
  data    = data_long,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary_table_edu <- data_long |>
  group_by(EduGroup2, Group) |>
  summarise(correct = sum(Response == 1), total = n(), .groups = "drop")

plot_df_edu <- make_plot_df(
  model_edu, "EduGroup2", "Education",
  summary_table_edu, "Control", "Experimental"
) |>
  mutate(group = "Education")

# ==========================================================
# 4. Region subgroup
# ==========================================================
data_combined <- bind_rows(exp_data, ctrl_data) |>
  filter(!is.na(Territory)) |>
  mutate(
    Territory = factor(Territory),
    Group     = factor(Group, levels = c("Control", "Experimental")),
    ID        = factor(ID)
  )

data_long <- data_combined |>
  select(ID, Territory, Group, `1`:`132`) |>
  pivot_longer(cols = `1`:`132`, names_to = "ItemID", values_to = "Response") |>
  filter(!is.na(Response)) |>
  mutate(
    Response = ifelse(Response == "对", 1, 0),
    ItemID   = factor(ItemID)
  )

model_region <- glmer(
  Response ~ Territory * Group + (1 | ID) + (1 | ItemID),
  data    = data_long,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary_table_region <- data_long |>
  group_by(Territory, Group) |>
  summarise(correct = sum(Response == 1), total = n(), .groups = "drop")

plot_df_region <- make_plot_df(
  model_region, "Territory", "Region",
  summary_table_region, "Control", "Experimental"
) |>
  mutate(group = "Region")

# ==========================================================
# 5. Combine results and create the forest plot
# ==========================================================
plot_df_all <- bind_rows(
  plot_df_sex, plot_df_age, plot_df_edu, plot_df_region
) |>
  mutate(indent_label = paste0("   ", subgroup))

group_headers <- plot_df_all |>
  distinct(group) |>
  mutate(
    indent_label = group,
    subgroup = NA_character_,
    group1 = NA_character_,
    group2 = NA_character_,
    mean = NA_real_,
    lower = NA_real_,
    upper = NA_real_
  )

plot_df_final <- bind_rows(group_headers, plot_df_all) |>
  arrange(factor(group, levels = c("Sex", "Age Group", "Education", "Region")))

tabletext <- cbind(
  c("Subgroup", plot_df_final$indent_label),
  c("AI", plot_df_final$group1),
  c("Control", plot_df_final$group2),
  c(
    "AI - Control Diff (95% CI)",
    ifelse(
      is.na(plot_df_final$mean), "",
      sprintf(
        "%.1f (%.1f to %.1f)",
        plot_df_final$mean * 100,
        plot_df_final$lower * 100,
        plot_df_final$upper * 100
      )
    )
  )
)

CairoPDF("Figure4.pdf",
         width = 13, height = 8.2, family = "Times New Roman")

forestplot(
  labeltext = tabletext,
  mean      = c(NA, plot_df_final$mean  * 100),
  lower     = c(NA, plot_df_final$lower * 100),
  upper     = c(NA, plot_df_final$upper * 100),
  is.summary = FALSE,
  zero      = 0,
  boxsize   = 0.2,
  col       = fpColors(box = "black", line = "black", summary = "black"),
  xticks    = seq(-10, 30, 10),
  xlab      = "Absolute Difference (AI - Control, percentage points)",
  graph.pos = 5,
  txt_gp    = fpTxtGp(
    label = gpar(fontfamily = "Times New Roman", fontface = "plain", fontsize = 9),
    xlab  = gpar(fontfamily = "Times New Roman", fontface = "bold", fontsize = 10),
    title = gpar(fontfamily = "Times New Roman", fontface = "bold", fontsize = 12)
  ),
  title     = "Subgroup Analysis: AI vs Control",
  new_page  = FALSE,
  colgap    = unit(8, "mm")
)

# Add figure footnote
footnote_text <- paste0(
  "Note: Educational attainment was dichotomized as bachelor’s degree or higher versus below bachelor’s degree/current undergraduate.\n",
  "Absolute differences are presented as AI minus Control in percentage points. Crude subgroup-specific proportions are shown\n",
  "for descriptive reference and may differ from the model-based marginal estimates."
)


grid.text(
  footnote_text,
  x = unit(0.02, "npc"),
  y = unit(0.015, "npc"),
  just = c("left", "bottom"),
  gp = gpar(fontfamily = "Times New Roman", fontsize = 7.5, col = "#555555")
)

dev.off()
