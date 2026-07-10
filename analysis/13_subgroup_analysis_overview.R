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
        filter(Group == exp_lab) |>
        mutate(group1 = sprintf("%d/%d (%.1f)", correct, total, correct / total * 100)),
      by = c("label" = subgroup_var)
    ) |>
    left_join(
      summary_table |>
        filter(Group == ctrl_lab) |>
        mutate(group2 = sprintf("%d/%d (%.1f)", correct, total, correct / total * 100)),
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
exp_data  <- read_excel("data/LungDiag.xlsx")      |> mutate(ID = paste0("Exp_", ID), Group = "Experimental")
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
library(car) # Required for Anova()

# Helper function to extract the interaction P value
get_p_int <- function(model, term) {
  Anova(model, type = "III")[term, "Pr(>Chisq)"]
}

# Extract interaction P values for each subgroup
p_int_df <- tibble(
  group = c("Sex", "Age Group", "Education", "Region"),
  p_interaction = c(
    get_p_int(model_sex, "Sex:Group"),
    get_p_int(model_age, "AgeGroup:Group"),
    get_p_int(model_edu, "EduGroup2:Group"),
    get_p_int(model_region, "Territory:Group")
  )
)

# Prepare combined data and row labels
# ==========================================================
# 5. Combine results and create a Nature/JAMA-style forest plot
# ==========================================================

library(car)
library(forestplot)
library(grid)
library(Cairo)
library(tidyverse)

# ---------- Helper function to extract the interaction P value ----------
get_p_int <- function(model, term) {
  as.numeric(Anova(model, type = "III")[term, "Pr(>Chisq)"])
}

# ---------- Helper function to format interaction P values ----------
format_p <- function(p) {
  if (is.na(p)) return("")
  
  out <- case_when(
    p < 0.001 ~ "<.001",
    p > 0.99  ~ ">0.99",
    TRUE      ~ sprintf("%.2f", p)
  )
  
  return(out)
}

# ---------- Extract interaction P values for each subgroup ----------
p_int_df <- tibble(
  group = c("Sex", "Age Group", "Education", "Region"),
  p_interaction = c(
    get_p_int(model_sex, "Sex:Group"),
    get_p_int(model_age, "AgeGroup:Group"),
    get_p_int(model_edu, "EduGroup2:Group"),
    get_p_int(model_region, "Territory:Group")
  )
)

# ---------- Combine subgroup results ----------
group_order <- c("Sex", "Age Group", "Education", "Region")

group_display <- c(
  "Sex"       = "Sex",
  "Age Group" = "Age, y",
  "Education" = "Education",
  "Region"    = "Region"
)

plot_df_all <- bind_rows(
  plot_df_sex,
  plot_df_age,
  plot_df_edu,
  plot_df_region
) |>
  mutate(
    group = factor(group, levels = group_order),
    label = as.character(label),
    
    # Shorten long row labels for figure readability
    label = dplyr::recode(
      label,
      "Without bachelor's degree / current undergraduate" =
        "No bachelor's degree/current undergraduate"
    ),
    
    group1 = replace_na(group1, ""),
    group2 = replace_na(group2, "")
  )

# ---------- Place the interaction P value in the middle row of each subgroup block ----------
# This mimics reference-style figures by centering the interaction P value
# within each subgroup block instead of printing it on every row.
plot_df_final <- map_dfr(group_order, function(g) {
  
  d <- plot_df_all |>
    filter(as.character(group) == g) |>
    mutate(
      row_type  = "body",
      row_label = paste0("   ", label),
      p_text    = ""
    )
  
  p_val <- p_int_df |>
    filter(group == g) |>
    pull(p_interaction)
  
  if (nrow(d) > 0 && length(p_val) == 1 && !is.na(p_val)) {
    d$p_text[ceiling(nrow(d) / 2)] <- format_p(p_val)
  }
  
  header <- tibble(
    group     = g,
    row_type  = "header",
    row_label = unname(group_display[g]),
    group1    = "",
    group2    = "",
    mean      = NA_real_,
    lower     = NA_real_,
    upper     = NA_real_,
    p_text    = ""
  )
  
  bind_rows(
    header,
    d |>
      transmute(
        group = as.character(group),
        row_type,
        row_label,
        group1,
        group2,
        mean,
        lower,
        upper,
        p_text
      )
  )
})

# ---------- Text for model-based differences ----------
diff_text <- ifelse(
  is.na(plot_df_final$mean),
  "",
  sprintf(
    "%.1f (%.1f to %.1f)",
    plot_df_final$mean  * 100,
    plot_df_final$lower * 100,
    plot_df_final$upper * 100
  )
)

# ---------- Build the table text matrix ----------
# Split the table header into two rows to avoid overlap with horizontal rules.
tabletext <- rbind(
  c(
    "Subgroup",
    "No. correct/No. total (%)",
    "",
    "Difference",
    "Interaction"
  ),
  c(
    "",
    "LungDiag",
    "Control",
    "(95% CI)",
    "P value"
  ),
  cbind(
    plot_df_final$row_label,
    plot_df_final$group1,
    plot_df_final$group2,
    diff_text,
    plot_df_final$p_text
  )
)

# ---------- Vectors for forest plot ----------
plot_mean  <- c(NA_real_, NA_real_, plot_df_final$mean  * 100)
plot_lower <- c(NA_real_, NA_real_, plot_df_final$lower * 100)
plot_upper <- c(NA_real_, NA_real_, plot_df_final$upper * 100)

is_summary <- c(
  TRUE,
  TRUE,
  plot_df_final$row_type == "header"
)

# ---------- Horizontal rules ----------
# Keep horizontal rules above the first row, below the header, and at the bottom.
n_rows <- nrow(tabletext)

hrzl <- lapply(
  seq_len(n_rows + 1),
  function(i) gpar(col = "#D0D0D0", lwd = 0.35)
)
names(hrzl) <- as.character(seq_len(n_rows + 1))

hrzl[["1"]] <- gpar(col = "black", lwd = 0.6)
hrzl[["3"]] <- gpar(col = "black", lwd = 0.6) # Place the rule below the second header row.
hrzl[[as.character(n_rows + 1)]] <- gpar(col = "black", lwd = 0.6)

# ---------- Output ----------
font_family <- "Arial"

# ==========================================================
# Layout parameters
# ==========================================================
# Reduce vertical whitespace by setting the figure height to 6.0.
fig_width  <- 11.5
fig_height <- 6.0

# Top annotation area parameters
top_rule_y   <- 0.875
favor_y      <- 0.850
favor_line_y <- c(0.833, 0.868)

# Main plot area parameters
plot_x      <- 0.50
plot_y      <- 0.505
plot_width  <- 0.96
plot_height <- 0.82

# Footnote area parameters
foot_rule_y <- 0.115
footnote_y  <- 0.065

CairoPDF(
  "Figure4.pdf",
  width  = fig_width,
  height = fig_height,
  family = font_family
)

grid.newpage()

# Main plot viewport
pushViewport(
  viewport(
    x      = plot_x,
    y      = plot_y,
    width  = plot_width,
    height = plot_height
  )
)

forestplot(
  labeltext = tabletext,
  mean      = plot_mean,
  lower     = plot_lower,
  upper     = plot_upper,
  
  is.summary = is_summary,
  
  zero   = 0,
  clip   = c(-10, 30),
  xticks = c(-10, 0, 10, 20, 30),
  
  graph.pos   = 5,
  graphwidth  = unit(60, "mm"),
  
  boxsize     = 0.15,
  lwd.ci      = 1.0,
  lwd.zero    = 0,
  lwd.xaxis   = 0.8,
  ci.vertices = FALSE,
  
  grid = structure(
    0,
    gp = gpar(col = "#9E9E9E", lty = 2, lwd = 0.8)
  ),
  
  col = fpColors(
    box     = "#2E5E68",
    line    = "#2F2F2F",
    summary = "#2E5E68",
    zero    = "#9E9E9E"
  ),
  
  align = c("l", "c", "c", "c", "c"),
  
  txt_gp = fpTxtGp(
    label = gpar(
      fontfamily = font_family,
      fontsize   = 6.0
    ),
    summary = gpar(
      fontfamily = font_family,
      fontface   = "bold",
      fontsize   = 5.8
    ),
    ticks = gpar(
      fontfamily = font_family,
      fontsize   = 9.0
    ),
    xlab = gpar(
      fontfamily = font_family,
      fontsize   = 8.0
    )
  ),
  
  xlab = "Absolute difference, LungDiag - Control, percentage points",
  
  colgap     = unit(6.0, "mm"),
  lineheight = unit(0.45, "cm"),
  hrzl_lines = hrzl,
  
  mar = unit(c(0, 0, 0, 0), "mm"),
  new_page = FALSE
)

popViewport()

# ==========================================================
# Footnote
# ==========================================================
footnote_text <- paste0(
  "Note: Differences are model-based marginal estimates and are shown as LungDiag minus Control in percentage points. ",
  "Crude subgroup-specific proportions are shown for descriptive reference."
)

# Draw the separator line above the footnote.
grid.lines(
  x = unit(c(0.02, 0.98), "npc"),
  y = unit(foot_rule_y, "npc"),
  gp = gpar(col = "black", lwd = 0.6)
)

grid.text(
  footnote_text,
  x = unit(0.02, "npc"),
  y = unit(footnote_y, "npc"),
  just = c("left", "bottom"),
  gp = gpar(
    fontfamily = font_family,
    fontsize   = 5.5,
    col        = "#333333"
  )
)

dev.off()