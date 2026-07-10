# This script generates a bar plot of crude item-level accuracy by question type
# and study arm, with Wilson 95% confidence intervals.

# ==============================
# 0. Load required packages
# ==============================
library(readxl)
library(dplyr)
library(binom)
library(ggplot2)
library(Cairo)
font_family <- "Arial"
pt_to_geom <- function(pt) pt / 2.845276
# ==============================
# 1. Read source datasets
# ==============================
data_ai  <- read_excel("data/LungDiag.xlsx")
data_ctl <- read_excel("data/Control.xlsx")

# ==============================
# 2. Define item column indices
# ==============================
all_cols   <- 20:151
type1_cols <- 20:63
type2_cols <- seq(64, 150, 2)
type3_cols <- seq(65, 151, 2)

# ==============================
# 3. Function: crude accuracy and Wilson 95% CI
# ==============================
get_prop_ci <- function(dat, cols) {
  correct <- sum(dat[, cols] == "对", na.rm = TRUE)
  total   <- sum(!is.na(dat[, cols]))
  prop    <- correct / total
  ci      <- binom.confint(correct, total, method = "wilson")[, c("lower", "upper")]
  
  tibble(
    Accuracy  = prop,
    CI_lower  = ci$lower,
    CI_upper  = ci$upper,
    n_correct = correct,
    n_total   = total
  )
}

# ==============================
# 4. Calculate results for all domains and both groups
# ==============================
calc_set <- function(dat) {
  bind_rows(
    Overall           = get_prop_ci(dat, all_cols),
    `Risk Behaviour`  = get_prop_ci(dat, type1_cols),
    Diagnostic        = get_prop_ci(dat, type2_cols),
    Triage            = get_prop_ci(dat, type3_cols),
    .id = "Type"
  )
}

ai_tbl  <- calc_set(data_ai)  %>% mutate(Group = "LungDiag")
ctl_tbl <- calc_set(data_ctl) %>% mutate(Group = "Control")

plot_df <- bind_rows(ai_tbl, ctl_tbl) %>%
  mutate(
    Group = factor(Group, levels = c("LungDiag", "Control")),
    Type  = factor(Type, levels = c("Overall", "Risk Behaviour", "Diagnostic", "Triage"))
  )

# ==============================
# 5. Generate the plot
# ==============================
p <- ggplot(plot_df, aes(x = Type, y = Accuracy, fill = Group)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(0.7),
    width = 0.5,
    colour = "black"
  ) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    position = position_dodge(0.7),
    width = 0.2,
    colour = "black"
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.1f%%", Accuracy * 100),
        "\n(",
        n_correct,
        "/",
        n_total,
        ")"
      )
    ),
    position = position_dodge(0.7),
    vjust = -0.8,
    size = pt_to_geom(6)
  ) +
  scale_fill_manual(
    values = c(
      "LungDiag"="#1f77b4",
      "Control"="darkorange"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0)
  ) +
  labs(
    x = "Question Type",
    y = "Accuracy (%)",
    caption = paste(
      "Bars show crude pooled item-level accuracy proportions.",
      "Error bars indicate Wilson 95% confidence intervals.",
      "Numerators and denominators are shown above bars.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_family = font_family, base_size = 6) +
  theme(
    text = element_text(family = font_family),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.caption = element_text(
      size = 5.5,
      hjust = 0,
      lineheight = 1.1
    ),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5)
  )

# ==============================
# 6. Save as PDF with embedded fonts
# ==============================
ggsave(
  "Figure3.pdf",
  plot = p,
  width = 8,
  height = 6.5,
  units = "in",
  device = cairo_pdf,
  family = font_family
)