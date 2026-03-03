# clean workspace
rm(list = ls())

# in case not installed, install pacman package
if (!require("pacman"))
  install.packages("pacman")
# install (if not done yet) and load required packages and read in self-written
# functions
pacman::p_load(here)
# load relevant functions, packages and labels defined in Script "load_packages_and_functions.R"
source(here("Scripts", "load_packages_and_functions.R"))

df <- readRDS(here(read_data_here, "data_ready.rds"))

pretty_name <- function(x) {
  dplyr::case_when(
    x == "viruslast_influenza_a_b" ~ "Viruslast im Abwasser Infl.",
    x == "meldedaten_inz_influenza" ~ "Meldedaten-Inz. Infl.",
    x == "sari_influenza" ~ "ICO-SARI SARI Infl.",
    x == "cvn_positives_influenza" ~ "CVN positive Test (Anzahl) Infl.",
    x == "cvn_proportion_positives_influenza" ~ "CVN positive Test (Anteil) Infl.",
    x == "viruslast_sars_co_v_2" ~ "Viruslast im Abwasser",
    x == "meldedaten_inz_sars" ~ "Meldedaten-Inz.",
    x == "sari_covid_19" ~ "ICO-SARI",
    x == "cvn_positives_coronavirus_sars_co_v_2" ~ "CVN positive Test (Anzahl)",
    x == "cvn_proportion_positives_coronavirus_sars_co_v_2" ~ "CVN positive Test (Anteil)",
    TRUE ~ x
  )
}

kw_label <- function(x) {
  iso <- ISOweek(x)          # "2023-W01"
  paste0(substr(iso, 1, 4), "-KW", substr(iso, 7, 8))
}

plot_std_group <- function(df, cols, title) {
  df %>%
    select(kalenderwoche, all_of(cols)) %>%
    pivot_longer(-kalenderwoche, names_to = "indikator", values_to = "wert") %>%
    group_by(indikator) %>%
    mutate(
      wert_std = as.numeric(scale(wert)),
      indikator_lbl = pretty_name(indikator)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = kalenderwoche, y = wert_std, color = indikator_lbl)) +
    geom_point(alpha = 0.85, size = 1.5, na.rm = TRUE) +
    geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1.3, na.rm = TRUE) +
    # scale_x_date(
    #   date_breaks = "8 weeks",   # change to "2 weeks" or "6 weeks" as needed
    #   labels = kw_label
    # ) +
    scale_x_date(
      date_breaks = "12 weeks",
      labels = function(x) {
        iso <- ISOweek::ISOweek(x)     # "YYYY-Www"
        year <- substr(iso, 1, 4)
        week <- substr(iso, 7, 8)
        paste0("KW", week, "\n", year) # week above year; year at every tick
      }
    ) +
    labs(
      title = title,
      x = "Kalenderwoche",
      y = "Standardisierter Wert",
      color = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5,size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.text.y = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15)   )
}

influenza_cols <- c(
  "viruslast_influenza_a_b",
  "meldedaten_inz_influenza",
  "sari_influenza",
  "cvn_positives_influenza",
  "cvn_proportion_positives_influenza"
)

sars_covid_cols <- c(
  "viruslast_sars_co_v_2",
  "meldedaten_inz_sars",
  "sari_covid_19",
  "cvn_positives_coronavirus_sars_co_v_2",
  "cvn_proportion_positives_coronavirus_sars_co_v_2"
)

p_influenza <- plot_std_group(df, influenza_cols, "Influenza: Indikatoren (standardisiert) über die Zeit")
p_sarscov2  <- plot_std_group(df, sars_covid_cols,  "SARS-CoV-2 / COVID-19: Indikatoren (standardisiert) über die Zeit")

p_influenza
p_sarscov2

ggsave(here(results_here,"influenza_comparison_plot.png"), plot = p_influenza, width = 10, height = 6, dpi = 300)
ggsave(here(results_here,"sars_comparison_plot.png"),  plot = p_sarscov2,  width = 10, height = 6, dpi = 300)
