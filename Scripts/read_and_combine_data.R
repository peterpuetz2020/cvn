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

# ----------------------------
# Data: AMELAG aggregated curve (weekly virus load)
# ----------------------------
amelag <- "https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/main/amelag_aggregierte_kurve.tsv" %>%
  readr::read_tsv(show_col_types = FALSE) %>%        # read TSV from GitHub
  janitor::clean_names() %>%                         # standardize column names to snake_case
  dplyr::filter(typ == "Influenza A+B" | typ == "SARS-CoV-2") %>%         # keep only Influenza-related rows
  dplyr::mutate(
    # convert daily date to ISO week start (Mon) for consistent weekly aggregation
    kalenderwoche = lubridate::floor_date(datum, "week", week_start = 1)
  ) %>%
  dplyr::select(kalenderwoche, typ, viruslast) %>%   # keep only needed columns
  tidyr::pivot_wider(                              # columns = 'typ' values; values = virus load
    names_from = "typ",
    values_from = viruslast
  ) %>%
  janitor::clean_names(parsing_option = 3) %>%   
  # clean new column names created by pivot
  dplyr::rename_at(                                # prefix influenza-related cols with 'viruslast_'
    dplyr::vars(c(dplyr::contains("influenza"), dplyr::contains("sars"))),
    ~ paste0("viruslast_", .)
  )

sari_hosp <- "https://raw.githubusercontent.com/robert-koch-institut/SARI-Hospitalisierungsinzidenz/main/SARI-Hospitalisierungsinzidenz.tsv" %>% 
  readr::read_tsv(show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(sari == "Influenza" | sari == "COVID-19", altersgruppe == "00+") %>%    # Influenza, all ages
  dplyr::mutate(
    # convert ISO week label (YYYY-Www) to a date by appending "-1" (Monday)
    kalenderwoche = paste0(kalenderwoche, "-1"),
    kalenderwoche = ISOweek::ISOweek2date(kalenderwoche)
  ) %>% 
  dplyr::select(kalenderwoche, sari, sari_hosp = hospitalisierungsinzidenz) %>% 
  tidyr::pivot_wider(                              # columns = 'typ' values; values = virus load
    names_from = "sari",
    values_from = sari_hosp 
  ) %>% 
  janitor::clean_names(parsing_option = 3) %>%   
  # clean new column names created by pivot
  dplyr::rename_at(                                # prefix influenza-related cols with 'viruslast_'
    dplyr::vars(c(dplyr::contains("influenza"), dplyr::contains("covid"))),
    ~ paste0("sari_", .)
  )

# # ----------------------------
# # Data: GrippeWeb ILI incidence (nationwide, weekly)
# # ----------------------------
# 
# gw <- "https://raw.githubusercontent.com/robert-koch-institut/GrippeWeb_Daten_des_Wochenberichts/main/GrippeWeb_Daten_des_Wochenberichts.tsv" %>% 
#   readr::read_tsv(show_col_types = FALSE) %>% 
#   janitor::clean_names() %>% 
#   dplyr::filter(
#     erkrankung == "ILI",
#     altersgruppe == "00+",
#     region == "Bundesweit"
#   ) %>% 
#   dplyr::mutate(
#     kalenderwoche = paste0(kalenderwoche, "-1"),
#     kalenderwoche = ISOweek::ISOweek2date(kalenderwoche)
#   ) %>% 
#   dplyr::select(kalenderwoche, gw_inz = inzidenz)
# 
# inz <- "https://raw.githubusercontent.com/robert-koch-institut/GrippeWeb_Daten_des_Wochenberichts/main/GrippeWeb_Daten_des_Wochenberichts.tsv" %>% 
#   readr::read_tsv(show_col_types = FALSE) %>% 
#   janitor::clean_names() %>% 
#   dplyr::filter(
#     erkrankung == "ILI",
#     altersgruppe == "00+",
#     region == "Bundesweit"
#   ) %>% 
#   dplyr::mutate(
#     kalenderwoche = paste0(kalenderwoche, "-1"),
#     kalenderwoche = ISOweek::ISOweek2date(kalenderwoche)
#   ) %>% 
#   dplyr::select(kalenderwoche, gw_inz = inzidenz)
meldedaten_infl <- "https://raw.githubusercontent.com//robert-koch-institut/Influenzafaelle_in_Deutschland/main/IfSG_Influenzafaelle.tsv" %>%
 readr::read_tsv(show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::filter(altersgruppe == "00+", region == "Deutschland") %>%
  dplyr::mutate(
    kalenderwoche = paste0(meldewoche, "-1"),
    kalenderwoche = ISOweek::ISOweek2date(kalenderwoche)
  ) %>%
  # rename incidence to a distinct column name
  dplyr::select(kalenderwoche, meldedaten_inz_influenza = inzidenz)  # NOTE: column name kept as in original (see notes)


meldedaten_sars <- "https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/refs/heads/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv" %>%
  readr::read_csv(show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::filter(altersgruppe == "00+") %>%
  mutate(
    kalenderwoche = floor_date(meldedatum, unit = "week", week_start = 1)
  ) %>% 
  group_by(kalenderwoche) %>%
  slice_max(meldedatum, n = 1) %>%   # take last day of week
  ungroup() %>%  # keep altersgruppe if needed
  # rename incidence to a distinct column name
  dplyr::select(kalenderwoche, meldedaten_inz_sars = inzidenz_7_tage)  

# ----------------------------
# Merge datasets by calendar week
# ----------------------------
df_cvn <- read_delim(here(read_data_here, "cvn_sarscov2_influenza_2324.csv"), 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(date_of_result = as.Date(date_of_result, format = "%d.%m.%Y"))

df_cvn %>% 
  count(virus)

# tests per week
df_cvn %>% 
  mutate(
    kalenderwoche = floor_date(date_of_result, unit = "week", week_start = 1) # Monday
  )%>%
  group_by(virus) %>% 
  count(kalenderwoche, name = "n_tests") %>%
  ungroup() %>% 
    group_by(virus) %>%
    summarise(
    n = sum(!is.na(n_tests)),
    Min = min(n_tests, na.rm = TRUE),
    Mean = mean(n_tests, na.rm = TRUE),
    Median = median(n_tests, na.rm = TRUE),
    Max = max(n_tests, na.rm = TRUE),
    SD = sd(n_tests, na.rm = TRUE)
    )
  
df_cvn <- df_cvn %>% 
  mutate(virus = ifelse(virus == "Influenza A (non-differentiated)" | virus == "Influenza B", "Influenza", virus)) %>% 
  filter(virus == "Coronavirus (SARS-CoV-2)" | virus == "Influenza") %>% 
  dplyr::select(virus, patient_id, country, virus, date_of_result, result)  %>% 
  filter(date_of_result >= starting_date,
         date_of_result <= ending_date)

#%>% 
#distinct(patient_id, .keep_all = T)


# cases -------------------------------------------------------------------
# drop repeated cases and negatives
# 1) helper: keep only first positive in each 28-day window (per patient)
keep_pos_by_window <- function(dates, window_days = 28L) {
  keep <- logical(length(dates))
  last_kept <- as.Date(NA)
  
  for (i in seq_along(dates)) {
    if (is.na(last_kept) || dates[i] > last_kept + window_days) {
      keep[i] <- TRUE
      last_kept <- dates[i]
    }
  }
  keep
}

# 2) keep only "valid" positives (deduped) and count per week
weekly_pos <- df_cvn %>%
  filter(result == "Positive") %>%
  arrange(virus, patient_id, date_of_result) %>%
  group_by(virus, patient_id) %>%
  mutate(keep = keep_pos_by_window(date_of_result, 28L)) %>%
  ungroup() %>%
  filter(keep) %>%
  mutate(
    kalenderwoche = floor_date(date_of_result, unit = "week", week_start = 1) # Monday
  ) %>%
  group_by(virus) %>% 
  count(kalenderwoche, name = "n_pos") %>%
  ungroup() %>% 
  arrange(virus, kalenderwoche)

cvn_weekly_pos <- weekly_pos %>% 
  tidyr::pivot_wider(                              # columns = 'typ' values; values = virus load
    names_from = "virus",
    values_from = n_pos 
  ) %>% 
  janitor::clean_names(parsing_option = 3) %>%   
  # clean new column names created by pivot
  dplyr::rename_at(                                # prefix influenza-related cols with 'viruslast_'
    dplyr::vars(c(dplyr::contains("influenza"), dplyr::contains("corona"))),
    ~ paste0("cvn_positives_", .)
  ) 

# 3) plot
number_plot <- ggplot(weekly_pos, aes(x = kalenderwoche, y = n_pos)) +
  geom_point(size = 1) +
  geom_smooth(span = 0.1) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%V\n %G") +
  theme_classic() +
  labs(x = "Kalenderwoche", y = "Positive Tests (Anzahl, Duplikate entfernt)") +
  facet_wrap(~virus, scales = "free_y")
#ggsave(here(results_here,"cvn_number_of_positive_tests.png"), width = 10, height = 6, dpi = 300)


# share of positive test --------------------------------------------------
weekly_share_pos <- df_cvn %>%
  arrange(virus, patient_id, date_of_result) %>%
  mutate(
    kalenderwoche = floor_date(date_of_result, unit = "week", week_start = 1) # Monday
  ) %>% 
  #group_by(virus, week_start) %>%
  summarise(.by = c(virus, kalenderwoche), n_tests = n(), share_pos = mean(result == "Positive")) %>% 
  ungroup()

cvn_weekly_share_pos <- weekly_share_pos %>% 
  dplyr::select(-n_tests) %>% 
  tidyr::pivot_wider(                              # columns = 'typ' values; values = virus load
    names_from = "virus",
    values_from = share_pos 
  ) %>% 
  janitor::clean_names(parsing_option = 3) %>%   
  # clean new column names created by pivot
  dplyr::rename_at(                                # prefix influenza-related cols with 'viruslast_'
    dplyr::vars(c(dplyr::contains("influenza"), dplyr::contains("corona"))),
    ~ paste0("cvn_proportion_positives_", .)
  ) 

# 3) plot
propotion_plot <- ggplot(weekly_share_pos, aes(x = kalenderwoche, y = share_pos)) +
  geom_point(size = 1) +
  geom_smooth(span = 0.1) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%V\n %G") +
  theme_classic() +
  labs(x = "Kalenderwoche", y = "Positive Tests (Anteil)") +
  facet_wrap(~virus, scales = "free_y")
#ggsave(here(results_here,"cvn_proportion_of_positive_tests.png"), width = 10, height = 6, dpi = 300)

combined_plot <- number_plot / propotion_plot
print(combined_plot)
ggsave(
  here(results_here,"cvn_plots_infl_sars.png"),
  plot = combined_plot,
  width = 12,
  height = 12,
  dpi = 300
)

df <- amelag %>% 
  dplyr::left_join(meldedaten_sars,  by = "kalenderwoche") %>% 
  dplyr::left_join(meldedaten_infl,  by = "kalenderwoche") %>% 
  dplyr::left_join(sari_hosp,   by = "kalenderwoche") %>% 
  dplyr::left_join(cvn_weekly_pos,   by = "kalenderwoche") %>% 
  dplyr::left_join(cvn_weekly_share_pos,   by = "kalenderwoche") %>% 
  filter(kalenderwoche >= starting_date,
         kalenderwoche <= ending_date) %>% 
  arrange(kalenderwoche)


# ----------------------------
# Save result as RDS
# ----------------------------

# 'read_data_here' should be a character scalar path (e.g., "Out")
# produced by your loader script; ensure it exists before saving.
saveRDS(df, here(read_data_here, "data_ready.rds"))


