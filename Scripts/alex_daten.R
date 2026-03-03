library(tidyverse)
df <- read_delim("C:/Users/PuetzP/Desktop/cvn_sarscov2_influenza_2324.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(date_of_result = as.Date(date_of_result, format = "%d.%m.%Y"))

df %>% 
  count(virus)

df <- df %>% 
  # SARS
  filter(virus_id == 22) %>% 
  select(id, patient_id, country, virus, date_of_result, result) #%>% 
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
weekly_pos <- df %>%
  filter(result == "Positive") %>%
  arrange(patient_id, date_of_result) %>%
  group_by(patient_id) %>%
  mutate(keep = keep_pos_by_window(date_of_result, 28L)) %>%
  ungroup() %>%
  filter(keep) %>%
  mutate(
    week_start = floor_date(date_of_result, unit = "week", week_start = 1) # Monday
  ) %>%
  count(week_start, name = "n_pos") %>%
  arrange(week_start)

# 3) plot
ggplot(weekly_pos, aes(x = week_start, y = n_pos)) +
  geom_point(size = 1) +
  geom_smooth(span = 0.1) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%V\n %G") +
  theme_classic() +
  labs(x = "Week", y = "Positive tests")


# share of positive test --------------------------------------------------
weekly_share_pos <- df %>%
  arrange(patient_id, date_of_result) %>%
  mutate(
    week_start = floor_date(date_of_result, unit = "week", week_start = 1) # Monday
  ) %>% 
  group_by(week_start) %>%
  summarise(n_tests = n(), share_pos = mean(result == "Positive"))

ggplot(weekly_share_pos, aes(x = week_start, y = share_pos)) +
  geom_point(size = 1) +
  geom_smooth(span = 0.1) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%V\n %G") +
  theme_classic() +
  labs(x = "Week", y = "Share of positive tests")

