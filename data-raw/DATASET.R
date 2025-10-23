## code to prepare `DATASET` dataset goes here

library(BHAI)
library(dplyr)
library(tidyr)
library(purrr)
data("german_pps_2011_repr", package = "BHAI")

hai_list <- num_hai_patients_by_stratum

hai_data_clean <- map_dfr(names(hai_list), function(inf_type) {
  df <- as.data.frame(hai_list[[inf_type]])
  df <- df %>%
    rownames_to_column(var = "Age_Group") %>%
    pivot_longer(cols = -Age_Group,
                 names_to = "Sex",
                 values_to = "Count") %>%
    mutate(Infection_Type = inf_type)
  df})

hai_headlines <- tibble::tribble(
  ~metric, ~value, ~unit, ~source,
  "Cases (Germany, 2011)", 478000, "count", "Zacher et al. 2019",
  "Deaths (Germany, 2011)", 16000, "count", "Zacher et al. 2019",
  "DALYs (Germany, 2011)", 249000, "count", "Zacher et al. 2019",
  "DALYs per 100k (DE)", 308, "per100k", "Zacher et al. 2019",
  "DALYs per 100k (EU/EEA)", 290, "per100k", "Zacher et al. 2019")

usethis::use_data(hai_data_clean, hai_headlines, overwrite = TRUE)
