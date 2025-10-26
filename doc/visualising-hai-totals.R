## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
library(haiInsight)
data("hai_data_clean")
head(hai_data_clean)

## ----fig.alt="Bar chart showing total infections by infection type"-----------
plot_hai_totals(hai_data_clean, by = "Infection_Type")

## ----fig.alt="Bar chart showing total infections by age group"----------------
plot_hai_totals(hai_data_clean, by = "Age_Group")

## ----fig.alt="Bar chart showing total infections by sex"----------------------
plot_hai_totals(hai_data_clean, by = "Sex")

