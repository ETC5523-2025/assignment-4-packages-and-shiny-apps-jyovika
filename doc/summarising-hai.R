## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
library(haiInsight)
data("hai_data_clean")
head(hai_data_clean)

## -----------------------------------------------------------------------------
summarise_hai(hai_data_clean, by = "Infection_Type", as_kable = TRUE)

## -----------------------------------------------------------------------------
summarise_hai(hai_data_clean)

## -----------------------------------------------------------------------------
summarise_hai(hai_data_clean, by = "Sex", as_kable = TRUE)

