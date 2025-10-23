#' Cleaned Healthcare-Associated Infection Data (Germany, 2011)
#'
#' A tidy dataset showing infection counts by infection type, age group, and sex
#' from the ECDC Point Prevalence Survey 2011 for Germany.
#'
#' @format A data frame with 190 rows and 4 variables:
#' \describe{
#'   \item{Age_Group}{Age band of patients, e.g., `[1;4]`, `[25;34]`.}
#'   \item{Sex}{Patient sex ("F" or "M").}
#'   \item{Count}{Number of infection cases in that stratum.}
#'   \item{Infection_Type}{HAP, UTI, SSI, BSI, CDI.}
#' }
#' @source Data derived from the BHAI package: Zacher et al. (2019),
#' \emph{Application of a new methodology and R package reveals a high burden of
#' healthcare-associated infections in Germany compared to the EU/EEA, 2011–2012}.
"hai_data_clean"

#' Headline HAI Estimates (Germany, 2011)
#'
#' This dataset was **manually compiled** by the package author based on values
#' reported in Zacher et al. (2019):
#' *Application of a new methodology and R package reveals a high burden of
#' healthcare-associated infections in Germany compared to the EU/EEA, 2011-2012.*
#'
#' It is not derived from raw data. Instead, it summarises key published indicators
#' (cases, deaths, DALYs) for communication and visualisation purposes within the app.
#'
#' @format A tibble with 5 rows and 4 variables:
#' \describe{
#'   \item{metric}{Indicator name (e.g. "Cases (Germany, 2011)").}
#'   \item{value}{Numeric estimate reported in the paper.}
#'   \item{unit}{Measurement unit (e.g. "count", "per100k").}
#'   \item{source}{Citation of the original study.}
#' }
#' @source Zacher et al. (2019), *Euro Surveill.* 24(46):1900135.
"hai_headlines"

