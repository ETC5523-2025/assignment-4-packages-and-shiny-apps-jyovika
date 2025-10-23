#' Summarise HAI cases
#'
#' Totals HAI counts by a chosen variable.
#'
#' @param data A data frame like `hai_data_clean` (with columns: Infection_Type, Age_Group, Sex, Count)
#' @param by One of "Infection_Type", "Age_Group", or "Sex".
#'
#' @return A tibble with columns: `{by}`, `Total`
#' @examples
#' # data("hai_data_clean", package = "haiInsight")
#' # summarise_hai(hai_data_clean, by = "Infection_Type")
#' @export
#' @importFrom dplyr group_by summarise arrange desc
summarise_hai <- function(data, by = "Infection_Type") {
  if (!by %in% names(data)) stop("`by` must be one of: Infection_Type, Age_Group, Sex.")
  dplyr::group_by(data, .data[[by]]) |>
    dplyr::summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Total))
}
