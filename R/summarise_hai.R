#' Summarise HAI cases
#'
#' Totals HAI counts by a chosen variable (e.g. infection type, age group, or sex).
#'
#' @param data A data frame like `hai_data_clean`
#'   (must include columns: Infection_Type, Age_Group, Sex, Count)
#' @param by The column name to group by:
#'   one of `"Infection_Type"`, `"Age_Group"`, or `"Sex"`.
#' @param as_kable Logical. If TRUE, returns a formatted kable table
#'   (requires the knitr and kableExtra packages).
#' @param caption Character. Caption for the table when `as_kable = TRUE`.
#'
#' @return A tibble with columns `{by}` and `Total`.
#'
#' @examples
#' data("num_hai_patients_by_stratum", package = "BHAI")
#' # summarise_hai(hai_data_clean, by = "Infection_Type")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise arrange desc
summarise_hai <- function(
    data,
    by = "Infection_Type",
    as_kable = FALSE,
    caption = "Summary of HAI cases by selected grouping variable."
) {
  # --- validation ---
  if (!by %in% names(data)) {
    stop("`by` must be one of: Infection_Type, Age_Group, or Sex.")
  }

  # --- summarise totals ---
  tbl <- dplyr::group_by(data, .data[[by]]) |>
    dplyr::summarise(Total = sum(.data$Count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$Total))

  names(tbl)[1] <- by

  if (!as_kable) return(tbl)

  # --- optional kable formatting ---
  if (!requireNamespace("knitr", quietly = TRUE) ||
      !requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Please install.packages(c('knitr','kableExtra')) to use as_kable = TRUE.")
  }

  align_vec <- rep("l", ncol(tbl))

  knitr::kable(
    tbl,
    caption = caption,
    align   = align_vec) |>

    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position   = "center") |>

    kableExtra::row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#598392") |>
    kableExtra::column_spec(1, bold = TRUE, color = "#004D40")
}
