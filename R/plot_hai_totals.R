#' Plot total HAI counts
#'
#' Creates a bar chart of total infections by a chosen variable.
#'
#' @param data Data frame (like `hai_data_clean`)
#' @param by Grouping variable ("Infection_Type", "Age_Group", or "Sex")
#' @export
#' @importFrom rlang .data
plot_hai_totals <- function(data, by = "Infection_Type") {
  sums <- summarise_hai(data, by)
  ggplot2::ggplot(sums, ggplot2::aes(x = reorder(.data[[by]], Total), y = Total)) +
    ggplot2::geom_col(fill = "#2C3E50") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = paste("HAI totals by", by),
      x = by, y = "Total infections"
    )
}
