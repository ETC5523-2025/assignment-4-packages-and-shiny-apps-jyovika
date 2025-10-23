#' Plot total HAI counts
#'
#' Creates a bar chart of total infections by a chosen variable.
#'
#' @param data Data frame like `hai_data_clean`
#' @param by One of "Infection_Type", "Age_Group", or "Sex"
#'
#' @examples
#' # data("hai_data_clean", package = "haiInsight")
#' # plot_hai_totals(hai_data_clean, by = "Infection_Type")
#' # plot_hai_totals(hai_data_clean, by = "Age_Group")
#' # plot_hai_totals(hai_data_clean, by = "Sex")
#'
#' @export
#' @importFrom rlang .data
plot_hai_totals <- function(data, by = "Infection_Type") {
  sums <- summarise_hai(data, by)

  # default: order categories by totals so biggest ends up near the top with coord_flip()
  x_map <- ggplot2::aes(x = reorder(.data[[by]], Total), y = Total)

  if (by == "Age_Group") {
    # parse lower bound: "[70;74]" -> 70, "[85;Inf]" -> 85
    lo <- suppressWarnings(as.numeric(sub("\\[(\\d+);.*", "\\1", sums$Age_Group)))
    lvl <- sums$Age_Group[order(lo)]
    sums$Age_Group <- factor(sums$Age_Group, levels = lvl)
    x_map <- ggplot2::aes(x = Age_Group, y = Total)
  }

  ggplot2::ggplot(sums, x_map) +
    ggplot2::geom_col(fill = "#2C3E50") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = paste("HAI totals by", by),
      x = by, y = "Total infections")
}

