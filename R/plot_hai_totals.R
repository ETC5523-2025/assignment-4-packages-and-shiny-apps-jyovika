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
#' @importFrom stats reorder
plot_hai_totals <- function(data, by = "Infection_Type", show_prop = FALSE, pal = NULL) {
  if (is.null(pal)) pal <- list(
    bar="#2C3E50", grid="#E6EEF0", panel="#FFFFFF", text="#1B2631", deep="#004D40"
  )

  sums <- summarise_hai(data, by)

  if (isTRUE(show_prop)) {
    total_all <- sum(sums$Total, na.rm = TRUE)
    sums$Value <- if (total_all > 0) sums$Total / total_all else 0
    y_lab <- "Proportion of infections"
  } else {
    sums$Value <- sums$Total
    y_lab <- "Total infections"
  }

  x_map <- ggplot2::aes(x = reorder(.data[[by]], .data$Value), y = .data$Value)

  if (by == "Age_Group") {
    # parse lower bound: "[70;74]" -> 70, "[85;Inf]" -> 85
    lo <- suppressWarnings(as.numeric(sub("\\[(\\d+);.*", "\\1", sums$Age_Group)))
    lvl <- sums$Age_Group[order(lo)]
    sums$Age_Group <- factor(sums$Age_Group, levels = lvl)
    x_map <- ggplot2::aes(x = .data$Age_Group, y = .data$Value)
  }

  g <- ggplot2::ggplot(sums, x_map) +
    ggplot2::geom_col(fill = pal$bar, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = pal$panel, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal$panel, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = pal$grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(colour = pal$text),
      axis.title       = ggplot2::element_text(colour = pal$deep)
    ) +
    ggplot2::labs(
      title = paste("HAI totals by", by),
      x = by, y = y_lab
    )

  if (isTRUE(show_prop)) {
    g <- g + scale_y_continuous(labels = scales::label_percent())
  }

  g
}


