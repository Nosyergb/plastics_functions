#' Plot plastic waste by country before and during COVID
#'
#' @param data Plastics dataset.
#'
#' @return A ggplot.
#' @export
plot_country_dist <- function(data) {

  country_change <- data |>
    dplyr::filter(
      parent_company != "Grand Total",
      country != "EMPTY"
    ) |>
    dplyr::group_by(country, year) |>
    dplyr::summarize(
      total_plastic = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = total_plastic
    ) |>
    dplyr::filter(!is.na(`2019`), !is.na(`2020`))

  outliers <- country_change |>
    dplyr::slice_max(`2020`, n = 2)

  country_change |>
    ggplot2::ggplot(ggplot2::aes(x = `2019`, y = `2020`)) +
    ggplot2::geom_point(color = "#D95C5C", alpha = 0.7, size = 3) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    ggrepel::geom_text_repel(
      data = outliers,
      ggplot2::aes(label = country),
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = "Plastic Waste by Country Before and During COVID",
      subtitle = "Countries above the dashed line reported more plastic waste in 2020 than in 2019",
      x = "Total Plastic Waste in 2019",
      y = "Total Plastic Waste in 2020"
    ) +
    ggplot2::theme_light()
}

