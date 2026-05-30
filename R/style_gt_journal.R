#' Style gt table with journal dashboard formatting
#'
#' @param table A gt table.
#' @param title Table title.
#' @param subtitle Table subtitle.
#' @param percent Columns to format as percentages.
#'
#' @return A styled gt table.
#' @export
style_gt_journal <- function(table, title, subtitle, percent) {

  table |>
    gt::tab_header(
      title = gt::md(title),
      subtitle = subtitle
    ) |>
    gt::fmt_percent(
      columns = percent,
      decimals = 2
    ) |>
    gt::cols_label(
      plastic_type = "Type",
      prop_2019 = "2019",
      prop_2020 = "2020",
      change = "Change"
    ) |>
    gt::tab_options(
      heading.background.color = "#EB6864",
      column_labels.background.color = "lightgrey",
      row.striping.background_color = "lightgrey",
      row.striping.include_table_body = TRUE
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "#333333",
        weight = gt::px(2)
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_title(groups = "subtitle")
      )
    ) |>
    gt::tab_options(
      data_row.padding = gt::px(18),
      heading.padding = gt::px(10)
    ) |>
    gt::data_color(
      columns = change,
      colors = scales::col_numeric(
        palette = c("lightgreen", "white", "#D95C5C"),
        domain = NULL
      )
    )
}
