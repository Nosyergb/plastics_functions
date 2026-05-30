#' Summarize changes in plastic type composition from 2019 to 2020
#'
#' @param data Plastics dataset.
#'
#' @return A tibble.
#' @export
summarize_plastics <- function(data) {

  plastic_types <- c("hdpe", "ldpe", "pet", "pp", "ps", "pvc", "o")

  Prop_change <- function(type) {
    data |>
      filter(parent_company != "Grand Total") |>
      group_by(year) |>
      summarize(
        total_type = sum(across(all_of(type)), na.rm = TRUE),
        total_all = sum(grand_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(prop = total_type / total_all) |>
      arrange(year) |>
      summarize(
        plastic_type = type,
        prop_2019 = prop[year == 2019],
        prop_2020 = prop[year == 2020],
        change = prop_2020 - prop_2019
      )
  }

  map_dfr(plastic_types, Prop_change) |>
    arrange(change)
}
