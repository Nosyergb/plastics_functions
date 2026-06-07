#' Wrangle plastic type totals by year
#'
#' Converts the plastics data from wide to long format using data.table and
#' summarizes the total amount and proportion of each plastic type by year.
#' Grand Total rows are removed before summarizing.
#'
#' @param data Plastics dataset.
#'
#' @return A data.table with year, plastic_type, total, and proportion columns.
#' @importFrom data.table as.data.table melt
#' @export
wrangle_plastic_types <- function(data = load_data()) {

  required_cols <- c(
    "parent_company",
    "year",
    "hdpe",
    "ldpe",
    "o",
    "pet",
    "pp",
    "ps",
    "pvc"
  )

  if (!all(required_cols %in% names(data))) {
    stop("data is missing required columns.")
  }

  plastic_types <- c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  dt <- data.table::as.data.table(
    clean_plastics_data(
      data = data,
      remove_empty_country = FALSE
    )
  )

  long_dt <- data.table::melt(
    dt,
    id.vars = "year",
    measure.vars = plastic_types,
    variable.name = "plastic_type",
    value.name = "plastic_amount"
  )

  summary_dt <- long_dt[
    ,
    list(
      total = sum(plastic_amount, na.rm = TRUE)
    ),
    by = list(year, plastic_type)
  ]

  year_totals <- summary_dt[
    ,
    list(year_total = sum(total, na.rm = TRUE)),
    by = year
  ]

  summary_dt <- merge(
    summary_dt,
    year_totals,
    by = "year"
  )

  summary_dt$proportion <- summary_dt$total / summary_dt$year_total

  summary_dt$year_total <- NULL

  summary_dt
}
