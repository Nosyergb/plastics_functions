test_that("wrangle_plastic_types returns a data.table", {
  result <- wrangle_plastic_types()

  expect_s3_class(result, "data.table")
})

test_that("wrangle_plastic_types returns expected columns", {
  result <- wrangle_plastic_types()

  expect_true(
    all(
      c(
        "year",
        "plastic_type",
        "total",
        "proportion"
      ) %in% names(result)
    )
  )
})

test_that("wrangle_plastic_types returns expected plastic types", {
  result <- wrangle_plastic_types()

  expect_true(
    all(
      c(
        "hdpe",
        "ldpe",
        "o",
        "pet",
        "pp",
        "ps",
        "pvc"
      ) %in% result$plastic_type
    )
  )
})

test_that("wrangle_plastic_types returns nonnegative totals", {
  result <- wrangle_plastic_types()

  expect_true(
    all(result$total >= 0)
  )
})

test_that("wrangle_plastic_types returns proportions between 0 and 1", {
  result <- wrangle_plastic_types()

  expect_true(
    all(result$proportion >= 0 & result$proportion <= 1)
  )
})

test_that("wrangle_plastic_types proportions sum to 1 within each year", {
  result <- wrangle_plastic_types()

  prop_sums <- result |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      prop_sum = sum(proportion, na.rm = TRUE),
      .groups = "drop"
    )

  expect_true(
    all(abs(prop_sums$prop_sum - 1) < 0.001)
  )
})

test_that("wrangle_plastic_types rejects data missing required columns", {
  bad_data <- tibble::tibble(
    year = c(2019, 2020),
    hdpe = c(1, 2)
  )

  expect_error(
    wrangle_plastic_types(
      data = bad_data
    )
  )
})
