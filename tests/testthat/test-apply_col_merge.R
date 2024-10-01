test_that("multiplication works", {
  #case study: gtcars
  # Get a subset of 8 cars from the `gtcars` dataset: two
  # from each manufacturer country of origin except the UK

  order_countries <- c("Germany", "Italy", "United States", "Japan")
  # Reorder the table rows by our specific ordering of groups
  tab <-
    gtcars_8 |>
    dplyr::arrange(
      factor(ctry_origin, levels = order_countries),
      mfr, desc(msrp)
    ) |>
    dplyr::mutate(car = paste(mfr, model)) |>
    dplyr::select(-mfr, -model) |>
    dplyr::group_by(ctry_origin) |>
    gt::gt(rowname_col = "car")

  # Use a few `cols_*()` functions to hide and move columns
  tab <-
    tab |>
    gt::cols_hide(columns = c(drivetrain, bdy_style)) |>
    gt::cols_move(
      columns = c(trsmn, mpg_c, mpg_h),
      after = trim
    )

  # Put the first three columns under a spanner
  # column with the label 'Performance'
  tab <-
    tab |>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  # Perform three column merges to better present
  # MPG, HP, and torque; relabel all the remaining
  # columns for a nicer-looking presentation
  tab <-
    tab |>
    gt::cols_merge(
      columns = c(mpg_c, mpg_h),
      pattern = "<<{1}c<br>{2}h>>"
    ) |>
    gt::cols_merge(
      columns = c(hp, hp_rpm),
      pattern = "{1}<br>@{2}rpm"
    ) |>
    gt::cols_merge(
      columns = c(trq, trq_rpm),
      pattern = "{1}<br>@{2}rpm"
    ) |>
    gt::cols_label(
      mpg_c = "MPG",
      hp = "HP",
      trq = "Torque",
      year = "Year",
      trim = "Trim",
      trsmn = "Transmission",
      msrp = "MSRP"
    )

  # Show the table
  actual_ordered_data <- tab |>
    create_ordered_data()

  actual_output <- tab |>
    apply_col_merge(actual_ordered_data)

  raw_expected_output <- readRDS(test_path("fixtures","gtcars8_example_ordered_data.RDS"))

  expected_output <- raw_expected_output |>
    dplyr::select(-c(drivetrain, bdy_style)) |>
    dplyr::relocate(c(trsmn, mpg_c, mpg_h),
                    .after = trim) |>
    dplyr::mutate(mpg_c = paste0(mpg_c, "c<br>", mpg_h, "h"),
                  mpg_h = NULL,
                  hp = paste0(hp, "<br>@", hp_rpm, "rpm"),
                  hp_rpm = NULL,
                  trq = paste0(trq, "<br>@", trq_rpm, "rpm"),
                  trq_rpm = NULL)

  testthat::expect_equal(actual_output, expected_output)
  })
