test_that("return expected ordered when using dyplr::group_by, gt::col_move or gt::col_hide", {

  gtcars_8 <-
    gt::gtcars |>
    dplyr::group_by(ctry_origin) |>
    dplyr::slice_head(n = 2) |>
    dplyr::ungroup() |>
    dplyr::filter(ctry_origin != "United Kingdom")
  order_countries <- c("Germany", "Italy", "United States", "Japan")

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

  tab_with_hide_and_move <-
    tab |>
    gt::cols_hide(columns = c(drivetrain, bdy_style)) |>
    gt::cols_move(
      columns = c(trsmn, mpg_c, mpg_h),
      after = trim
    )

  actual_output <- tab |>
    create_ordered_data()

  actual_output_with_hide_and_move <- tab_with_hide_and_move |>
    create_ordered_data()

  expected_output <- readRDS(test_path("fixtures","gtcars8_example_ordered_data.RDS"))

  expected_output_with_hide_and_move  <- expected_output |>
    dplyr::select(-c(drivetrain, bdy_style)) |>
    dplyr::relocate(c(trsmn, mpg_c, mpg_h),
                    .after = trim)

  expect_equal(actual_output,expected_output)

  expect_equal(actual_output_with_hide_and_move,expected_output_with_hide_and_move)

})
