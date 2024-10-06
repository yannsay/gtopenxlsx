test_that("Test that group_by, rowname_col, spanners work correctly", {
  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  # wb <- openxlsx::createWorkbook()
  wb <- openxlsx2::wb_workbook()

  # no group_by
  # openxlsx::addWorksheet(wb, "example_1")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_1")

  example_1 <- gtcars_8 |>
    gt::gt()

  wb <- gt_to_xlsx(example_1, wb#,
                   # "example_1"
                   )

  # with group_by
  # openxlsx::addWorksheet(wb, "example_2")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_2")

  example_2 <- gtcars_8 |>
    dplyr::group_by(ctry_origin) |>
    gt::gt()

  wb<-gt_to_xlsx(example_2, wb#,
                 #"example_2"
                 )

  # with rowname_col
  # openxlsx::addWorksheet(wb, "example_3")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_3")
  order_countries <- c("Germany", "Italy", "United States", "Japan")

  example_3 <-
    gtcars_8 |>
    dplyr::arrange(
      factor(ctry_origin, levels = order_countries),
      mfr, dplyr::desc(msrp)
    ) |>
    dplyr::mutate(car = paste(mfr, model)) |>
    dplyr::select(-mfr, -model) |>
    dplyr::group_by(ctry_origin) |>
    gt::gt(rowname_col = "car")

  wb <- gt_to_xlsx(example_3, wb#,
                   #"example_3"
                   )

  # with spanners
  # openxlsx::addWorksheet(wb, "example_4")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_4")

  order_countries <- c("Germany", "Italy", "United States", "Japan")

  example_4 <- example_3 |>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  wb <- gt_to_xlsx(example_4, wb#,
                   #"example_4"
                   )

  # with multiple spanners
  # openxlsx::addWorksheet(wb, "example_5")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_5")

  example_5 <- example_3 |>
    gt::tab_spanner(
      label = "MPG",
      columns = starts_with("mpg"),
      id = "mpg"
    ) |>
    gt::tab_spanner(
      label = "HP",
      columns = starts_with("hp"),
      id = "hp-span"
    ) |>
    gt::tab_spanner(
      label = "TRQ",
      columns = starts_with("trq"),
      id = "trq-span"
    ) |>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  wb <- gt_to_xlsx(example_5, wb#,
                   #"example_5"
                   )

  # with headers
  # openxlsx::addWorksheet(wb, "example_6")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_6")

  example_6 <- example_5 |>
    gt::tab_header(
      title = gt::md("The Cars of **gtcars**"),
      subtitle = "These are some fine automobiles"
    )
  wb <- gt_to_xlsx(example_6, wb#,
                   #"example_6"
                   )

  # with footsource
  # openxlsx::addWorksheet(wb, "example_7")
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet = "example_7")

  example_7 <- example_6 |>
    gt::tab_source_note(
      source_note = gt::md(
        "Source: Various pages within the Edmonds website."
      )
    )
  wb <- gt_to_xlsx(example_7, wb#,
                   #"example_7"
                   )

  temp_file_location <- paste0(temp_dir_to_test, "\\gt_to_xlsx.xlsx")

  # openxlsx::saveWorkbook(wb, temp_file_location)
  openxlsx2::wb_save(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    # purrr::map(~ openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x))
    purrr::map(~ openxlsx2::wb_to_df(file = temp_file_location, sheet = .x))

  expected_file_location <- testthat::test_path("fixtures", "gt_to_xlsx.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    # purrr::map(~ openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x))
    purrr::map(~ openxlsx2::wb_to_df(file = expected_file_location, sheet = .x))

  # no group_by
  expect_equal(actual_output[[1]], expected_output[[1]])

  # with group_by
  expect_equal(actual_output[[2]], expected_output[[2]])

  # with rowname_col
  expect_equal(actual_output[[3]], expected_output[[3]])

  # with spanners
  expect_equal(actual_output[[4]], expected_output[[4]])

  # with multiple spanners
  expect_equal(actual_output[[5]], expected_output[[5]])

  # with headers
  expect_equal(actual_output[[6]], expected_output[[6]])
})

test_that("apply_col_merge is working correclty", {
  order_countries <- c("Germany", "Italy", "United States", "Japan")
  # Reorder the table rows by our specific ordering of groups
  test_col_merge <-
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
  test_col_merge <-
    test_col_merge |>
    gt::cols_hide(columns = c(drivetrain, bdy_style)) |>
    gt::cols_move(
      columns = c(trsmn, mpg_c, mpg_h),
      after = trim
    )

  # Put the first three columns under a spanner
  # column with the label 'Performance'
  test_col_merge <-
    test_col_merge |>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  # Perform three column merges to better present
  # MPG, HP, and torque; relabel all the remaining
  # columns for a nicer-looking presentation
  test_col_merge <-
    test_col_merge |>
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

  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  # wb <- openxlsx::createWorkbook()
  wb <- openxlsx2::wb_workbook()

  # openxlsx::addWorksheet(wb, "example_cols_merge")
  wb <- wb |> openxlsx2::wb_add_worksheet("example_cols_merge")
  wb <- gt_to_xlsx(test_col_merge, wb#, "example_cols_merge"
                   )

  temp_file_location <- paste0(temp_dir_to_test, "\\gt_to_xlsx_cols_merge.xlsx")

  # openxlsx::saveWorkbook(wb, temp_file_location)
  openxlsx2::wb_save(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    # purrr::map(~ openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x))
    purrr::map(~ openxlsx2::wb_to_df(file = temp_file_location, sheet = .x))

  expected_file_location <- testthat::test_path("fixtures", "gt_to_xlsx_cols_merge.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    # purrr::map(~ openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x))
    purrr::map(~ openxlsx2::wb_to_df(file = expected_file_location, sheet = .x))

  # cols_merge
  expect_equal(actual_output[[1]], expected_output[[1]])
})


test_that("test that formats works correclty", {
  fmt_currency_example <-
    gtcars_8 |>
    gt::gt() |>
    gt::fmt_currency(columns = msrp, decimals = 0)

  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  # wb <- openxlsx::createWorkbook()
  wb <- openxlsx2::wb_workbook()

  # openxlsx::addWorksheet(wb, "fmt_currency_example")
  wb <- wb |>
    openxlsx2::wb_add_worksheet("fmt_currency_example")
  wb <- gt_to_xlsx(fmt_currency_example, wb#, "fmt_currency_example"
                   )

  temp_file_location <- paste0(temp_dir_to_test, "\\fmt_currency_example.xlsx")

  # openxlsx::saveWorkbook(wb, temp_file_location)
  openxlsx2::wb_save(wb, temp_file_location)

  # actual_output <- openxlsx::read.xlsx(temp_file_location)
  actual_output <- openxlsx2::wb_to_df(temp_file_location)

  expected_file_location <- testthat::test_path("fixtures", "gt_to_xlsx_fmt_currency.xlsx")

  # expected_output <- openxlsx::read.xlsx(expected_file_location)
  expected_output <- openxlsx2::wb_to_df(expected_file_location)

  # cols_merge
  expect_equal(actual_output[[1]], expected_output[[1]])

})
