test_that("Test that spanners are written correctly", {

  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx::createWorkbook()

  tester_wrapper <- function(gt_table, sheet) {
    openxlsx::addWorksheet(wb, sheet)

    ordered_example <- gt_table |>
      create_ordered_data()
    write_spanners(gt_table,
                              ordered_example,
                              wb = wb,
                              sheet_name = sheet,
                              row_to_start = 1)

  }
  order_countries <- c("Germany", "Italy", "United States", "Japan")
  tab <-
    gtcars_8 |>
    dplyr::arrange(
      factor(ctry_origin, levels = order_countries),
      mfr, dplyr::desc(msrp)
    ) |>
    dplyr::mutate(car = paste(mfr, model)) |>
    dplyr::select(-mfr, -model) |>
    dplyr::group_by(ctry_origin) |>
    gt::gt(rowname_col = "car")

  tab_with_spanner <-
    tab |>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  tester_wrapper(tab_with_spanner, "one_spanner")

  tab_with_two_level_spanner <-
    tab |>
    gt::tab_spanner(
      label = "mpg",
      columns = starts_with("mpg"),
      id = "mpg"
    ) |>
    gt::tab_spanner(
      label = "hp",
      columns = starts_with("hp"),
      id = "hp-span"
    ) |>
    gt::tab_spanner(
      label = "trq",
      columns = starts_with("trq"),
      id = "trq-span"
    )|>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )

  tester_wrapper(tab_with_two_level_spanner, "two_spanners")

  temp_file_location <- paste0(temp_dir_to_test, "\\spanners.xlsx")
  openxlsx::saveWorkbook(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x))

  expected_file_location <- testthat::test_path("fixtures", "spanners.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x))

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])

})
