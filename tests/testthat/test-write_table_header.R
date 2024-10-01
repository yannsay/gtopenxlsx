test_that("Test that headers are written correctly", {
  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx::createWorkbook()

  tester_wrapper <- function(gt_table, sheet) {
    openxlsx::addWorksheet(wb, sheet)

    ordered_example <- gt_table |>
      create_ordered_data()
    write_table_header(gt_table,
      ordered_example,
      wb = wb,
      sheet_name = sheet,
      row_to_start = 1
    )
  }
  no_title <- gtcars_8 |>
    gt::gt()

  tester_wrapper(no_title, "no_title")

  only_title <- no_title |>
    gt::tab_header(
      title = gt::md("The Cars of **gtcars**")
    )

  tester_wrapper(only_title, "only_title")

  both_headers <- no_title |>
    gt::tab_header(
      title = gt::md("The Cars of **gtcars**"),
      subtitle = "These are some fine automobiles"
    )

  tester_wrapper(both_headers, "both_headers")

  temp_file_location <- paste0(temp_dir_to_test, "\\headers.xlsx")
  openxlsx::saveWorkbook(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~ openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x)) |>
    suppressWarnings()

  expected_file_location <- testthat::test_path("fixtures", "headers.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    purrr::map(~ openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x)) |>
    suppressWarnings()

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])
  expect_equal(actual_output[[3]], expected_output[[3]])
})
