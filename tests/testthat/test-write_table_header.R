test_that("Test that headers are written correctly", {
  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx2::wb_workbook()

  tester_wrapper <- function(wb, gt_table, sheet) {
    wb <- wb |>
      openxlsx2::wb_add_worksheet(sheet = sheet, grid_lines = FALSE)


    ordered_example <- gt_table |>
      create_ordered_data()
    wb <- write_table_header(gt_table,
      ordered_example,
      wb = wb,
      row_to_start = 1
    )

    return(wb)
  }
  no_title <- gtcars_8 |>
    gt::gt()

  wb <- tester_wrapper(wb, gt_table = no_title, sheet = "no_title")

  only_title <- no_title |>
    gt::tab_header(
      title = gt::md("The Cars of **gtcars**")
    )
  wb <- tester_wrapper(wb, gt_table = only_title, sheet = "only_title")


  both_headers <- no_title |>
    gt::tab_header(
      title = gt::md("The Cars of **gtcars**"),
      subtitle = "These are some fine automobiles"
    )
  wb <- tester_wrapper(wb, gt_table = both_headers, sheet = "both_headers")

  temp_file_location <- paste0(temp_dir_to_test, "\\headers.xlsx")
  openxlsx2::wb_save(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~ openxlsx2::wb_to_df(file = temp_file_location, sheet = .x)) |>
    suppressMessages()

  expected_file_location <- testthat::test_path("fixtures", "headers.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    purrr::map(~ openxlsx2::wb_to_df(file = expected_file_location, sheet = .x)) |>
    suppressMessages()

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])
  expect_equal(actual_output[[3]], expected_output[[3]])
})
