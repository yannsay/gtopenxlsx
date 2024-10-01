test_that("write_source_note works", {

  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx::createWorkbook()

  tester_wrapper <- function(gt_table, sheet) {
    openxlsx::addWorksheet(wb, sheet)

    ordered_example <- gt_table |>
      create_ordered_data()
    write_source_note(gt_table,
                       ordered_example,
                       wb = wb,
                       sheet_name = sheet,
                       row_to_start = 1)

  }

  one_source_note <-  gtcars_8 |>
    gt::gt() |>
    gt::tab_source_note(
      source_note = gt::md(
        "Source: Various pages within the Edmonds website."
      ))

  tester_wrapper(one_source_note, "one_source_note")

  two_source_note <- one_source_note |>
    gt::tab_source_note(
      source_note = "source note 2")

  tester_wrapper(two_source_note, "two_source_note")

  temp_file_location <- paste0(temp_dir_to_test, "\\footers.xlsx")
  openxlsx::saveWorkbook(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x)) |>
    suppressWarnings()

  expected_file_location <- testthat::test_path("fixtures", "footers.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x))|>
    suppressWarnings()

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])
  }
  )
