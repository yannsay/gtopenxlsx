test_that("Test that stub and table body are written correclty", {

  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx::createWorkbook()

  actual_indexes_list <- list()
  tester_wrapper <- function(gt_table, sheet) {
    openxlsx::addWorksheet(wb, sheet)

    ordered_example <- gt_table |>
      create_ordered_data()
    write_stub_and_table_body(gt_table,
                              ordered_example,
                              wb = wb,
                              sheet_name = sheet,
                              row_to_start = 1)
  }

  # no group_by
  example_0 <- gtcars_8 |>
    gt::gt()

  actual_indexes_list[["stub_and_body"]] <- tester_wrapper(example_0, "stub_and_body")

  # with group_by
  example_1 <- gtcars_8 |>
    dplyr::group_by(ctry_origin) |>
    gt::gt()

  actual_indexes_list[["group_by"]] <- tester_wrapper(example_1, "group_by")

  # with rowname_col
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

  actual_indexes_list[["group_by_and_rowname_col"]] <- tester_wrapper(tab, "group_by_and_rowname_col")

  temp_file_location <- paste0(temp_dir_to_test, "\\stub_and_body_test.xlsx")
  openxlsx::saveWorkbook(wb, temp_file_location)

  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x))

  expected_file_location <- testthat::test_path("fixtures", "stub_and_body.xlsx")

  expected_output <- readxl::excel_sheets(expected_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = expected_file_location, sheet = .x))

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])
  expect_equal(actual_output[[3]], expected_output[[3]])

  expected_indexes_list <- readRDS(testthat::test_path("fixtures", "write_stub_indexes_list.RDS"))
  expect_equal(actual_indexes_list, expected_indexes_list)
})

