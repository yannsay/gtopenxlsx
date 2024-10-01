test_that("columns names are correct, no labels, no spanner", {
  temp_dir_to_test <- withr::local_tempdir(fileext = "test")
  wb <- openxlsx::createWorkbook()

  tab <- gtcars_8 |>
    dplyr::mutate(car = paste(mfr, model)) |>
    dplyr::select(-mfr, -model)

  # no group_by
  openxlsx::addWorksheet(wb, "gtcars_test_no_group")

  gt_table_no_group <- tab |>
    gt::gt()

  ordered_example <- gt_table_no_group |>
    create_ordered_data()

  write_stub_head_and_column_labels(gt_table_no_group,
                                    ordered_example,
                                    wb = wb,
                                    sheet_name = "gtcars_test_no_group",
                                    row_to_start = 1)
  # group_by
  openxlsx::addWorksheet(wb, "gtcars_test_gt_table_group_by")

  gt_table_group_by <- tab |>
    dplyr::group_by(ctry_origin) |>
    gt::gt()

  ordered_example <- gt_table_group_by |>
    create_ordered_data()

  write_stub_head_and_column_labels(gt_table_group_by,
                                    ordered_example,
                                    wb = wb,
                                    sheet_name = "gtcars_test_gt_table_group_by",
                                    row_to_start = 1)

  # rowname_col
  openxlsx::addWorksheet(wb, "gtcars_test_rowname_col")

  gt_table_rowname_col <- tab |>
    gt::gt(rowname_col = "ctry_origin")

  ordered_example <- gt_table_rowname_col |>
    create_ordered_data()

  write_stub_head_and_column_labels(gt_table_rowname_col,
                                    ordered_example,
                                    wb = wb,
                                    sheet_name = "gtcars_test_rowname_col",
                                    row_to_start = 1)

  # group_by and rowname_col
  openxlsx::addWorksheet(wb, "gtcars_test_both")

  gt_table_both <- tab |>
    dplyr::group_by(ctry_origin) |>
    gt::gt(rowname_col = "car")

  ordered_example <- gt_table_both |>
    create_ordered_data()

  write_stub_head_and_column_labels(gt_table_both,
                                    ordered_example,
                                    wb = wb,
                                    sheet_name = "gtcars_test_both",
                                    row_to_start = 1)

  # cols merge and labels
  order_countries <- c("Germany", "Italy", "United States", "Japan")

  gt_table_spanners_merge_labels <- gtcars_8 |>
    dplyr::arrange(
      factor(ctry_origin, levels = order_countries),
      mfr, desc(msrp)
    ) |>
    dplyr::mutate(car = paste(mfr, model)) |>
    dplyr::select(-mfr, -model) |>
    dplyr::group_by(ctry_origin) |>
    gt::gt(rowname_col = "car")|>
    gt::cols_hide(columns = c(drivetrain, bdy_style)) |>
    gt::cols_move(
      columns = c(trsmn, mpg_c, mpg_h),
      after = trim
    )|>
    gt::tab_spanner(
      label = "Performance",
      columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
    )|>
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

  ordered_example <- gt_table_spanners_merge_labels |>
    create_ordered_data()
  openxlsx::addWorksheet(wb, "gt_spanners_merge_labels")

  write_stub_head_and_column_labels(gt_table_spanners_merge_labels,
                                    ordered_example,
                                    wb = wb,
                                    sheet_name = "gt_spanners_merge_labels",
                                    row_to_start = 1)



  temp_file_location <- paste0(temp_dir_to_test, "\\gtcars_test_col_names.xlsx")
  openxlsx::saveWorkbook(wb, temp_file_location)
  actual_output <- readxl::excel_sheets(temp_file_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = temp_file_location, sheet = .x))

  expected_output_location <- testthat::test_path("fixtures", "gtcars_test_col_names.xlsx")
  expected_output <- readxl::excel_sheets(expected_output_location) |>
    purrr::map(~openxlsx::read.xlsx(xlsxFile = expected_output_location, sheet = .x))

  expect_equal(actual_output[[1]], expected_output[[1]])
  expect_equal(actual_output[[2]], expected_output[[2]])
  expect_equal(actual_output[[3]], expected_output[[3]])
  expect_equal(actual_output[[4]], expected_output[[4]])
  expect_equal(actual_output[[5]], expected_output[[5]])

  })
