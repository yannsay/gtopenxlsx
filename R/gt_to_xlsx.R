#' Write a gt table to an xlsx workbook
#'
#' @param gt_table A gt table
#' @param wb Name of Workbook object like `openxlsx::createWorkbook()`
#' @param sheet_name name of the sheet in the workbook
#'
#' @return Nothing. It will update the wb object.
#' @export
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "gtcars_example_0")
#'
#' example_0 <- gt::gtcars |>
#'   gt::gt()
#'
#' example_0 |>
#'   gt_to_xlsx(wb = wb, sheet_name = "gtcars_example_0")
gt_to_xlsx <- function(gt_table, wb, sheet_name) {
  ordered_data <- gt_table |>
    create_ordered_data()

  if ("_formats" %in% names(gt_table)) {
    ordered_data <- apply_formats(
      gt_table,
      ordered_data
    )
  }

  if ("_col_merge" %in% names(gt_table)) {
    ordered_data <- apply_col_merge(gt_table, ordered_data)
  }

  current_row <- 1

  write_table_header(
    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    wb = wb,
    sheet_name = sheet_name,
    row_to_start = current_row
  )
  if (!is.null(gt_table$`_heading`$title)) {
    current_row <- current_row + 1
  }

  if (!is.null(gt_table$`_heading`$subtitle)) {
    current_row <- current_row + 1
  }

  if (nrow(gt_table$`_spanners`) > 0) {
    write_spanners(
      gt_table = gt_table,
      ordered_gt_data = ordered_data,
      wb = wb,
      sheet_name = sheet_name,
      row_to_start = current_row
    )

    number_of_level <- gt_table$`_spanners`$spanner_level |> max()
    current_row <- current_row + number_of_level
  }


  write_stub_head_and_column_labels(
    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    wb = wb,
    sheet_name = sheet_name,
    row_to_start = current_row
  )

  current_row <- current_row + 1

  list_cell_indexes <- write_stub_and_table_body(
    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    wb = wb,
    sheet_name = sheet_name,
    row_to_start = current_row
  )


  if (length(gt_table$`_source_notes`) > 0) {
    final_index_stub <- list_cell_indexes |>
      purrr::map(~ .x[["end"]]) |>
      purrr::list_c() |>
      max()

    current_row <- final_index_stub + 1
    write_source_note(
      gt_table = gt_table,
      ordered_gt_data = ordered_data,
      wb = wb,
      sheet_name = sheet_name,
      row_to_start = current_row
    )
  }
}
