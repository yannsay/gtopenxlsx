#' Write a gt table to an xlsx workbook
#'
#' @param wb Name of the Workbook object created with `openxlsx2::wb_workbook()`
#' @param gt_table A gt table
#'
#' @return wb with the active sheet with the gt table rendered.
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook() |>
#'   openxlsx2::wb_add_worksheet("gtcars_example_0")
#' example_0 <- gt::gtcars |>
#'   gt::gt()
#' wb <- gt_to_xlsx(wb = wb,example_0)
#'
gt_to_xlsx <- function(wb,
                       gt_table
                       #sheet_name
                       ) {
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

  wb <- write_table_header(
    wb = wb,

    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    row_to_start = current_row
  )
  if (!is.null(gt_table$`_heading`$title)) {
    current_row <- current_row + 1
  }

  if (!is.null(gt_table$`_heading`$subtitle)) {
    current_row <- current_row + 1
  }

  if (nrow(gt_table$`_spanners`) > 0) {
    wb <- write_spanners(
      gt_table = gt_table,
      ordered_gt_data = ordered_data,
      wb = wb,
      row_to_start = current_row
    )

    number_of_level <- gt_table$`_spanners`$spanner_level |> max()
    current_row <- current_row + number_of_level
  }


  wb <- write_stub_head_and_column_labels(
    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    wb = wb,
    row_to_start = current_row
  )

  current_row <- current_row + 1

  wb_and_indexes <- write_stub_and_table_body(
    gt_table = gt_table,
    ordered_gt_data = ordered_data,
    wb = wb,
    row_to_start = current_row
  )

  wb <- wb_and_indexes[["wb"]]

  list_cell_indexes <- wb_and_indexes[["cell_data_row"]]


  if (length(gt_table$`_source_notes`) > 0) {
    final_index_stub <- list_cell_indexes |>
      purrr::map(~ .x[["end"]]) |>
      purrr::list_c() |>
      max()

    current_row <- final_index_stub + 1
   wb <- write_source_note(
      gt_table = gt_table,
      ordered_gt_data = ordered_data,
      wb = wb,
      row_to_start = current_row
    )
  }
  return(wb)
}
