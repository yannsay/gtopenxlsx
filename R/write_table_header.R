#' Write table headers
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return wb with the active sheet with table headers.
#' @export
write_table_header <- function(wb, row_to_start, gt_table, ordered_gt_data) {
  current_row <- row_to_start
  total_cols <- ncol(ordered_gt_data)

  if (!is.null(gt_table$`_heading`$title)) {
    wb <- wb |>
      openxlsx2::wb_add_data(x = gt_table$`_heading`$title, start_col = 1, start_row = current_row, col_names = FALSE) |>
      openxlsx2::wb_merge_cells(cols = 1:total_cols, rows = current_row) |>
      apply_style_title(cols = 1:total_cols, rows = current_row)

    current_row <- current_row + 1
  }

  ## write subtitle
  if (!is.null(gt_table$`_heading`$subtitle)) {
    wb <- wb |>
      openxlsx2::wb_add_data(x = gt_table$`_heading`$subtitle, start_col = 1, start_row = current_row, col_names = FALSE) |>
      openxlsx2::wb_merge_cells(cols = 1:total_cols, rows = current_row) |>
      apply_style_title(cols = 1:total_cols, rows = current_row)
  }

  return(wb)
}
