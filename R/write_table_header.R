#' Write table headers
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return Nothing. It will update the wb object with table headers.
#' @export
write_table_header <- function(wb, row_to_start, gt_table, ordered_gt_data) {
  current_row <- row_to_start
  total_cols <- ncol(ordered_gt_data)
  # wb <- wb |>
  #   openxlsx2::wb_set_active_sheet(sheet_name)

  if (!is.null(gt_table$`_heading`$title)) {
    # openxlsx::writeData(wb, sheet_name, gt_table$`_heading`$title, startCol = 1, startRow = current_row, colNames = F)
    wb <- wb |>
      openxlsx2::wb_add_data(x = gt_table$`_heading`$title, start_col = 1, start_row = current_row, col_names = FALSE) |>

    # openxlsx::mergeCells(wb, sheet_name, cols = 1:total_cols, rows = current_row)
    openxlsx2::wb_merge_cells( cols = 1:total_cols, rows = current_row ) |>

    # openxlsx::addStyle(wb, sheet_name, rows = current_row, cols = 1:total_cols, style = title_style)
      apply_style_title(cols = 1:total_cols, rows = current_row)

    current_row <- current_row + 1
  }

  ## write subtitle
  if (!is.null(gt_table$`_heading`$subtitle)) {
    # openxlsx::writeData(wb, sheet_name, gt_table$`_heading`$subtitle, startCol = 1, startRow = current_row, colNames = F)
    wb <- wb |>
      openxlsx2::wb_add_data(x = gt_table$`_heading`$subtitle, start_col = 1, start_row = current_row, col_names = FALSE) |>

    # openxlsx::mergeCells(wb, sheet_name, cols = 1:total_cols, rows = current_row)
    openxlsx2::wb_merge_cells( cols = 1:total_cols, rows = current_row ) |>

    # openxlsx::addStyle(wb, sheet_name, rows = current_row, cols = 1:total_cols, style = title_style)
      apply_style_title(cols = 1:total_cols, rows = current_row)

  }

  return(wb)
}
