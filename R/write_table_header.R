#' Write table headers
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return Nothing. It will update the wb object with table headers.
#' @export
write_table_header <- function(gt_table, ordered_gt_data, wb, sheet_name, row_to_start) {
  current_row <- row_to_start
  total_cols <- ncol(ordered_gt_data)

  if (!is.null(gt_table$`_heading`$title)) {
    openxlsx::writeData(wb, sheet_name, gt_table$`_heading`$title, startCol = 1, startRow = current_row, colNames = F)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:total_cols, rows = current_row)

    openxlsx::addStyle(wb, sheet_name, rows = current_row, cols = 1:total_cols, style = title_style)
    current_row <- current_row + 1
  }

  ## write subtitle
  if (!is.null(gt_table$`_heading`$subtitle)) {
    openxlsx::writeData(wb, sheet_name, gt_table$`_heading`$subtitle, startCol = 1, startRow = current_row, colNames = F)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:total_cols, rows = current_row)

    openxlsx::addStyle(wb, sheet_name, rows = current_row, cols = 1:total_cols, style = title_style)
  }
}
