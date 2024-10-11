#' Write table headers
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return wb with the active sheet with the source note.
#' @export
write_source_note <- function(wb,
                              row_to_start,
                              gt_table,
                              ordered_gt_data) {
  current_row <- row_to_start
  total_cols <- ncol(ordered_gt_data)

  for (i in 1:length(gt_table$`_source_notes`)) {
    dims_to_style <- openxlsx2::wb_dims(rows = current_row, cols = 1:total_cols)

    wb <- wb |>
      openxlsx2::wb_add_data(
        x = gt_table[["_source_notes"]][[i]] |> as.character(),
        start_row = current_row, col_names = FALSE
      ) |>
      openxlsx2::wb_merge_cells(dims = dims_to_style) |>
      openxlsx2::wb_add_font(dims = dims_to_style, size = 10)
    current_row <- current_row + 1
  }
  return(wb)
}
