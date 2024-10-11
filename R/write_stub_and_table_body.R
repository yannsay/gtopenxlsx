#' Write stub and table body
#'
#' @inheritParams gt_to_xlsx
#' @param ordered_gt_data ordered data from `create_ordered_data`
#' @param row_to_start Number of the row where to start writing the stub and body.
#'
#' @return Returns a list with :
#'    - wb with the active sheet with stub and body table.
#'    - a list with the first row and last row with information for each group.
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook() |>
#'   openxlsx2::wb_add_worksheet()
#' example_0 <- gt::gtcars |>
#'   gt::gt()
#' gtcars_example <- example_0 |>
#'   create_ordered_data()
#' wb <-  wb |>
#'   write_stub_and_table_body(gt_table = example_0,ordered_gt_data = gtcars_example,1)
write_stub_and_table_body <- function(wb,row_to_start,gt_table, ordered_gt_data
                                      ) {
  label_groups_row <- list()
  cell_data_row <- list()
  if (rlang::is_empty(gt_table$`_row_groups`)) {
    wb <- wb |>
      openxlsx2::wb_add_data(x = ordered_gt_data,
                            start_row = row_to_start,
                            col_names = FALSE) |>
      apply_style_border_stub_and_body(rows = row_to_start, cols = 1:ncol(ordered_gt_data))

    cell_data_row[["all"]][["start"]] <- row_to_start
    cell_data_row[["all"]][["end"]] <- row_to_start + nrow(ordered_gt_data) - 1
  } else {
    # stub
    ## row group label
    row_start_stub <- row_to_start

    for (i in unique(gt_table$`_row_groups`)) {
      wb <- wb |>
        openxlsx2::wb_add_data(x = i,
                              start_row = row_start_stub,
                              col_names = FALSE) |>
        openxlsx2::wb_merge_cells( cols = 1:ncol(ordered_gt_data), rows = row_start_stub ) |>

        apply_style_column_label(rows = row_to_start, cols = 1:ncol(ordered_gt_data))

      label_groups_row[[i]] <- row_start_stub

      row_start_stub <- row_start_stub + 1

      data_to_write <- ordered_gt_data[gt_table$`_stub_df`$group_id == i, ]
      wb <- wb |>
        openxlsx2::wb_add_data(x = data_to_write,
                              start_row = row_start_stub,
                              col_names = FALSE) |>
        apply_style_border_stub_and_body(rows = row_start_stub, cols = 1:ncol(data_to_write))

      cell_data_row[[i]][["start"]] <- row_start_stub
      cell_data_row[[i]][["end"]] <- row_start_stub + nrow(data_to_write) - 1

      row_start_stub <- row_start_stub + nrow(data_to_write)
    }
  }
  return(list(wb = wb,
              cell_data_row = cell_data_row))
}
