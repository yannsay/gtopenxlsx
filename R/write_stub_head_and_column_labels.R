#' Write stub and columns label
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return wb with the active sheet with stub and columns labels.
#' @export
#'
#' @examples
#' wb <- openxlsx2::wb_workbook() |>
#'   openxlsx2::wb_add_worksheet()
#'
#' gt_table_example <- gtcars_8 |>
#'   gt::gt()
#' ordered_example <- gt_table_example |>
#'   create_ordered_data()
#' wb <- wb |>
#'   write_stub_head_and_column_labels(
#'     row_to_start = 1,
#'     gt_table = gt_table_example,
#'     ordered_gt_data = ordered_example
#'   )
#'
write_stub_head_and_column_labels <- function(wb, row_to_start,
                                              gt_table,
                                              ordered_gt_data) {
  labels_to_change <- gt_table$`_boxhead`$var |>
    purrr::set_names(gt_table$`_boxhead`$column_label)
  ordered_gt_data <- ordered_gt_data |>
    dplyr::rename(tidyselect::any_of(labels_to_change))
  label_to_write <- names(ordered_gt_data) |> as.list()

  if ("stub" %in% gt_table$`_boxhead`$type) {
    label_to_write[[1]] <- NA_character_
  }
  ## write columns names
  wb <- wb |>
    openxlsx2::wb_add_data(
      x = label_to_write,
      col_names = FALSE,
      start_row = row_to_start
    ) |>
    apply_style_column_label(rows = row_to_start, cols = 1:length(label_to_write))
  return(wb)
}
