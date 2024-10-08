#' Get information for a given spanner
#'
#' @inheritParams write_stub_and_table_body
#' @param row number of the row if the `_spanners` df
#'
#' @return a list with the label spanner and columns numbers.
#' @export
#'
get_spanner_info <- function(gt_table, row, ordered_gt_data) {
  list(
    label_spanner = gt_table$`_spanners`$spanner_label[[row]],
    column_ids_spanner = which(names(ordered_gt_data) %in% gt_table$`_spanners`$vars[[row]])
  )
}

#' Create a helper to write spanners
#'
#' It will create a list for information:
#' - A list for each level spanner.
#'
#' Within each level list, there will one list for each spanner with label and columns numbers.
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return A list of helpers.
#' @export
#'
create_spanner_helper <- function(gt_table, ordered_gt_data) {
  spanner_helper <- list()
  for (i in max(gt_table$`_spanners`$spanner_level):1) {
    spanner_helper[[paste0("level", i)]] <- which(gt_table$`_spanners`$spanner_level == i) |>
      purrr::map(~ get_spanner_info(gt_table, .x, ordered_gt_data))
  }
  return(spanner_helper)
}

#' Helper to write one spanner
#'
#' It will write one spanner.
#'
#' @inheritParams write_stub_and_table_body
#' @param spanner_info helper with spanner columns number and label.
#' @param style style to use to format the spanner.
#'
#' @return Nothing. It will update the wb object with one spanner.
#'
write_one_spanner <- function(spanner_info,
                              wb,
                              sheet_name,
                              row_to_start,
                              style = column_labels_border) {
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = spanner_info$label_spanner,
    startCol = min(spanner_info$column_ids_spanner),
    startRow = row_to_start
  )
  openxlsx::mergeCells(wb = wb, sheet = sheet_name, cols = spanner_info$column_ids_spanner, rows = row_to_start)
  openxlsx::addStyle(
    wb = wb, sheet = sheet_name, rows = row_to_start,
    cols = spanner_info$column_ids_spanner, style = style
  )
}

#' Write spanners
#'
#' It creates a spanner helper with `create_ordered_data` then uses `write_one_spanner` to write all
#' spanners.
#' The cells of the spanner will be merged.
#'
#' @inheritParams write_stub_and_table_body
#'
#' @return Nothing. It will update the wb object with all spanners.
#' @export
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "one_spanner")
#'
#' tab_with_spanner <-
#'   gtcars_8 |>
#'   gt::gt() |>
#'   gt::tab_spanner(
#'     label = "Performance",
#'     columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
#'   )
#'
#' ordered_data <- tab_with_spanner |>
#'   create_ordered_data()
#'
#' write_spanners(
#'   gt_table = tab_with_spanner,
#'   ordered_gt_data = ordered_data,
#'   wb = wb,
#'   sheet_name = "one_spanner",
#'   row_to_start = 1
#' )
write_spanners <- function(gt_table, ordered_gt_data, wb, sheet_name, row_to_start) {
  spanner_helper <- create_spanner_helper(gt_table, ordered_gt_data)

  row_to_start_spanner <- row_to_start
  for (i in 1:length(spanner_helper)) {
    spanner_helper[[i]] |>
      purrr::map(~ write_one_spanner(wb,
        sheet = sheet_name,
        spanner = .x,
        row = row_to_start_spanner,
        style = column_labels_border
      ))

    row_to_start_spanner <- row_to_start_spanner + 1
  }
}
