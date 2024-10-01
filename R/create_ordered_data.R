#' Order the data to be written
#'
#' @inheritParams gt_to_xlsx
#'
#' @return dataframe with the columns in correct order or removed.
#' @export
#'
#' @examples
#' example_0 <- gt::gtcars |>
#'   gt::gt()
#'
#' gtcars_example <- example_0 |>
#'   create_ordered_data()
create_ordered_data <- function(gt_table) {
  ordered_data <- gt_table$`_data`[, gt_table$`_boxhead`$var]

  # dealing with group by
  if (!rlang::is_empty(gt_table$`_row_groups`)) {
    group_vars <- gt_table$`_boxhead` |>
      dplyr::filter(type == "row_group") |>
      dplyr::pull(var)
    ordered_data <- ordered_data |>
      dplyr::select(-tidyr::all_of(group_vars))
  }

  # dealing with stub
  rowname_column <- gt_table$`_boxhead` |>
    dplyr::filter(type == "stub") |>
    dplyr::pull(var)

  if (!rlang::is_empty(rowname_column)) {
    ordered_data <- ordered_data |>
      dplyr::relocate(
        !!rlang::sym(rowname_column),
        .before = rlang::sym(names(ordered_data)[[1]])
      )
  }

  # dealing with hidden columns
  hidden_column <- gt_table$`_boxhead` |>
    dplyr::filter(type == "hidden") |>
    dplyr::pull(var)

  if (!rlang::is_empty(hidden_column)) {
    ordered_data <- ordered_data |>
      dplyr::select(-tidyr::all_of(hidden_column))
  }

  return(ordered_data)
}
