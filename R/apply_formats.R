#' @inheritParams write_stub_and_table_body
#' Heavily copied from `gt:::render_formats`
apply_formats <- function(gt_table,
                          ordered_data) {

  if (length(gt_table$`_formats`) == 0) {
    return(ordered_data)
  }

  for (fmt in seq_along(gt_table$`_formats`))  {
    # from `gt:::render_formats`
    # Determine if the formatting function has a function relevant to
    # the context; if not, use the `default` function (which should
    # always be present)
    # from `gtopenxlsx` alwyas using default
    eval_func <- "default"

    # Obtain compatibility information for the formatting function
    compat <- gt_table$`_formats`[[fmt]]$compat

    # Get the rows to which the formatting should be constrained
    rows <- gt_table$`_formats`[[fmt]]$rows

    for (col in gt_table$`_formats`[[fmt]][["cols"]]) {
      # from `gt:::render_formats`
      # Perform rendering but only do so if the column is present
      # Or if we are confident that we have a compatible formatter and no rows /cols are hidden

      # from gtopenxlsx The checks if columns and format are compatible are not done, it is expecting that the
      # gt table is rendering.

        # Omit rows that are not present in the `data_tbl` object
        rows <- base::intersect(seq_len(nrow(ordered_data)), rows)

        result <- gt_table$`_formats`[[fmt]]$func[[eval_func]](ordered_data[[col]][rows])

        # If any of the resulting output is `NA`, that means we want
        # to NOT make changes to those particular cells' output
        # (i.e. inherit the results of the previous formatter).
        ### from gtopenxlsx forcing the new col to change mode/type
        mode(ordered_data[[col]]) <- mode(result)
        ordered_data[[col]][rows][!is.na(result)] <- result[!is.na(result)]

    }
  }
  return(ordered_data)
}
