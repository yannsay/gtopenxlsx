missing_val_token <- "::missing_val::"

#' @inheritParams write_stub_and_table_body
#' Heavily copied from `gt:::perform_col_merge`
apply_col_merge <- function(gt_table,
                             ordered_data){


  if (length(gt_table$`_col_merge`) == 0) {
    return(ordered_data)
  }

  for (i in seq_along(gt_table$`_col_merge`)) {
    type <- gt_table$`_col_merge`[[i]]$type

    type <- rlang::arg_match0(
      type,
      c("merge", "merge_range", "merge_uncert", "merge_n_pct")
    )

    if (type == "merge") {
      mutated_column <- gt_table$`_col_merge`[[i]]$vars[[1]]

      data_tbl<-gt_table$`_data` #should be the inital column

      columns <- gt_table$`_col_merge`[[i]][["vars"]]
      rows <- gt_table$`_col_merge`[[i]][["rows"]]
      pattern <- gt_table$`_col_merge`[[i]][["pattern"]]

      glue_src_na_data <- lapply(as.list(data_tbl[rows, columns]), FUN = is.na)

      glue_src_data <- as.list(data_tbl[rows, columns])

      glue_src_data <-
        lapply(
          seq_along(glue_src_data),
          FUN = function(x) {

            # The source data (and 'source data' here means data that's already
            # been formatted and converted to `character`) having a character
            # `"NA"` value signals that the value should *probably* be treated
            # as missing (we are relatively certain it wasn't modified by
            # `sub_missing()`, a case where we consider the value *not* to be
            # missing because it was handled later) but we also want to
            # corroborate that with the original data values (checking for true
            # missing data there)

            missing_cond <- glue_src_data[[x]] == "NA" & glue_src_na_data[[x]]
            missing_cond[is.na(missing_cond)] <- TRUE

            glue_src_data[[x]][missing_cond] <- missing_val_token
            glue_src_data[[x]]
          }
        )

      glue_src_data <- stats::setNames(glue_src_data, seq_len(length(glue_src_data)))

      glued_cols <- as.character(glue_gt_gtopenxlsx(glue_src_data, pattern))

      if (grepl("<<.*?>>", pattern)) {

        glued_cols <-
          vapply(
            glued_cols,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE,
            FUN = resolve_secondary_pattern_gtopenxlsx
          )

        glued_cols <- gsub("<<|>>", "", glued_cols)
      }

      glued_cols <- gsub(missing_val_token, "NA", glued_cols, fixed = TRUE)

      ordered_data[[mutated_column]] <-ordered_data[[mutated_column]] |>  as.character()
      ordered_data[rows, mutated_column] <- glued_cols

    }
  }
  return(ordered_data)
  }
