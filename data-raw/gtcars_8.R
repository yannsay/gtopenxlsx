## code to prepare `gtcars_8` dataset goes here
gtcars_8 <-
  gt::gtcars |>
  dplyr::group_by(ctry_origin) |>
  dplyr:: slice_head(n = 2) |>
  dplyr::ungroup() |>
  dplyr::filter(ctry_origin != "United Kingdom")
usethis::use_data(gtcars_8, overwrite = TRUE)
