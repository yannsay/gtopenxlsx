test_that("apply_format works with fmt_currency", {
  fmt_currency_example <-
    gtcars_8 |>
    gt::gt() |>
    gt::fmt_currency(columns = msrp, decimals = 0)

  ordered_table <- fmt_currency_example |>
    create_ordered_data()

  actual_output <- apply_formats(
    fmt_currency_example,
    ordered_table
  )

  expected_results <- actual_output$msrp |>
    formatC(format = "s") |>
    trimws()
  expect_equal(actual_output$msrp, expected_results)
})
