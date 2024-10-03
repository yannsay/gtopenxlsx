# title_style <- openxlsx::createStyle(halign = "center", border = "TopLeftRight", borderColour = "#D3D3D3")
# percent_style <- openxlsx::createStyle(numFmt = "0%")
sourcenote_style <- openxlsx::createStyle(fontSize = 10)

border_colour <- openxlsx2::wb_color(hex = "D3D3D3")
apply_style_title <- function(wb,rows, cols) {
  dims_to_style <- openxlsx2::wb_dims(rows = rows, cols = cols)
  wb <- wb |>
    openxlsx2::wb_add_border(
                             dims = dims_to_style,
                             top_border = "thin", top_color = border_colour,
                             right_border = "thin", right_color = border_colour,
                             bottom_border = "",
                             left_border = "thin", left_color = border_colour) |>
    openxlsx2::wb_add_cell_style(
                                 dims = dims_to_style,
                                 horizontal = "center")
  return(wb)

}

apply_style_column_label <- function(wb,rows, cols) {
  dims_to_style <- openxlsx2::wb_dims(rows = rows, cols = cols)
  wb <- wb |>
    openxlsx2::wb_add_border(
      dims = dims_to_style,
      top_color = border_colour,
      right_color = border_colour,
      bottom_border = "",
      left_color = border_colour)
  return(wb)
}

# column_labels_border <- openxlsx::createStyle(border = "TopLeftRight", borderColour = "#D3D3D3")

