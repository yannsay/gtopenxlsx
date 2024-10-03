title_style <- openxlsx::createStyle(halign = "center", border = "TopLeftRight", borderColour = "#D3D3D3")
column_labels_border <- openxlsx::createStyle(border = "TopLeftRight", borderColour = "#D3D3D3")
percent_style <- openxlsx::createStyle(numFmt = "0%")
sourcenote_style <- openxlsx::createStyle(fontSize = 10)

border_colour <- openxlsx2::wb_color(hex = "D3D3D3")
apply_title_style <- function(wb, sheet_name, rows, cols) {
  dims_to_style <- openxlsx2::wb_dims(rows = rows, cols = cols)
  wb <- wb |>
    openxlsx2::wb_add_border(sheet = sheet_name,
                             dims = dims_to_style,
                             top_border = "thin", top_color = border_colour,
                             right_border = "thin", right_color = border_colour,
                             bottom_border = "",
                             left_border = "thin", left_color = border_colour) |>
    openxlsx2::wb_add_cell_style(sheet = sheet_name,
                                 dims = dims_to_style,
                                 horizontal = "center")
  return(wb)

}

# title_style <- openxlsx::createStyle(halign = "center", border = "TopLeftRight", borderColour = "#D3D3D3")
# openxlsx::addStyle(wb, sheet_name, rows = current_row, cols = 1:total_cols, style = title_style)
