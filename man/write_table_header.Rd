% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_table_header.R
\name{write_table_header}
\alias{write_table_header}
\title{Write table headers}
\usage{
write_table_header(gt_table, ordered_gt_data, wb, sheet_name, row_to_start)
}
\arguments{
\item{gt_table}{A gt table}

\item{ordered_gt_data}{ordered data from `create_ordered_data`}

\item{wb}{Name of Workbook object like `openxlsx::createWorkbook()`}

\item{sheet_name}{name of the sheet in the workbook}

\item{row_to_start}{Number of the row where to start writting the stub and body.}
}
\value{
Nothing. It will update the wb object with table headers.
}
\description{
Write table headers
}
