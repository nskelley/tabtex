#' Format and generate a Markdown-formatted table from a data frame
#'
#' @param .data A data frame or data frame extension (e.g., a tibble).
#' @param out A character string that specifies the path of the output .Md file. If missing (default), the result is merely printed to the console.
#' @param headings A character vector that specifies the column headers for the table. When not specified, the data frame's column names will be used instead. When a named character vector, column headers will be the values of the vector whose names correspond to a column name in the original data frame. When a column from the data frame is not represented in the named character vector, the column header will be blank, unless `blank_headings` is changed to `FALSE`
#' @param blank_headings A logical value dictating whether column headers can be empty. If `TRUE`, columns left unspecified by the character vector passed as `headings` are blank. If `FALSE`, unspecified column headers are filled using the original data frame's column names.
#' @param suppress_savemsg A logical value for whether to suppress the message that confirms (a) that the table was saved and (b) the absolute filepath of that save.
#'
#' @export

tabdown <- function(.data, 
                   out,
                   headings,
                   blank_headings = TRUE,
                   suppress_savemsg = FALSE) {
  # Column headers
  headingsToUse <- vector(mode = "character", length = ncol(.data))
  
  if (missing(headings)) {
    headingsToUse <- names(.data)
  } else if (is.null(names(headings))) {
    # Unnamed vector case
    for (index in 1:length(headingsToUse)) {
      headingsToUse[index] <- headings[index]
    }
  } else {
    # Named vector case
    for (name in names(headings)) {
      headingsToUse[which(names(.data) == name)] <- headings[name]
    }
  }
  
  # If requested, fill in blank headings with column names
  if (!blank_headings) {
    emptyHeadings <- which(is.na(headingsToUse) | headingsToUse == "")
    headingsToUse[emptyHeadings] <- names(.data)[emptyHeadings]
  }
  
  headingsToUse[which(is.na(headingsToUse))] <- ""
  
  # Table column headers
  table <- paste0("| ", paste(headingsToUse, collapse = " | "), " |")
  table <- paste0(table, "\n|", paste0(rep(" --- |", ncol(.data)), collapse = ""))
  
  # Fill in cell values
  for (row in 1:nrow(.data)) {
    # Start a new line
    table <- paste0(table, "\n|")
    
    for (column in 1:ncol(.data)) {
      table <- paste(table, .data[row, column], "|")
    }
  }
  
  if (missing(out)) {
    message(table)
  } else if (!(tools::file_ext(out) %in% c("txt", "Md", ""))) {
    # Fix file extension if not being saved as a plaintext or Markdown file
    fixed_filename <- paste0(tools::file_path_sans_ext(out), ".Md")
    warning(paste0("Extension ", tools::file_ext(out), 
                   " is not valid. Saving as ", fixed_filename, " instead."))
    write(table, file = fixed_filename)
  } else if (tools::file_ext(out) == "") {
    write(table, file = paste0(out, ".Md"))
  } else {
    write(table, file = out)
  }
  
  if (!missing(out) & !suppress_savemsg) {
    message(paste0("Table saved as ", tools::file_path_as_absolute(out)))
  }
}
