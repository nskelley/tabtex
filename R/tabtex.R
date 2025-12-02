#' Format and generate a LaTeX-formatted table from a data frame
#'
#' @param .data A data frame or data frame extension (e.g., a tibble).
#' @param ... Additional arguments (to be implemented).
#'
#' @return A character string containing LaTeX-formatted table code, or writes to file if `out` is specified.
#' @export
#'
#' @examples
#' # TODO: Add examples once implementation is complete
#' # tabtex(mtcars)
tabtex <- function(.data, 
                   out = NULL,
                   digits = 3L,
                   headings,
                   blank_headings = FALSE,
                   special_left = TRUE) {
  
  ## Transform data for digits (when applicable)
  if (!missing(digits) & as.integer(digits) == digits) {
    digitized.cols <- lapply(.data, function(col) {
      if (is.numeric(col) & digits >= 0) {
        return(format(round(col, digits = digits), nsmall = digits))
      } else {
        return(col)
      }
    })
    .data <- do.call(cbind, digitized.cols)
  }
  
  table <- ""
  
  # Add tabular call
  .ncols <- ncol(.data)
  if (special_left) {
    .ncols <- .ncols - 1
    table <- paste(table, paste0("\\begin{tabular}{@{\\extracolsep{",
                                 "\\fill}}l*{", .ncols, "}{c}}"),
                   sep = "\n")
  } else {
    table <- paste(table, "\n", paste0("\\begin{tabular*}{$table_width}",
                                       "{*{", .ncols, "}{c}}"))
  }
  
  # Add hlines between title (caption) and actual content from data frame
  table <- paste0(table, "\n", "\t\\hline\\hline")
  
  headingsToUse <- names(.data)
  
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
  
  print(headingsToUse)
  # If requested, fill in blank headings with column names
  if (!blank_headings) {
    emptyHeadings <- which(is.na(headingsToUse) | headingsToUse == "")
    headingsToUse[emptyHeadings] <- names(.data)[emptyHeadings]
  }
  
  headingsToUse[which(is.na(headingsToUse))] <- ""
  
  # Add column headers
  table <- paste0(table, "\n\t",
                  paste(headingsToUse, collapse = " & "),
                  " \\\\\n\t\\hline")
  
  # Fill in values for each cell in the table
  .startc <- ifelse(special_left, 2, 1)
  for (row in 1:nrow(.data)) {
    # Start new line
    if (special_left) {
      table <- paste0(table, "\n\t", .data[row, 1])
    } else {
      table <- paste0(table, "\n\t")
    }
    
    for (column in .startc:ncol(.data)) {
      if (is.numeric(.data[, column])) {
        table <- paste(table, paste0("$", .data[row, column], "$"), 
                       sep = " & ")
      } else {
        table <- paste(table, .data[row, column], sep = " & ")
      }
    }
    # Create the next row
    table <- paste0(table, " \\\\")
  }
  
  # Add two horizontal lines at end of table and close the tabular
  table <- paste0(table, "\n\t\\hline\\hline\n\\end{tabular}")
  
  if (!missing(out)) {
    if (!(tools::file_ext(out) %in% c("tex", ""))) {
      # Fix file extension if not being saved as a .tex file
      fixed_filename <- paste0(tools::file_path_sans_ext(out), ".tex")
      warning(paste0("Extension ", tools::file_ext(out), 
                     " is not valid. Saving as ", fixed_filename, " instead."))
      write(table, file = fixed_filename)
    } else if (tools::file_ext(out) == "") {
      write(table, file = paste0(out, ".tex"))
    } else {
      write(table, file = out)
    }
    message(paste0("Table saved as ", tools::file_path_as_absolute(out)))
  }
}
