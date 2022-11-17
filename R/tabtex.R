
#' Format and generate a LaTeX-formatted table from a data frame
#'
#' @param .data A data frame or data frame extension (e.g., a tibble).
#' @param out A character string that specifies the path of the output tex file.
#' @param title A character string that specifies the title for the table.
#' @param label A character string that specifies the LaTeX label (e.g., "tab:table1") for the table to be used in LaTeX references.
#' @param width A character string specifying the width of the table (by default, 0.8\linewidth, 80% of the line width).
#' @param position A character string containing only elements of "h", "t", "b", "p", and "!" that dictate where the table is placed in the LaTeX document.
#' @param note A character string containing the note to place underneath the table.
#' @param note_width A character string that specifies the width of the note. By default, the note will be the same width as the table itself (specified with `width`).
#' @param note_label A character string containing the label preceding the note. By default, "Note:" in italics (\textit{Note:}).
#' @param long_negatives A logical value indicating whether negative signs preceding numbers in cells should be converted to mathematical negative signs ("$-$") in the LaTeX output.
#' @param numbered A logical value indicating whether the table should be numbered in LaTeX (if `TRUE`, uses the `table` environment; if `FALSE`, uses the `table*` environment instead).
#' @param special_left A logical value indicating whether the leftmost column should be formatted separately from the other columns (`TRUE` by default).
#'
#' @export
#'
#' @examples
#' # create a LaTeX-formatted table to display the mtcars dataset
#' tabtex(mtcars)
#'
#' # reformat the mtcars dataset and save it as a LaTeX table
#' mtcars
#' cars <- mtcars[, c("mpg", "hp", "wt", "am")]
#' cars$am[cars$am == 1] <- "Manual"
#' cars$am[cars$am == 0] <- "Automatic"
#' names(cars) <- c("MPG", "Horsepower", "Weight (1000 lbs)", "Transmission")
#' 
#' tabtex(cars, out = "simple_cars.tex")
#' 
#' # do similar reformatting with dplyr but add a note specifying that weight is in 1000s of pounds
#' library(dplyr)
#' 
#' cars <- mtcars %>%
#'   mutate(am = ifelse(am == 1, "Manual", "Automatic")) %>%
#'   select("MPG" = mpg, "Horsepower" = hp, "Weight" = wt, "Transmission" = am) %>%
#'   tabtex(out = "dplyr_cars.tex", note = "Weight is measured in thousands of pounds.")

tabtex <- function(.data, 
                   out,
                   title, 
                   label,
                   width = "0.8\\linewidth",
                   position = "!htbp",
                   note,
                   note_width,
                   note_label = TRUE,
                   long_negatives = TRUE,
                   numbered = TRUE,
                   special_left = TRUE) {
  
  ## Define local strings
  localStrings <- c()
  localStrings["table_caption"] <- title
  localStrings["table_label"] <- label
  localStrings["table_width"] <- width
  localStrings["table_pos"] <- position
  localStrings["table_note"] <- note
  
  if (missing(note_width)) {
    localStrings["note_width"] <- width
  } else {
    localStrings["note_width"] <- note_width
  }
  
  if (!missing(note) & note_label) {
    localStrings["note_start"] <- "\\textit{Note:}"
  } else {
    localStrings["note_start"] <- ""
  }
  
  ## Build table
  if (numbered) {
    table <- "\\begin{table}[$table_pos]\n\\centering\n"
  } else {
    table <- "\\begin{table*}[$table_pos]\n\\centering\n"
  }
  
  # Add table caption (only if requested)
  if (!missing(title)) {
    table <- paste0(table, "\\caption{$table_caption}\n")
  }
  
  # Add the label (only if requested)
  if (!missing(label)) {
    table <- paste0(table, "\\label{$table_label}")
  }
  
  # Add tabular call
  if (special_left) {
    localStrings["table_cols"] <- ncol(.data) - 1
    table <- paste(table, "\\begin{tabular*}{$table_width}{@{\\extracolsep{\\fill}}l*{$table_cols}{c}}",
                   sep = "\n")
  } else {
    localStrings["table_cols"] <- ncol(.data)
    table <- paste(table, "\n", "\\begin{tabular*}{$table_width}{*{$table_cols}{c}}")
  }
  
  # Add hlines between title (caption) and actual content from data frame
  table <- paste0(table, "\n", "\\hline\\hline")
  
  # TK return to this to deal with what happens if (a) a character vector is passed for column headers or (b) a named character vector is passed
  # if (!names_are_headers) {
  #   names(.data) <- as.vector(.data[1, ])
  #   .data <- .data[2:nrow(.data), ]
  # }
  
  # Add column headers
  table <- paste0(table, "\n",
                  paste(names(.data), collapse = " & "),
                  "\\\\\n\\hline")
  
  # Fill in values for each cell in the table
  for (row in 1:nrow(.data)) {
    # Start new line
    table <- paste0(table, "\n", .data[row, 1])
    
    for (column in 2:ncol(.data)) {
      table <- paste(table, .data[row, column], sep = " & ")
    }
    # Create the next row
    table <- paste0(table, "\\\\")
  }
  
  # Add two horizontal lines at end of table and close the tabular
  table <- paste0(table, "\n\\hline\\hline\n\\end{tabular*}")
  
  # Add note (only if note is requested)
  if (!missing(note)) {
    table <- paste0(table, "\n\\begin{minipage}{$note_width}\n$note_start$table_note\n\\end{minipage}")
  }
  
  # Close table environment
  if (numbered) {
    table <- paste0(table, "\n", "\\end{table}")
  } else {
    table <- paste0(table, "\n", "\\end{table*}")
  }
  
  # Replace local strings
  for (strName in names(localStrings)) {
    table <- gsub(paste0("$", strName), localStrings[strName], table, fixed = TRUE)
  }
  
  # Fix negative numbers
  if (long_negatives) {
    table <- gsub("-[^0-9]*", "$-$", table)
  }
  
  if (missing(out)) {
    table
  } else if (tools::file_ext(out) != "tex") {
    # Fix file extension if not being saved as a .tex file
    write(table, file = paste0(tools::file_path_sans_ext(out), ".tex"))
  } else {
    write(table, file = out)
  }
}
