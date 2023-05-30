
#' Format and generate a LaTeX-formatted table from a data frame
#'
#' @param .data A data frame or data frame extension (e.g., a tibble).
#' @param out A character string that specifies the path of the output tex file.
#' @param title A character string that specifies the title for the table.
#' @param label A character string that specifies the LaTeX label (e.g., "tab:table1") for the table to be used in LaTeX references.
#' @param width A character string specifying the width of the table (by default, 0.8\\linewidth, 80% of the line width).
#' @param position A character string containing only elements of "h", "t", "b", "p", and "!" that dictate where the table is placed in the LaTeX document.
#' @param digits A non-negative integer value of the desired number of digits to show after the decimal in the table.
#' @param note A character string containing the note to place underneath the table.
#' @param note_width A character string that specifies the width of the note. By default, the note will be the same width as the table itself (specified with `width`).
#' @param note_label A logical value that specifies whether the table note should be preceded by "Note:" in italics, when applicable.
#' @param headings A character vector that specifies the column headers for the table. When not specified, the data frame's column names will be used instead. When a named character vector, column headers will be the values of the vector whose names correspond to a column name in the original data frame. When a column from the data frame is not represented in the named character vector, the column header will be blank, unless `blank_headings` is changed to `FALSE`
#' @param blank_headings A logical value dictating whether column headers can be empty. If `TRUE`, columns left unspecified by the character vector passed as `headings` are blank. If `FALSE`, unspecified column headers are filled using the original data frame's column names.
#' @param long_negatives A logical value indicating whether negative signs preceding numbers in cells should be converted to mathematical negative signs ("$-$") in the LaTeX output.
#' @param numbered A logical value indicating whether the table should be numbered in LaTeX (if `TRUE`, uses the `table` environment; if `FALSE`, uses the `table*` environment instead).
#' @param special_left A logical value indicating whether the leftmost column should be formatted separately from the other columns (`TRUE` by default).
#' @param suppress_savemsg A logical value for whether to suppress the message that confirms (a) that the table was saved and (b) the absolute filepath of that save.
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
                   digits = 3,
                   note,
                   note_width,
                   note_label = TRUE,
                   headings,
                   blank_headings = TRUE,
                   long_negatives = TRUE,
                   numbered = TRUE,
                   special_left = TRUE,
                   suppress_savemsg = FALSE) {
  
  ## Define local strings
  localStrings <- c()
  localStrings["table_caption"] <- ifelse(missing(title), "", title)
  localStrings["table_label"] <- ifelse(missing(label), "", label)
  localStrings["table_width"] <- width
  localStrings["table_pos"] <- position
  localStrings["table_note"] <- ifelse(missing(note), "", note)
  
  if (missing(note_width)) {
    localStrings["note_width"] <- width
  } else {
    localStrings["note_width"] <- note_width
  }
  
  if (!missing(note) & note_label) {
    localStrings["note_start"] <- "\\textit{Note: }"
  } else {
    localStrings["note_start"] <- ""
  }
  
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
  
  # Add column headers
  table <- paste0(table, "\n",
                  paste(headingsToUse, collapse = " & "),
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
    table <- gsub("-(?=\\d)", "$-$", table, perl = TRUE)
  }
  
  if (missing(out)) {
    message(table)
    table
  } else if (!(tools::file_ext(out) %in% c("tex", ""))) {
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
  
  if (!missing(out) & !suppress_savemsg) {
    message(paste0("Table saved as ", tools::file_path_as_absolute(out)))
  }
}
