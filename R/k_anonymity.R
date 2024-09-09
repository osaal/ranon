#' Check multiple categorical variables for k-anonymity
#' 
#' @param data A data frame or tibble to be used
#' @param limit The value for k as an integer
#' @param ... Two or more categorical variables using Tidyverse syntax (see Details)
#' 
#' @returns Data frame containing all unique combinations whose counts fall under the supplied limit
#' @export
#' 
#' @details
#' The `...` parameter supports two or more categorical variables supplied using Tidyverse notation. This means full support for `:`, `!`, `&`, `|` and `c()`.
#' @examples 
#' data <- data.frame(
#'  A = sample(c(1, 2, 3), 100, replace = TRUE),
#'  B = sample(c(4, 5), 100, replace = TRUE),
#'  C = sample(c(6, 7, 8), 100, replace = TRUE)
#' )
#' # Explicitly specify variables
#' k_anonymisation(data, 5, A, B, C)
#' # Using quosure slicing
#' k_anonymisation(data, 5, A:C)
#' # Using negation
#' k_anonymisation(data, 5, !c(A:B))
k_anonymisation <- function(data, limit, ...) {
  # Select data according to variables
  cols <- quos(...)
  selected_data <- data |> dplyr::select(!!!cols)
  
  # Cross-tabulate selected variables
  crosstab <- as.data.frame(table(selected_data))
  
  # Retrieve all unique combinations that fall below the k limit
  low_count_cells <- crosstab |>
    dplyr::filter(.data$Freq < limit)
  
  # Exit early if no cells fall below the threshold
  if (length(low_count_cells) == 0) {
    message("All variable combinations are at or above the supplied threshold")
    return(NULL)
  }
  
  return(low_count_cells)
}