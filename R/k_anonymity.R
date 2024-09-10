#' Check multiple categorical variables for k-anonymity
#'
#' @param data A data frame or tibble to be used
#' @param limit The value for k as an integer
#' @param ... Two or more categorical variables using Tidyverse syntax (see Details)
#'
#' @returns Data frame containing all unique combinations whose counts fall under the supplied limit. Returns `NULL` if all combinations are above the limit.
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
#' k_anonymity(data, 5, A, B, C)
#' # Using quosure slicing
#' k_anonymity(data, 5, A:C)
#' # Using negation
#' k_anonymity(data, 5, !c(A:B))
k_anonymity <- function(data, limit, ...) {
  # Argument checks
  if (!(checkmate::check_data_frame(data) || checkmate::check_tibble(data))) {
    cli::cli_abort(c(
      "{.var data} must be a data frame or tibble",
      "x" = "You supplied the type {.cls class(data)}"
    ))
  }
  if (!(checkmate::check_int(limit))) {
    cli::cli_abort(c(
      "{.var limit} must be a single integer",
      "x" = "You supplied the type {.cls class(limit)}"
    ))
  }

  # Select data according to variables
  cols <- dplyr::quos(...)

  # Capture error from incorrect cols and return more helpful error message
  selected_data <- rlang::try_fetch(
    data |> dplyr::select(!!!cols),
    error = function(cnd) {
      cli::cli_abort(c(
        "All supplied columns must exist in {.var data}",
        "X" = "You supplied the column names {sapply(cols, rlang::f_text)}",
        "i" = "{.var data} contains the columns {names(data)}",
        "i" = "Tip: Negated columns must also exist in the data"
      ),
      parent = cnd)
    }
  )

  # Crosstabulate and retrieve all unique combinations that fall below the limit
  low_count_cells <- as.data.frame(table(selected_data)) |>
    dplyr::filter("Freq" < limit)

  # Exit early if no cells fall below the threshold
  if (length(low_count_cells) == 0) {
    message("All variable combinations are at or above the supplied threshold")
    return(NULL)
  }

  return(low_count_cells)
}