#' Check cell counts for categorical variables
#'
#' @param data A data frame or tibble to be used
#' @param limit The value for k as an integer
#' @param col The variable to be checked in Tidyverse notation
#'
#' @returns Data frame containing all variable categories with counts less than a specified limit
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data <- data.frame(
#'  A = sample(c(1, 2, 3), 100, replace = TRUE),
#'  B = sample(c(4, 5), 100, replace = TRUE),
#'  C = sample(c(6, 7, 8), 100, replace = TRUE)
#' )
#' check_count(data, 5, A)
check_count <- function(data, limit, col) {
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
  col_quo <- rlang::enquo(col)
  # Capture error from incorrect cols and return more helpful error message
  selected_data <- rlang::try_fetch(
    data |> dplyr::select(!!col_quo),
    error = function(cnd) {
      cli::cli_abort(c(
        "The supplied column must exist in {.var data}",
        "x" = "You supplied the column name {rlang::f_text(col_quo)}",
        "i" = "{.var data} contains the columns {names(data)}"
      ),
      parent = cnd)
    }
  )

  # Tabulate and retrieve all cells that fall below the limit
  low_count_cells <- as.data.frame(table(selected_data)) |>
    dplyr::filter(.data[["Freq"]] < limit) # nolint: object_usage_linter.

  # Exit early if no cells fall below the threshold
  if (length(low_count_cells) == 0) {
    message("All variable combinations are at or above the supplied threshold")
    return(NULL)
  }

  return(low_count_cells)
}