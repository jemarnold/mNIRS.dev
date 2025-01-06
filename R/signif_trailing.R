#' Print a numeric value as character with trailing zeroes
#'
#' Converts a numeric vector to a character vector to preserve trailing zeroes for display.
#'
#' @param x A numeric vector.
#' @param digits An integer scalar specifying the number of digits to preserve.
#' @param format Indicates how to treat `digits`. Either the desired number of digits after
#' the decimal point (`format = "f"`) or significant digits (`format = "fg"`).
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' signif_trailing(c(1, 1.3, 1.35), digits = 2, format = "f")
#' signif_trailing(c(1, 1.3, 1.35), digits = 2, format = "fg")
signif_trailing <- function(x, digits = 2, format = c("f", "fg")) {
    ## format = "f" for decimal place; "fg" for signif
    format = match.arg(format)

    ## argument validation
    if (!rlang::is_double(x)) {
        cli::cli_abort("{.arg x} must be a numeric vector.")
    }
    if (!rlang::is_integerish(digits)) {
        cli::cli_abort("{.arg digits} must be an integer scalar")
    }

    ## remove trailing `.` or "NA"
    ## https://stackoverflow.com/a/35280610/15389854
    gsub("\\.$|NA", "",
         formatC(
             x, digits = digits, format = format, flag = "#"))
}

