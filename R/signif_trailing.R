#' Print a numeric value as character with trailing zeroes
#'
#' Converts a numeric to a character object to preserve trailing zeroes
#' for display.
#'
#' @param x A numeric vector.
#' @param digits An integer scalar specifying the number of digits to preserve.
#' @param format Indicates how to treat `digits`. Either the desired number of
#' digits after the decimal point (`format = "digits"`) or significant digits
#' (`format = "signif"`).
#'
#' @return A character vector.
#'
#' @seealso [formatC()], [round()], [signif()]
#'
#' @examples
#' x <- c(1, 1.3, 1.35)
#' signif_trailing(x, digits = 2, format = "digits")
#' signif_trailing(x, digits = 2, format = "signif")
#'
#' @keywords internal
#' @export
signif_trailing <- function(
        x,
        digits = 2,
        format = c("digits", "signif")
) {
    ## format = "digits" for decimal place; "signif" for signif
    format = match.arg(format)
    format <- switch(format,
                     "digits" = "f",
                     "signif" = "fg")

    ## argument validation
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a numeric vector.")
    }
    if (!rlang::is_integerish(digits)) {
        cli::cli_abort("{.arg digits} must be an integer scalar")
    }

    ## remove trailing `.` or "NA"
    ## https://stackoverflow.com/a/35280610/15389854
    gsub("\\.$|NA", "", formatC(x, digits = digits, format = format, flag = "#"))
}
