#' Create a named list of parameters
#'
#' Validates parameters to pass either globally or column-wise to a specified
#' named list
#'
#' @param x Either a vector, a list, or a named list
#' @param names names to assign `x` to
#'
#' @examples
#' assign_to_named_list(x = c("item"), names = c("A", "B"))
#' assign_to_named_list(x = list(c("item1", "item2")), names = c("A", "B"))
#' assign_to_named_list(x = list(c("item1", "item2")), names = c("A", "B"))
#' ## error when `names(x)` and `names` do not match
#' assign_to_named_list(
#'     x = list(A = "item1", B = "item2"),
#'     names = c("A", "B", "C"))
#' assign_to_named_list(
#'     x = list(A = c(x = "item1", y = "item2"), B = "itemB", C = "itemC"),
#'     names = c("A", "B"))
#' assign_to_named_list(
#'     x = list(Q = c(x = "item1", y = "item2"), R = "itemB"),
#'     names = c("A", "B"))
#' assign_to_named_list(
#'     x = list(Q = c(x = "item1", y = "item2"), R = "itemB", S = "itemC"),
#'     names = c("Q", "R", "S"))
#'
#' @return A named list object.
#'
#' @keywords internal
assign_to_named_list <- function(x, names) {

    if (is.list(x) & length(names(x)) == length(x)) {
        if (any(names(x) != names)) {
            cli::cli_abort(paste(
                "Names in {.arg x} do not match {.arg names}"
            ))
        }
        x
    } else if (is.list(x)) {
        setNames(rlang::rep_along(names, x), names)
    } else {
        setNames(rlang::rep_along(names, list(x)), names)
    }
}
