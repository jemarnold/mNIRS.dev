#' Methods for mNIRS.kinetics objects
#'
#' Methods defined for objects returned from [process_kinetics()].
#'
#' @param x object of class `mNIRS.kinetics` as returned from
#' [process_kinetics()]
#' @param ... further arguments passed through, see description of return value
#' for details.
#'
#' @return
#' \describe{
#'   \item{`print`}{Prints the mNIRS kinetics model summary}
#'   \item{`plot`}{Returns a plot of mNIRS kinetics}
#' }
#'
#' @name mNIRS.kinetics-methods


### methods for mNIRS.kinetics objects

#' @rdname mNIRS.kinetics-methods
#' @method print mNIRS.kinetics
#' @export

print.mNIRS.kinetics <- function(x, ...) {
    if (x$method == "monoexponential") {

        cat("\n")
        cat("Monoexponential Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", names(x$data)[2], " ~ SSmonoexp(",
            names(x$data)[1], ", A, B, TD, tau)", sep = "")
        cat("\n")
        if (!is.na(x$data_name)) {
            cat("  data:        ", x$data_name)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ A + (B - A) * (1 - exp((TD - x))/tau)")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")
        cat("  Fit Criteria")
        cat("\n")
        print(round(unlist(x$fit_criteria), 3))
        cat("\n")

    } else if (x$method == "logistic") {

        cat("\n")
        cat("Logistic (Sigmoidal) Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", names(x$data)[2], " ~ SSlogis(",
            names(x$data)[1], ", Asym, xmid, scal)", sep = "")
        cat("\n")
        if (!is.na(x$data_name)) {
            cat("  data:        ", x$data_name)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ Asym / (1 + exp((xmid - x) / scal))")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")
        cat("  Fit Criteria")
        cat("\n")
        print(round(unlist(x$fit_criteria), 3))
        cat("\n")

    } else if (x$method == "half_time") {

        cat("\n")
        cat("Non-parametric Half-Time")
        cat("\n")
        if (!is.na(x$data_name)) {
            cat("  data:        ", x$data_name)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")

    } else {

        cat("\n")
        cat("NO MODEL FOUND")
        cat("\n")

    }

}
