#' Update a model object with Fixed coefficients
#'
#' Re-fit a model with fixed coefficients provided as an argument. Fixed
#' coefficients are not modified when optimising for best fit.
#'
#' @param model An existing model object from `lm`, `nls`, `glm`, and many others.
#' @param ... Additional arguments to define a fixed coefficients.
#'
#' @details
#' If additional arguments are left blank, or if a coefficient does not exist in
#' the model, the model will be returned unchanged.
#'
#' @return A model object.
#'
#' @examples
#' Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
#' SSlogis(Chick.1$Time, Asym = 368, xmid = 14, scal = 6)
#' (model <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1))
#' (model_fixed <- update_fixed_coefs(model, xmid = 20))
#'
#' plot(Chick.1$Time, Chick.1$weight, type = "o", pch = 16,
#'      xlab = "Time", ylab = "weight")
#' lines(Chick.1$Time, fitted(model), col = "red", lwd = 2)
#' lines(Chick.1$Time, fitted(model_fixed), col = "blue", lwd = 2)
#' legend("topleft", legend = c("Observed", "model", "model_fixed"),
#'        col = c("black", "red", "blue"),
#'        pch = c(16, NA, NA),
#'        lwd = c(1, 2, 2))
#'
#' @export
update_fixed_coefs <- function(model, ...) {
    ## Get the arguments to be fixed
    fixed_params <- list(...)
    ## Get the parameter values from the model
    current_params <- as.list(coef(model))
    ## Update parameter values with fixed values from ...
    for (param_name in names(fixed_params)) {
        current_params[[param_name]] <- fixed_params[[param_name]]
    }
    ## Remove fixed parameters from the start list
    start_params <- current_params[!names(current_params) %in% names(fixed_params)]
    ## Construct the updated formula expression
    updated_formula <- do.call(substitute, list(formula(model), fixed_params))
    ## Update the model
    update(object = model,
           formula = updated_formula,
           start = start_params,
           data = dynGet(model$data))
}
