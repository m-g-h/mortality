#' Test if an object is a scalar
#'
#' @param x Object to test
#'
#' @return Displays an error message informing that x should be a scalar.
#'
#' @keywords Internal
#'
#' x = 1:10
#' stop_if_not_scalar(x)
#'
stop_if_not_scalar <- function(x){

  len = length(x)

  is_not_scalar = !(is.atomic(x) && len == 1L)

  if(is_not_scalar){
    stop(paste0("Argument `",
                deparse(substitute(x)),
                "` is a parameter that has to be a scalar but is of length ",
                len,
                "."))
  }

}

#' Upper incomplete gamma distribution
#'
#' @description
#' \loadmathjax{}
#' Calculates
#' \mjsdeqn{\int_x^\infty t^{u-1} \exp(-t) dt}
#'
#' using numerical integration via \code{\link{integrate}}.
#'
#' @param u \code{Numeric scalar} corresponding to \mjseqn{u}
#' @param x \code{Numeric scalar or vector} corresponding to \mjseqn{x}
#'
#' @return Returns a \code{numeric scalar}
#' @export
#'
#' @importFrom stats integrate
#'
#' @examples
#'
#' upper_inc_gamma(-0.005, 2)
#'
upper_inc_gamma = function(u, x){

  # Fail gracefully if arguments are not scalars
  stop_if_not_scalar(u)

  # Non-vectorised integration function
  intfun = function(u, x){
    # Perform the integration using an anonymous function
    integrate(f = function(t) t^(u-1) * exp(-t),
              lower = x,
              upper = Inf
    )$value
  }

  # Vectorised function
  vecfun = Vectorize(intfun,
            vectorize.args = c("x"))

  # Compute the output
  vecfun(u, x)
}


#' Test if an package is installed and fail else
#'
#' @param package \code{string scalar} Giving the package to test for
#'
#' @return Displays an error message indicating that the package should be installed
#'
#' @keywords Internal
#'
stop_if_package_is_missing = function(package){
  # Check if package is installed
  package_installed = requireNamespace(package, quietly = TRUE)

  # Stop and display error message
  if (!package_installed){
    stop("Package ", package,
         ' is required for this function. Install it e.g. via `install.packages("',
         package,
         '")`.')
  }
}
