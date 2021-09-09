#' Test if an object is a scalar
#'
#' @param x Object to test
#'
#' @return Returns a {Logical scalar} with value \code{TRUE} if the object is a
#'  scalar
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
#' @param x \code{Numeric scalar} corresponding to \mjseqn{x}
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
  stop_if_not_scalar(x)

  # Perform the integration using an anonymous function
  integrate(f = function(t) t^(u-1) * exp(-t),
            lower = x,
            upper = Inf
            )$value
}
