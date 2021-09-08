
#' Calculate Gompertz-Makeham Force of Motion
#'
#' \loadmathjax{}
#' Calculates \mjseqn{\mu(t) = A + R e^{\alpha t}}
#'
#' @param A \code{numeric scalar} corresponding to the \mjseqn{A} parameter
#' @param R \code{numeric scalar} corresponding to the \mjseqn{R} parameter
#' @param alpha \code{numeric scalar} corresponding to the \mjseqn{\alpha} parameter
#' @param t \code{numeric scalar or vector} corresponding to the age(s) \mjseqn{t}
#'
#' @return Returns a \code{numeric scalar} giving the Gompertz-Makeham force
#' of mortality \mjseqn{\mu(t)}
#' @export
#'
#' @examples
#'
#' # You can use a single age:
#' force_of_mortality_GM(A = 0.005,
#'                       R = 0.00005,
#'                       alpha = 0.0093,
#'                       t = 10)
#'
#' # Or a vector of ages:
#' force_of_mortality_GM(A = 0.005,
#'                       R = 0.00005,
#'                       alpha = 0.0093,
#'                       t = 0:100)
#'
force_of_mortality_GM = function(A, R, alpha, t){

  # Fail gracefully if the parameters are not scalars
  stop_if_not_scalar(A)
  stop_if_not_scalar(R)
  stop_if_not_scalar(alpha)

  # Calculate the force of motion
  A + R * exp(alpha * t)

}

#' Calculate Gompertz-Makeham Force of Motion
#'
#' \loadmathjax{}
#' Calculates \mjseqn{S(t) = -At - \frac{R}{\alpha} (\exp(\alpha t) - 1)  }
#'
#' @inheritParams force_of_mortality_GM
#'
#' @return Returns a \code{numeric scalar} giving the Gompertz-Makeham force
#' of mortality \mjseqn{\mu(t)}
#' @export
#'
#' @examples
#'
#' # You can use a single age:
#' survival_function_GM(A = 0.005,
#'                      R = 0.00005,
#'                      alpha = 0.0093,
#'                      t = 10)
#'
#' # Or a vector of ages:
#' survival_function_GM(A = 0.005,
#'                      R = 0.00005,
#'                      alpha = 0.0093,
#'                      t = 0:100)
#'
survival_function_GM = function(A, R, alpha, t){

  # Fail gracefully if the parameters are not scalars
  stop_if_not_scalar(A)
  stop_if_not_scalar(R)
  stop_if_not_scalar(alpha)

  # Calculate the survival probability
  exp(-(A*t) - (R/alpha) * (exp(alpha*t) - 1) )

}




