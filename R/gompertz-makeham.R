
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
#' force_of_mortality_GM(t = 0,
#'                       A = 0.00033,
#'                       R = 0.00014,
#'                       alpha = 0.11521)
#'
#' # Or a vector of ages:
#' force_of_mortality_GM(t = 0:100,
#'                       A = 0.00033,
#'                       R = 0.00014,
#'                       alpha = 0.11521
#'                       )
#'
force_of_mortality_GM = function(t, A, R, alpha){

  # Fail gracefully if the parameters are not scalars
  stop_if_not_scalar(A)
  stop_if_not_scalar(R)
  stop_if_not_scalar(alpha)

  # Calculate the force of motion
  A + R * exp(alpha*t)

}

#' Calculate Gompertz-Makeham Force of Motion
#'
#' \loadmathjax{}
#' Calculates \mjseqn{S(t) = -At - \frac{R}{\alpha} (\exp(\alpha t) - 1)  }
#'
#' @inheritParams force_of_mortality_GM
#'
#' @return Returns a \code{numeric scalar} giving the Gompertz-Makeham survival
#' probability \mjseqn{S(t)}
#' @export
#'
#'
#' @examples
#'
#' # You can use a single age:
#' survival_function_GM(t = 0,
#'                      A = 0.00033,
#'                      R = 0.00014,
#'                      alpha = 0.11521)
#'
#' # Or a vector of ages:
#' survival_function_GM(t = 0:100,
#'                      A = 0.00033,
#'                      R = 0.00014,
#'                      alpha = 0.11521)
#'
survival_function_GM = function(t, A, R, alpha){

  # Fail gracefully if the parameters are not scalars
  stop_if_not_scalar(A)
  stop_if_not_scalar(R)
  stop_if_not_scalar(alpha)

  # Calculate the survival probability
  exp(-(A*t) - ((R/alpha) * (exp(alpha*t) - 1) ) )

}

#' Calculate Gompertz-Makeham Life Expectancy
#'
#' @description
#' \loadmathjax{}
#' Calculates
#' \mjsdeqn{ \mathcal{M}_{GM}(t) =  \frac{1}{b} \exp\left( \frac{a e^{bt}}{b}
#'  \right ) \left( \frac{a e^{bx}}{b} \right)^{c/b}
#'  \Gamma\left( - \frac{c}{b}, \frac{a e^{bt}}{b} \right)
#'  }
#' with the upper incomplete gamma function
#' \mjsdeqn{\Gamma(\cdot, \cdot) = \int_x^\infty t^{u-1} e^{-t} dt, \; x>0,
#'  \quad u \in \mathbb{R}
#' }
#' which is implemented in \code{\link{upper_inc_gamma}}.
#'
#' @inheritParams force_of_mortality_GM
#'
#' @return Returns a \code{numeric scalar} giving the Gompertz-Makeham life
#' expectancy \mjseqn{e_{GM}(t)}
#' @export
#'
#' @seealso \code{\link{upper_inc_gamma}}
#'
#' @section Sources:
#'
#' Castellares et al. (2020) - \emph{On closed-form expressions to Gompertz–Makeham
#' life expectancy}. Theoretical Population Biology 134 (August 2020): 53-60.
#' \href{https://doi.org/10.1016/j.tpb.2020.04.005}{https://doi.org/10.1016/j.tpb.2020.04.005}
#'
#' @examples
#'
#' # You can use a single age:
#' life_expectancy_GM(t = 0,
#'                    A = 0.00033,
#'                    R = 0.00014,
#'                    alpha = 0.11521)
#'
#' # Or a vector of ages:
#' life_expectancy_GM(t = 0:100,
#'                    A = 0.00033,
#'                    R = 0.00014,
#'                    alpha = 0.11521)
#'
life_expectancy_GM = function(t, A, R, alpha){

  # Fail gracefully if the parameters are not scalars
  stop_if_not_scalar(A)
  stop_if_not_scalar(R)
  stop_if_not_scalar(alpha)

  # Calculate objects that are repeated in the formula (and coincide with the
  # upper incomplete gamma function parameters)
  gamma_u = -(A/alpha)
  gamma_x = (R * exp(alpha*t)) / alpha

  ## Calculate the result
  (1/alpha) * exp(gamma_u) * (gamma_x^(A/alpha)) * upper_inc_gamma(gamma_u, gamma_x)

}
