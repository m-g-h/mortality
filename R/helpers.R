#' Test if an object is a scalar
#'
#' @param x Object to test
#'
#' @return Returns a {Logical scalar} with value \code{TRUE} if the object is a
#'  scalar
#'
#' @keyword Internal
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
