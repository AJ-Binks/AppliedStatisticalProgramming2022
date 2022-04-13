#' Log likelihood function
#'
#' Calculates a log likelihood for the observed data. Used for object of class \code{PoisMLE}.
#'
#' @param y The vector of observed data
#' @param lambda the assumed value of lambda
#'
#' @return Numeric value
#'  \item{LL}{The log likelihood for the observed data conditioned on the value of lamda}
#' @author Annie Jarman
#' @note This is used internally for the \code{estimatePois} function
#' @aliases logLikelihood
#' @examples
#' 
#' set.seed(1227)
#' y <- rpois(1227,130)
#' lambda <- mle(y)
#' logLik(y,lambda)
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @import methods
#' @import stats
#' @rdname logLik
#' @export

#write logLik function - dont need to use S4 (devtools lecture)
logLik <- function(y, lambda){
  n <- length(y)
  LL <- 1*n*lambda - sum(log(factorial(y))) + log(lambda)*sum(y)
  return(LL)
}
