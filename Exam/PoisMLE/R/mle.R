#' MLE function
#'
#' Calculates the maximum likelihood estimator for lambda. Used for object of class \code{PoisMLE}.
#'
#' @param y The vector of observed data
#'
#' @return Numeric value
#'  \item{MLE}{The maximum likelihood estimator for lamda}
#' @author Annie Jarman
#' @note This is used internally for the \code{estimatePois} function
#' @aliases MLE
#' @examples
#' 
#' set.seed(1227)
#' y <- rpois(1227,130)
#' mle(y)
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{standardError}}
#' @import stats
#' @rdname mle
#' @export

#write mle function - dont need to use S4 (devtools lecture)
mle <- function(y){
  n <- length(y)
  MLE <- sum(y, na.rm=TRUE)/n
  return(MLE)
}
