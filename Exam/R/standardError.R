#' Standard Error function
#'
#' Calculates the standard error. Used for object of class \code{PoisMLE}.
#'
#' @param y The vector of observed data
#' @param SEtype One of two methods of calculation; basic or bootstrap 
#' @param B The number of bootstrapped resamplings if using SEtype="bootstrap". Default set to 1000. 
#'
#' @return Numeric value
#'  \item{SE}{The standard error. For SEtype="bootstrap", this is the standard deviation of bootstrapped MLEs.}
#' @author Annie Jarman
#' @note This is used internally for the \code{estimatePois} function
#' @aliases SE
#' @examples
#' 
#' set.seed(1227)
#' y <- rpois(12270,130)
#' standardError(y, "basic")
#' standardError(y, "bootstrap", 1000)
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @import methods
#' @import stats
#' @rdname standardError
#' @export

#write se function - dont need to use S4 (devtools lecture)
standardError <- function(y, SEtype, B=1000){
  #test B is right
  if(B <=0){
    stop("invalid B")
  }
  #create n
  n <- length(y)
  #if SEtype="basic"
  if(SEtype=="basic"){
    stError <- sqrt(mle(y)/n)
  }
  #if SEtype="bootstrap"
  if(SEtype=="bootstrap"){
    #create B samples from y (sample with replacement) - result should be a matrix n by B
    Bsamples <- replicate(B, sample(y, n), TRUE)
    #calculate MLE for each sample - result should be a vector of MLES of length B
    sampleMLE <- apply(Bsamples, 2, mle)
    #find standard deviation of sampleMLE vector - this output is the bootstrapped standard error
    stError <- stats::sd(sampleMLE)
  }
  return(stError)
}
