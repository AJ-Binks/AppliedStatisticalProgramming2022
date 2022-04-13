#' estimate Poisson fitted model function
#'
#' Takes in data and returns an object of class \code{PoisMLE}.
#'
#' @param y The vector of observed data
#' @param SEtype One of two methods of calculation; basic or bootstrap 
#' @param B The number of bootstrapped resamplings if using SEtype="bootstrap". Default set to 1000. 
#'
#' @return an object of class \code{PoisMLE} containing
#'  \item{y}{The original data of positive integers}
#'  \item{MLE}{The maximum likelihood estimator for this dataset} 
#'  \item{LL}{The log likelihood calculated from the observed data assuming the MLE is correct}
#'  \item{SE}{The standard error. For SEtype="bootstrap", this is the standard deviation of bootstrapped MLEs.}
#'  \item{SEtype}{One of two methods - basic or bootstrap - used to calculate the standard error}
#' @author Annie Jarman
#' @aliases estimatePoisson
#' @examples
#' 
#' set.seed(1227)
#' y <- rpois(12270,130)
#' estimatePois(y, "basic)
#' estimatePois(y, "bootstrap", 1000)
#' @seealso \code{\link{standardError}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @rdname estimatePois
#' @export

#set generic
setGeneric(name = "estimatePois",
           def=function(y,SEtype,B=1000)
           {standardGeneric("estimatePois")}
)

#set method
#use internal functions to return PoisMLE class object
setMethod(f = "estimatePois",
          definition = function(y,SEtype=c("basic","bootstrap"),B){
            #use internal functions to fill slot outputs
            return(methods::new("PoisMLE",
                       y=y,
                       MLE = mle(y),
                       LL = logLik(y, mle(y)),
                       SE = standardError(y, SEtype, B),
                       SEtype = SEtype))
          }
)
