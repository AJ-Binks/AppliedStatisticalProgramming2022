#' fitted model for PoisMLE
#'
#' Finds a fitted model of class \code{PoisMLE} using the function \code{estimatePois}
#'
#'The class has the following slots:
#' \itemize{
#'  \item{y}{The original data}
#'  \item{MLE}{The maximum likelihood estimator for this dataset} 
#'  \item{LL}{The log likelihood calculated from the observed data assuming the MLE is correct}
#'  \item{SE}{The standard error for the MLE}
#'  \item{SEtype}{The method used to calculate the standard error}
#'  }
#' @author Annie Jarman
#' @aliases poissonMLE
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}} 
#' @seealso \code{\link{plotMLE}}
#' @rdname PoisMLE
#' @export

#set validity checks
check_PoisMLE <- function(object){
  #create errors log - saw that Cecilia did this for PS5 and thought it was clever
  #test input
  errors <- character()
  #data should all be positive integers or zero for Poisson models
  if(any(object@y<0)){
    errors<- c(errors, "Vector y constains non-positive values")
  }
  if(any(as.integer(object@y)!=object@y)){
    errors <- c(errors, "Vector y constains non-integer values")
  }
  #test LL
  if(logLik(object@y, object@MLE)!=object@LL){
    errors<- c(errors, "logLik is invalid")
  }
  #test MLE
  if(mle(object@y)!=object@MLE){
    errors<- c(errors, "MLE is invalid")
  }
  #test SE
  test <- object@SEtype == "basic" | object@SEtype == "bootstrap"
  if(!test){
    errors<- c(errors, "SEtype is invalid")
  }
  # if there are errors, return errors 
  if(length(errors) == 0) TRUE else errors 
}

#' @rdname PoisMLE
#' @export
#set class definition and include validity check
setClass(Class="PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = numeric(),
           MLE = numeric(),
           LL = numeric(),
           SE = numeric(),
           SEtype = character()
         ),
         validity= check_PoisMLE
)

#' @rdname PoisMLE
#' @export
#set method using validator
setMethod("initialize", "PoisMLE", 
          function(.Object, ...){
            value = methods::callNextMethod()
            return(value)
          }
) 

