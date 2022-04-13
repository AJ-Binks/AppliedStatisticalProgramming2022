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

#plot
#set generic
setGeneric(f="plot",
           signature(x="PoisMLE")
           definition=function(x){
             plot(x@MLE)
           }
)

#set method
setMethod(f="plot",
          signature(x="PoisMLE"),
          definition=function(x){
            #make sequence of lambdas around mle
            lambdaSeq <- seq(x@MLE - 2, x@MLE + 2, by=0.1)
            #take log likelihood of lambda sequence
            likelihoods <- logLik(x@y, lambdaSeq)
            #put together in dat
            dat <- data.fame("x"=lambdaSeq, "y"=likelihoods)
            #create df
            df <- data.frame("x"=x@MLE, "y"=x@LL)
            #use ggplot2 to plot
            plot <- ggplot2::ggplot()+ 
              geom_vline(xintercept=x@MLE, color="red")+ #MLE
              geom_point(aes(x=x, y=y), data=df, color="red")+ #ME
              geom_point(aes(x=lambdaSeq, y=likelihoods), data=dat) + #points for potential lambdas and their likelihood
              geom_vline(aes(xintercept=lambdaHat, color="red"))+ #vertical line for MLE
              geom_vline(aes(xintercept=x@MLE + 1.96*se, linetype="dashed", color="grey")) + #vertical line for higher confidence interval
              geom_vline(aes(xintercept=x@MLE - 1.96*se, linetype="dashed", color="grey")) + #vertical line for lower confidence interval
              labs(x="Lambda", y="Log Likelihood") +
              theme_minimal()
            return(plot)  
          }
)