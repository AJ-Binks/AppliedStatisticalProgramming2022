#' Plotting function for PoisMLE
#'
#' plots the MLE with a confidence interval of plus and minus 1.96 standard errors. Used for object of class \code{PoisMLE}.
#'
#' @param y The vector of observed data
#' @param SEtype One of two methods of calculation; basic or bootstrap 
#' @param B The number of bootstrapped resamplings if using SEtype="bootstrap". Default set to 1000.
#'
#' @return plot
#'  \item{plot}{A plot for PoisMLE that shows the MLE plus and minus 1.96 standard errors.}
#' @author Annie Jarman
#' @note This is used internally for the \code{estimatePois} function
#' @examples
#' 
#' set.seed(1227)
#' y <- rpois(1227,130)
#' plotMLE(y, "basic")
#' plotMLE(y, "bootstrap", 1000)
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @rdname plotMLE
#' @export

#set generic
setGeneric(name="plotMLE",
           def=function(y, SEtype, B=1000)
           {standardGeneric("plotMLE")}
)

#set method
setMethod(f="plotMLE",
          definition=function(y, SEtype=c("basic", "bootstrap"), B){
            #get mle
            lambdaHat <- mle(y)
            #get se
            se <- standardError(y, SEtype, B)
            #make sequence of lambdas around mle
            lambdaSeq <- seq(lambdaHat - 2, lambdaHat + 2, by=0.1)
            #take log likelihood of lambda sequence
            likelihoods <- logLik(y, lambdaSeq)
            #join these in df
            df <- data.frame("x"=lambdaSeq, "y"=likelihoods)
            #use ggplot2 to plot
            plot <- ggplot2::ggplot()+ 
              geom_point(aes(lambdaSeq, likelihoods), data=df) + #points for potential lambdas and their likelihood
              geom_vline(aes(xintercept=lambdaHat, color="red"))+ #vertical line for MLE
              geom_vline(aes(xintercept=lambdaHat + 1.96*se, color="grey")) + #vertical line for higher confidence interval
              geom_vline(aes(xintercept=lambdaHat - 1.96*se, color="grey")) + #vertical line for lower confidence interval
              labs(x="Lambda", y="Log Likelihood") +
              theme_minimal()
            return(plot)  
          }
)