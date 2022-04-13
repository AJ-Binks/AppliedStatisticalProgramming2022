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
#' @import ggplot2
#' @rdname plotMLE
#' @export

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
