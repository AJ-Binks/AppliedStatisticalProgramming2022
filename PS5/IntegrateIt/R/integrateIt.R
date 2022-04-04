#source document
library(devtools)
library(roxygen2)


#' integrateIt function
#'
#' integrates with trapezoidal or simpsons rule
#'
#' @param x A vector of input values
#' @param f_x function to generate y with x 
#' @param a integration starting value
#' @param b integration ending value
#' @param rule pick method of either trapezoidal or simpson
#'
#' @return Return a list including:
#'  \item{integral}{the approximated integral}
#'  \item{x}{vector of values}
#'  \item{y}{vector of evaluated values}
#'
#' @author Annie Jarman
#' @note generic function to produce approximate integral object with Simpson or Trapezoid rule
#' @examples
#' x <- seq(0, 10)
#' y <- seq(0,20,2)
#' integrateIt(x, y, 0, 10, rule="Trapezoid")
#'
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}
#' @rdname integrateIt
#' @aliases integrateIt
#' @export

#make generic
setGeneric(name = "integrateIt",
           def = function(x, f_x, a, b, rule)
           {standardGeneric("integrateIt")}
)
#' @export

#set method
setMethod(f = "integrateIt",
          definition = function(x, f_x, a, b, rule){
            #extract values needed for integration
            #x and y
            x_vals <- x[x >= a & x <= b]
            y_vals <- unlist(lapply(x_vals, f_x))
            #n
            n <- length(x_vals)
            #h
            h <- (b - a) / n 
            
            #use Simpson
            if (rule == "Simpson") {
              y_vals_mid <- y_vals[2:(n-1)]
              result <- (h / 3) * (y_vals[1] + sum(4 * y_vals_mid[c(T, F)]) + sum(2 * y_vals_mid[c(F,T)]) + y_vals[n])
              return(new("Simpson", 
                         x = x_vals, 
                         y = y_vals,
                         integral = result))
            }
            
            #use Trapezoid
            if (rule == "Trapezoid") {
              result <- (h / 2) * (y_vals[1] + sum(2 * y_vals[2:(n-1)]) + y_vals[n])
              return(new("Trapezoid", 
                         x = x_vals, 
                         y = y_vals,
                         integral = result))
            }
          }
)






