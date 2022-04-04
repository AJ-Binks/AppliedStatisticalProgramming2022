#create Trapezoid class

#' Trapezoid integration object
#' 
#' objects in this class \code{Trapezoid} are created with the function \code{integrateIt}
#' 
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{integral} The integral
#' \item \code{x} a vector of values
#' \item \code{y} a vector of evaluated values 
#' }
#'
#' @author Annie Jarman: \email{a.a.jarman@wustl.edu}
#' @aliases Trapezoid 
#' @rdname Trapezoid
#' @export
#create class - much of this comes from Jacob's code in class slides 18
#set class
setClass(Class = "Trapezoid", 
         representation = representation(
           x = "numeric",
           y = "numeric",
           integral = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           integral = c()
         ),
         validity = check_trapezoid
)
#' @export

#validity
check_trapezoid <- function(object){
  #define values needed for integration
  #bounds for x
  a <- object@x[1]
  b <- tail(object@x, 1)
  #bounds for y
  f_a <- object@y[1]
  f_b <- tail(object@y, 1)
  #n
  n <- length(object@x)
  #h
  h <- (b - a) / n 
  #x
  x_vec <- object@x[a < object@x & b > object@x]
  #y
  y_vec <- object@y[f_a < object@y & f_b > object@y]
  
  #compute integral
  result <- (h / 2) * (f_a + sum(2 * y_vals) + f_b)
  
  #check integral result
  if(result!=object@integral){return("integration incorrect")}
}
#' @export

#set method, i.e., initialization
setMethod("initialize", 
          "Trapezoid", 
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
) 
#' @export

setMethod(f = "print",
          signature(x = "Trapezoid"),
          definition = function(x){
            print(x@integral)
          })
