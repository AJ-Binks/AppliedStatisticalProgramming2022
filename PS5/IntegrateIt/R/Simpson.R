#create Simpson class

#' Simpson integration object
#' 
#' objects in this class \code{Simpson} are created with the function \code{integrateIt}
#' 
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{integral} The integral
#' \item \code{x} a vector of values
#' \item \code{y} a vector of values of same dimensionality as \code{x} 
#' }
#'
#' @author Annie Jarman: \email{a.a.jarman@wustl.edu}
#' @aliases Simpson 
#' @rdname Simpson
#' @export
#create class - much of this comes from Jacob's code in class slides 18
#set class
setClass(Class = "Simpson", 
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
         validity = check_simpson
)
#' @export

#validity
check_simpson <- function(object){
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
  result <- (h / 3) * (f_a + sum(4 * y_vec[c(T, F)]) + sum(2 * y_vec[c(F,T)]) + f_b)
  
  #check integral result
  if(result!=object@integral){return("integration incorrect")}
}
#' @export

#set method, i.e., initialization
setMethod("initialize", 
          "Simpson", 
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
) 
#' @export

setMethod(f = "print",
          signature(x = "Simpson"),
          definition = function(x){
            print(x@integral)
          })
