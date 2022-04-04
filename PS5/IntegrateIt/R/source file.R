#source document
library(devtools)
library(roxygen2)


#' integrateIt function
#'
#' integrates with trapezoidal or simpsons rule
#'
#' @param x A vector of values
#' @param y A vector of values with the same dimensionality as \code{x}.
#' @param a integration starting value
#' @param b integration ending value
#' @param type pick method of either trapezoidal or simpson
#'
#' @return Return a list including:
#'  \item{object}{Object of class corresponding to Rule argument}
#'  \item{x}{x value of the returned object}
#'  \item{y}{y value of the returned object}
#'
#' @author Annie Jarman
#' @note generic function
#' @examples
#' x <- seq(0, 10)
#' y <- seq(0,20,2)
#' integrateIt(x, y, 0, 10, type="Trap")
#'
#' @seealso \code{\link{print}}
#' @rdname integrateIt
#' @aliases integrateIt
#' @export
#make generic
setGeneric(name="integrateIt",
def=function(x, y, a, b, type)
{standardGeneric("integrateIt")}
)
#' @export
#set method
setMethod(f="integrateIt",
          definition=function(x,y,a,b,type){
            x<-as.matrix(x) #change vector to matrix
            x<-t(x) #same
            y<-as.matrix(y) #change vector to matrix
            y<-t(y) #same
            if (ncol(x)==ncol(y)){# condition to check the function works
              if (type=="Trap"){
                trapvec<-y*2 #setting up the doubled values
                trapvec[1]<-y[1] #replace the first
                trapvec[ncol(y)]<-y[ncol(y)] #replace the last
                n <- length(x)-1 #minus 1 is to make the math work
                h<-(b-a)/n #creates the h with the bounds
                trapoutput<-h/2*(trapvec) #setting up the series of numbers
                x<-as.vector(x) #transfering the x and y output back to how it was entered
                y<-as.vector(y)
                return(new("Trapezoid", result=sum(trapoutput), x = x, y = y)) #create the output, notice the sum of trap output for all of the values
              }
              if (type=="Simpson"){
                simpvec<-y #Setting up the vector
                for (i in 1:ncol(y)){
                  if((i %% 2) == 0){
                    simpvec[i]<-y[i]*4} #for loop mutiplying every other value by 4
                  else {simpvec[i]<-y[i]*2}} #doing the rest by two
                simpvec[1]<-y[1] #replace the first term
                simpvec[ncol(y)]<-y[ncol(y)] #replace the last term
                n <- length(x)-1 #minus 1 is to make the math work
                h<-(b-a)/n #create the h again with the bounds
                simpoutput<-h/3*(simpvec) #same thing but divided by 3
                x<-as.vector(x) #transfering the x and y output back to how it was entered
                y<-as.vector(y)
                return(new("Simpson", result=sum(simpoutput), x = x, y = y))} #Create the output
              else{return("Please select a valid type: either Simpson or Trapezoid")}} #error message
            else{return("make sure your x and y are of equal length")} #error message
          }
)

devtools::document()


