#load libraries
library(devtools)
library(roxygen2)
library(testthat)

#setwd
setwd("~/Documents/GitHub/PS3/Untitled/Exam/")

#uncomment and run once to create package directory
#create("PoisMLE")

#can keep running this chunk with new updates to package
#load package
current.code <- as.package("poisMLE")
#loads all the functions
load_all(current.code)
#makes the help files
document(current.code)
#run the R checks
check(current.code)

#run examples from functions to make sure they work
set.seed(1227)
y <- rpois(1227,130)
#test mle
lambda <- mle(y)
#test logLik
logLik(y,lambda)
#test both standard errors
standardError(y, "basic")
standardError(y, "bootstrap", 1000)
#test plot
library(ggplot2)
plotMLE(y, "basic")
plotMLE(y, "bootstrap", 1000)
#test final output
estimatePois(y, "basic")
estimatePois(y, "bootstrap")

#test help functions
?mle()
?logLik()
?standardError()
?estimatePois()
?PoisMLE
?plotMLE

