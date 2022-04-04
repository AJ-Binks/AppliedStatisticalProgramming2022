#load libraries
library(devtools)
library(roxygen2)
#set wd
setwd("~/Documents/GitHub/PS3/Untitled/PS5")

#load package
current.code <- as.package("integrateIt")
#load functions
load_all(current.code)
#make help file
document(current.code)

#test



# This installs the package
devtools::install(current.code)

# Call help page on our method
?integrateIt
?`Simpson-class`

# This builds the package
# devtools::build("integrateIt")
