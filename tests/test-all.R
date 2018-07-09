library(testthat)
library(nat)

# suppress RGL in headless environments (some CRAN build machines fail otherwise)
if(!interactive())
  Sys.setenv(RGL_USE_NULL=TRUE)

test_check("nat")
