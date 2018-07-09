# patch over an issue with trailing separators on Windows
file.exists <- function(...) {
  x=unlist(pairlist(...), use.names = FALSE)
  if (.Platform$OS == "windows") {
    x=sub("[/\\]$","", x)
  }
  base::file.exists(x)
}


no_internet <- function() {

  nit <- Sys.getenv("NAT_INTERNET_TESTS")
  if (nzchar(nit))
    return(!as.logical(nit))

  if(!requireNamespace('httr', quietly = TRUE))
    stop("Please install suggested library httr to use no_internet")

  internet.ok = identical(
    try(httr::status_code(
      httr::HEAD("http://flybrain.mrc-lmb.cam.ac.uk/", httr::timeout(2))),
      silent = TRUE),
    200L)
  
  Sys.setenv(NAT_INTERNET_TESTS = internet.ok)
  !internet.ok
}

skip_cran_no_internet <- function() {
  if(!requireNamespace('testthat', quietly = TRUE))
    stop("Please install suggested library testthat to use skip_cran_no_internet")
  testthat::skip_on_cran()
  testthat::skip_if(no_internet(), message = "No internet!")
}
