#' Preprocess OTM33A data set prior to Gaussian fits
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' dat <- run.preprocess.STR(dat)

run.preprocess.STR <- function(dat) {
  # dat <- rename.STR(dat)
  dat <- align.time(dat,40)
  dat <- rotateWindDirection(dat)
  return(dat)
}
