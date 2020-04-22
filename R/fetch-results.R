#' Fetch final results from data
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return Vector of named results, suitable for storing in a data.table
#' @export
#' @examples
#' fetch.results(dat)
fetch.results <- function(dat) {
  try(y <- round(c(PSG=attr(dat,"CH4.PSG"),
           wd.rot=as.numeric(attr(dat,"CH4.wd.rot")),
           Ly.peak = as.numeric(attr(dat,"CH4.Ly.peak"))),2))
  return(y)
}
