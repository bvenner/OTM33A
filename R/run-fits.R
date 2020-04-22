#' run batch files for a data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' @return
#'
#' @export
#' @examples
#' run.fits(dat)

run.fits <- function(db,analyte="CH4",wdfilt=60,binwidth=10,min.n=0.02) {
  tryCatch({
    dat <- cast.db(db,analyte=analyte)
    dat <- subtract.background(dat,analyte=analyte)
    dat <-  calcPSG.bin(dat,analyte=analyte,wdfilt=wdfilt,binwidth=binwidth,min.n=min.n)
    dat <- calcLyProjection.bin(dat,analyte=analyte,thetafilt=wdfilt,binwidth=binwidth,min.n=min.n)
    dat <- calcPlume(dat)
    dat <- calcEmissionsRate(dat,analyte=analyte)
#  retval=fetch.results(dat)
  return(dat)})
}
