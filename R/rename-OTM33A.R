#' Melt and rename data columns from read.OTM33A for subsequent processing
#' @param dat Data table
#' @export
#' @examples
#' dat=rename.OTM33A(dat)

rename.OTM33A <- function(dat) {
  dat[,Index := 1:.N]
  setnames(dat,"AirMar.Barometer..mBar.","Pressure")
  setnames(dat,"X3DS.Sonic.Temp","Temp")
  setnames(dat,"AirMar.Wind.Direction..TRUE.","wd2")
  setnames(dat,"AirMar.Air.Temp..oC.","Temp.Alt")
  return(dat)
}
