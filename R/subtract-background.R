#' Calculate background as the average of the lowest five percent of data from data set
#' @param
#' dat:  Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return None, but has the side effect of adding column CH4 and attribute bg to dat
#' @export
#' @examples
#' subtract.background(dat)

subtract.background <- function(dat,analyte="CH4") {
  bg.name = paste(analyte,".bg",sep="")
  if(is.null(attr(dat,bg.name))) {
    bg <- dat[Analyte<quantile(Analyte,probs=.05),mean(Analyte)]
    dat[,Analyte := Analyte - bg]
    setattr(dat,bg.name,bg)
  }
  return(dat)
}
