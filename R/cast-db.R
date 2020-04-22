#' cast db back to spreadsheet form for analysis
#' @param
#' db:  Data table in db style developed from as.db.OTM33A
#' analyte:  Data table in spreadsheet format for use in analysis routines
#' @return
#' dat:
#' @export
#' @examples
#' dat <- cast.db(db,analyte="CH4")
cast.db <- function(db,analyte="CH4") {
  statvars=c(analyte,"wd2","wd3","ws3","ws3u","ws3v","ws3w","Temp","Temp.Alt","Pressure","GPS.Latitude","GPS.Longitude","CO2","CH4","X3DS.U","X3DS.V","X3DS.W")
  dat <- dcast(db,Index~Parameter,subset=.(Parameter %in% statvars),fun.aggregate=mean,value.var="Value")
  setnames(dat,analyte,"Analyte")
  dat <- dat[!is.nan(Analyte)]
  attr(dat,"distance")<-attr(db,"distance")
  attr(dat,"heading")<-attr(db,"heading")
  attr(dat,"rotation.a")<-attr(db,"rotation.a")
  attr(dat,"rotation.b")<-attr(db,"rotation.b")
  attr(dat,"file.name")<-attr(db,"file.name")
  return(dat)
}
