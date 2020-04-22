#' Melt data from read.OTM33A into a database format
#' @param dat Data table
#'
#' @return
#'
#' @export
#' @examples
#' db=as.db.STR(dat)

as.db.STR <- function(dat) {
  measure.vars <- c("ws3u",
                    "ws3v",
                    "ws3w",
                    "ws3",
                    "wd2",
                    "wd3",
                    "Temp",
                    "Pressure",
                    "CO2",
                    "CH4",
                    "X3DS.U",
                    "X3DS.V",
                    "X3DS.W",
                    "GPS.Latitude",
                    "GPS.Longitude",
                    "Temp.Alt"
  )
  id.vars <- c("DateTime")
  db <- melt(dat,id.vars=id.vars,measure.vars=measure.vars,variable.name="Parameter",value.name="Value")
  attr(db,"distance")<-attr(dat,"distance")
  attr(db,"heading")<-attr(dat,"heading")
  attr(db,"rotation.a")<-attr(dat,"rotation.a")
  attr(db,"rotation.b")<-attr(dat,"rotation.b")
  attr(db,"file.name")<-attr(dat,"file.name")
  return(db)
}
