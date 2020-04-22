#' Melt data from read.OTM33A into a database format
#' @param dat Data table
#'
#' @return
#'
#' @export
#' @examples
#' db=as.db.OTM33A(dat)
as.db.OTM33A <- function(dat) {
  wind.vars <- c("ws3u",
                 "ws3v",
                 "ws3w",
                 "ws3",
                 "wd2",
                 "wd3",
                 "Temp",
                 "Pressure",
                 "X3DS.U",
                 "X3DS.V",
                 "X3DS.W",
                 "GPS.Latitude",
                 "GPS.Longitude",
                 "Temp.Alt",
                 "DateTime"
  )
  gas.vars = c("CO2",
               "CH4",
               "BEN",
               "TOL",
               "ETB",
               "XYO",
               "XYM",
               "XYP",
               "SO2",
               "STY",
               "FOR",
               "NH3",
               "NO",
               "NO2",
               "O3",
               "VOC"
  )
  wind.id.vars <- c("Index")
  gas.id.vars <-c("Index.gas")
  wind.db <- melt(dat[sub==TRUE],id.vars=wind.id.vars,measure.vars=wind.vars,variable.name="Parameter",value.name="Value")
  gas.db <- melt(dat[sub==TRUE],id.vars=gas.id.vars,measure.vars=gas.vars,variable.name="Parameter",value.name="Value")
  setnames(gas.db,"Index.gas","Index")
  gas.db=gas.db[Index>0]
  db <- rbind(wind.db,gas.db)
  db <- db[!is.nan(Value)]
  attr(db,"distance")<-attr(dat,"distance")
  attr(db,"heading")<-attr(dat,"heading")
  attr(db,"rotation.a")<-attr(dat,"rotation.a")
  attr(db,"rotation.b")<-attr(dat,"rotation.b")
  attr(db,"file.name")<-attr(dat,"file.name")
  return(db)
}

