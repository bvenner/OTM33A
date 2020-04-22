#' Caculate DQI statistics at the end of the batch.
#' DQI statistics are documented in Appendix D to Appendix F1 of OTM 33A, v 1.3, p 53 to 59
#' This has lots of hacks to remain consistent with original ORD code.
#'
#' @param
#' db:  Data table, database format for numerical data, after running emissions estimate calcs for CH4
#'
#' @return list of statistics used to calculate DQIs
#' @export
#' @examples
#' y = calcDQIdb(dat)
calcDQIdb <- function(datatable) {
  bg <- attr(datatable,"CH4.bg")
  statvars=c("wd2","wd3","ws3","ws3u","ws3v","ws3w","Temp","Temp.Alt","Pressure","GPS.Latitude","GPS.Longitude","CO2","CH4")
  db <- dcast(datatable,DateTime~Parameter,subset=.(Parameter %in% statvars),fun.aggregate=mean,value.var="Value")
  thetafilt <- attr(datatable,"CH4.thetafilt")
  rotation <- attr(datatable,"CH4.wd.rot")
  binwidth <- attr(datatable,"CH4.Ly.binwidth")
  db <- db[abs(wd3-rotation) < thetafilt]
  db.summary <- list(
    mn.WD.2D = db[,mean(wd2)],
    mn.WD.3D = db[,mean(wd3)],
    sd.WD.2D = db[,sd.wd.yam(wd2)],
    sd.WD.3D = db[,sd.wd.yam(wd3)],
    pressure = db[,mean(Pressure)],
    u = db[,mean(ws3u)],
    v = db[,mean(ws3v)],
    w = db[,mean(ws3w)],
    temp = db[,mean(Temp)]+273.15,
    u.u = db[,mean(ws3u^2)],
    v.v = db[,mean(ws3v^2)],
    w.w = db[,mean(ws3w^2)],
    temp.temp = db[,mean(Temp^2)],
    u.v = db[,sum(ws3u*ws3v)],
    u.w = db[,sum(ws3u*ws3w)],
    u.t = db[,sum(ws3u*Temp)],
    v.w = db[,sum(ws3v*ws3w)],
    v.t = db[,sum(ws3v*Temp)],
    w.t = db[,sum(ws3w*Temp)],
    gps.lat.mn = db[,mean(GPS.Latitude)],
    gps.lat.sd = db[,sd(GPS.Latitude)],
    gps.lon.mn = db[,mean(GPS.Longitude)],
    gps.lon.sd = db[,sd(GPS.Longitude)],
    CO2.mn = db[,mean(CO2)],
    CO2.sd = db[,sd(CO2)],
    N.5.percent = db[,.N]*0.05,
    CH4.max =  db[, max(CH4)] + bg,
    CH4.mn.95 = db[CH4>quantile(CH4,.95),mean(CH4)] + bg,
    CH4.sd.95 = db[CH4>quantile(CH4,.95),sd(CH4)],
    CH4.min = db[,min(CH4)] + bg,
    CH4.mn.5 = db[CH4<quantile(CH4,.05),mean(CH4)] + bg,
    CH4.sd.5 = db[CH4<quantile(CH4,.05),sd(CH4)],
    CH4.mn = db[, mean(CH4)] + bg,
    ws3.mn = db[, mean(ws3)],
    temp.C = db[, mean(Temp)],
    wd2.mn = db[, mean(wd2)],
    wd3.mn = db[, mean(wd3)],
    wd2.sd = db[, sd(wd2)],
    wd3.sd = db[, sd(wd3)],
    QA.count = db[,.N,cut(wd3-rotation, seq(-360, 360, binwidth), right=FALSE)][,max(N)],
    temp.ratio = (db[,mean(Temp)]+273.15)/(db[,mean(Temp.Alt)]+273.15),
    temp.sd = db[,sd(Temp)],
    temp.min = db[,min(Temp)],
    temp.max = db[,max(Temp)]
  )
  db.summary$PSG = attr(datatable,"CH4.PSG")
  db.summary$distance = attr(datatable,"distance")
  db.summary$turbint = attr(datatable,"turbint")
  db.summary$PG.sd.2 = attr(datatable,"PG.sd.2")
  db.summary$PG.sd.3 = attr(datatable,"PG.sd.3")
  db.summary$PG.ti = attr(datatable,"PG.ti")
  db.summary$pgsigmay = attr(datatable,"pgsigmay")
  db.summary$pgsigmaz = attr(datatable,"pgsigmaz")
  db.summary$centroid.dir = NA
  db.summary$binwidth = attr(datatable,"binwidth")
  db.summary$cut.wind.speed = NA
  db.summary$cut.bin.density = attr(datatable,"min.n")
  db.summary$a1 = attr(datatable,"CH4.Ly.peak")
  db.summary$Ly.sigma = attr(datatable,"CH4.Ly.sigma")
  db.summary$CH4.a1 = attr(datatable,"CH4.Ly.peak.g.per.m3")
  db.summary$QA.bin = (as.numeric(attr(datatable,"CH4.wd.rot"))+180)/10
  return(db.summary)
}
