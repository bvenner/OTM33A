#' Caculate DQI statistics at the end of the batch.
#' DQI statistics are documented in Appendix D to Appendix F1 of OTM 33A, v 1.3, p 53 to 59
#'
#' @param
#' dat:  Data table, after running emissions estimate calcs
#'
#' @return list of statistics used to calculate DQIs
#' @export
#' @examples
#' y = evalDQI(dat)
calcDQI <- function(dat) {
  mean.na<-function(x){mean(x,na.rm=T)}
  sd.na<-function(x){mean(x,na.rm=T)}
  bg <- attr(dat,"CH4.bg")
  dat.filt = dat[mysub==TRUE & theta.filter==TRUE]
  dat.summary <- c(
    mn.WD.2D = dat.filt[,mean.na(wd2)],
    mn.WD.3D = dat.filt[,mean.na(wd3)],
    sd.WD.2D = dat.filt[!is.na(wd2),sd.wd.yam(wd2)],
    sd.WD.3D = dat.filt[!is.na(wd3),sd.wd.yam(wd3)],
    pressure = dat.filt[,mean.na(Pressure)],
    u = dat.filt[,mean.na(X3DS.U)],
    v = dat.filt[,mean.na(X3DS.V)],
    w = dat.filt[,mean.na(X3DS.W)],
    temp = dat.filt[,mean.na(Temp)],
    u.u = dat.filt[,mean.na(X3DS.U^2)],
    v.v = dat.filt[,mean.na(X3DS.V^2)],
    w.w = dat.filt[,mean.na(X3DS.W^2)],
    temp.temp = dat.filt[,mean.na(Temp^2)],
    u.v = dat.filt[,mean.na(X3DS.U*X3DS.V)],
    u.w = dat.filt[,mean.na(X3DS.U*X3DS.W)],
    u.t = dat.filt[,mean.na(X3DS.U*Temp)],
    v.w = dat.filt[,mean.na(X3DS.V*X3DS.W)],
    v.t = dat.filt[,mean.na(X3DS.V*Temp)],
    w.t = dat.filt[,mean.na(X3DS.W*Temp)],
    gps.lat.mn = dat.filt[,mean.na(GPS.Latitude)],
    gps.lat.sd = dat.filt[,sd.na(GPS.Latitude)],
    gps.lon.mn = dat.filt[,mean.na(GPS.Longitude)],
    gps.lon.sd = dat.filt[,sd.na(GPS.Longitude)],
#    CO2.mn = dat.filt[,mean.na(CO2)],
#    CO2.sd = dat.filt[,sd(CO2)],
    N.5.percent = round(dat.filt[,.N]*0.05),
    CH4.max = dat.filt[,max(Analyte)] + bg,
    CH4.mn.95 = dat.filt[Analyte>quantile(Analyte,.95),mean.na(Analyte)] + bg,
    CH4.sd.95 = dat.filt[Analyte>quantile(Analyte,.95),sd.na(Analyte)],
    CH4.min = dat.filt[,min(Analyte)] + bg,
    CH4.mn.5 = dat.filt[Analyte<quantile(Analyte,.05),mean.na(Analyte)] + bg,
    CH4.sd.5 = dat.filt[Analyte<quantile(Analyte,.05),sd.na(Analyte)],
    CH4.mn = dat.filt[,mean.na(Analyte)]+bg,
    ws3.mn = dat.filt[,mean.na(ws3)],
    temp.K = dat.filt[,mean.na(Temp)]+273.15,
    wd2.mn = dat.filt[,mean.na(wd2)],
    wd3.mn = dat.filt[,mean.na(wd3)],
    wd2.sd = dat.filt[,sd.na(wd2)],
    wd3.sd = dat.filt[,sd.na(wd3)],
    turbint = attr(dat,"turbint"),
    PG.sd.2 = attr(dat,"PG.sd.2"),
    PG.sd.3 = attr(dat,"PG.sd.3"),
    PG.ti = attr(dat,"PG.ti"),
    pgsigmay = attr(dat,"pgsigmay"),
    pgsigmaz = attr(dat,"pgsigmaz"),
    centroid.dir = NA,
    binwidth = attr(dat,"binwidth"),
    cut.wind.speed = NA,
    cut.bin.density = attr(dat,"min.n"),
    a1 = attr(dat,"CH4.Ly.peak"),
    sigma = attr(dat,"CH4.Ly.sigma"),
    CH4.a1 = attr(dat,"CH4.Ly.peak.g.per.m3"),
    QA.bin = (as.numeric(attr(dat,"CH4.wd.rot"))+180)/10, # I can't discern from the Matlab code what this value is supposed to be
    QA.count = dat.filt[,.N,thetabin][,max(N)],
    temp.ratio = dat.filt[,(mean.na(Temp)+273.15)/(mean.na(Temp.Alt)+273.15)],
    temp.sd = dat.filt[,sd.na(Temp)],
    temp.min = dat.filt[,min(Temp)],
    temp.max = dat.filt[,max(Temp)],
    PSG = attr(dat,"CH4.PSG"),
    distance = attr(dat,"distance")
  )
  return(dat.summary)
}
