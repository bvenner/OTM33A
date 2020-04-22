#' Calculate plume characteristics from data.table
#' @param
#' dat: Analyte-specific Data table
#'
#' @return data.table with attributes set containing plume calculations
#' @export
#' @examples
#' dat = calcPlume(dat)

calcPlume <- function(dat) {
  thetafilt <- attr(dat,"CH4.thetafilt")
  rotation <- attr(dat,"CH4.wd.rot")
  dist.int <- round(attr(dat,"distance"))
  dat[,mysub :=abs(wd3-rotation) < thetafilt]
  # Input:  db:  data set
  # Output:  calculated plume characteristics

  # Constants from Gryning et al 1983

  # next statement imports psigma, turbint.breaks, wdsd.breaks
  data("pgsigma")

#  pgsigma=get(z[1])
#  turbint.breaks=get(z[2])
#  wdsd.breaks=get(z[3])

  # the following names depend upon renaming (i.e. rename.OTM33A)
  Tbar <- dat[mysub==TRUE,mean(Temp)]+273.15
  Pbar <- dat[mysub==TRUE,mean(Pressure)]
  Ubar <- dat[mysub==TRUE,mean(ws3)]
  ws3.sd <- dat[mysub==TRUE,sd(ws3)]

  wd3.sd <- dat[mysub==TRUE,sd.wd.yam(wd3)]
  wd2.sd <- dat[mysub==TRUE,sd.wd.yam(wd2)]
  turbint <- dat[mysub==TRUE,sd(ws3w)]/Ubar

  # PGI from turbulence, turbint.breaks imported from data(pgsigma)
  PGturbi <- as.numeric(as.character((cut(turbint, turbint.breaks, labels=rev(seq(1,7,1))))))
  # PGI from wd3_sd, wdsd.breaks imported from data(pgsigma)
  PG.sd.3 <- as.numeric(as.character(cut(wd3.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  PG.sd.2 <- as.numeric(as.character(cut(wd2.sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  # Calculate average PGI, round up if 0.5
  PGI <- round((PG.sd.3 + PGturbi)/2 + 0.0001)
  # pgsigma imported from pgsigma.Rdata
  pgsigmay <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                        pgsigma$PGI == PGI), "sigmay"])
  pgsigmaz <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                       pgsigma$PGI == PGI), "sigmaz"])

  setattr(dat,"PGI",as.numeric(PGI))
  setattr(dat,"PG.sd.3",as.numeric(PG.sd.3))
  setattr(dat,"PG.sd.2",as.numeric(PG.sd.2))
  setattr(dat,"PG.ti",as.numeric(PGturbi))
  setattr(dat,"Ubar",as.numeric(Ubar))
  setattr(dat,"Pbar",as.numeric(Pbar))
  setattr(dat,"Tbar",as.numeric(Tbar))
  setattr(dat,"turbint",as.numeric(turbint))
  setattr(dat,"pgsigmay",as.numeric(pgsigmay))
  setattr(dat,"pgsigmaz",as.numeric(pgsigmaz))
#  setattr(db,"PG.ti",as.numeric(PGturbi))
  return(dat)
}
