#' Calculate plume direction and spread using indiviudal data on the natural scale
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return object of class "lm" from plume fit
#' @export
#' @examples
#' calcPSG.ind(dat)

calcPSG.ind <- function(dat, analyte="CH4", plot = TRUE) {

  plot.title = attr(dat,"file.name")
  if(is.null(plot.title)) plot.title="file.name not set"

  # Set initial values for gaussian fit estimation
  mu0 <- dat[which.max(Analyte), wd3]
  k0 <- dat[,max(Analyte)]

  fig1 <- ggplot(dat, aes(x=wd3, y=Analyte), col="black") +
      geom_point(alpha=0.2)+
      theme_bw(base_size=16) +
      xlab("Wind Direction") +
      ylab(analyte) + ggtitle(plot.title)
  # Fit Gaussian curve to wd bins
  wdfit <- nls(Analyte ~ k*exp( - 1/2*((wd3-mu)/sigma)^2),
               start=c(mu = mu0, sigma=10, k = k0),
               data=dat, algorithm = "port")
  # Plot Gaussian fit to wind direction
  dat[,Fit := fitted(wdfit)]
  fig1 <- fig1 + geom_point(aes(y=Fit), col="red") + geom_line(aes(y=Fit), col="red")
  setattr(dat,paste(analyte,"wd.rot",sep="."),as.numeric(coef(wdfit)[1]))
  setattr(dat,paste(analyte,"wd.sigma",sep="."),as.numeric(coef(wdfit)[2]))
  setattr(dat,paste(analyte,"wd.peak",sep="."),as.numeric(coef(wdfit)[3]))
  if (plot==TRUE) {
     print(fig1)
  }
  return(dat)
}
