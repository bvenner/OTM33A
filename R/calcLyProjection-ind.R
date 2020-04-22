#' Calculate plume direction and spread along axis perpendicular to the wind using individual data
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return
#'   object of class "lm" with plume fit
#' @export
#' @examples
#' calcLyProjection.ind(dat)

calcLyProjection.ind <- function(dat,analyte="CH4",thetafilt=90,plot=TRUE) {
  distance <- attr(dat,"distance")
  dat[,Ly := distance*sin(theta*pi/180)]
  mu0 <- dat[which.max(Analyte), Ly]
  a0 <- dat[,max(Analyte)]
  sigma0 <- dat[,(max(Ly)-min(Ly))/5]
  start.0=list(mu=mu0,sigma=sigma0,a=a0)

  # Fit Gaussian curve to proxy distance
  lyfit <- nls(Analyte ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=dat,
                  start=start.0,
                  algorithm = "port")
  if (plot == TRUE) {
    # Plot Gaussian Fit
    dat[,Fit.Ly := fitted(lyfit)]
    fig2 <- ggplot(dat, aes(x=Ly, y=Analyte) ) +
      geom_point(col="black") +
      geom_line(aes(y=Fit.Ly), col="red") +
      theme_bw(base_size=16) +
      xlab("Ly") +
      ylab(analyte)
    print(fig2)
  }
  setattr(dat,paste(analyte,"Ly.rot",sep="."),as.numeric(coef(lyfit)[1]))
  setattr(dat,paste(analyte,"Ly.sigma",sep="."),as.numeric(coef(lyfit)[2]))
  setattr(dat,paste(analyte,"Ly.peak",sep="."),as.numeric(coef(lyfit)[3]))
  setattr(dat,paste(analyte,"thetafilt",sep="."),thetafilt)
  return(dat)
}
