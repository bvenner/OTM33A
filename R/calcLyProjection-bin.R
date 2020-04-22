#' Calculate plume direction and spread using binned data and natural scale
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' wdfilt:  number in degrees, used to exclude data from fit that are within the filter
#' @return object of class "lm" with plume fit,
#' has the side effect of adding columns thetabin, theta.filter, Ly to dat
#' @export
#' @examples
#' calcLyProjection.bin(dat)

calcLyProjection.bin <- function(dat,analyte="CH4",thetafilt=90,binwidth=2,min.n=0,plot=TRUE) {
  # Calculate filter only include wind from the source +/- wind filter
    rotation <- attr(dat,paste(analyte,"wd.rot",sep="."))
    distance <- attr(dat,"distance")
    plot.title=attr(dat,"file.name")
    bins <- seq(-360, 360, binwidth)
#    dat=dcast(db,Index~Parameter, subset= .(Parameter %in% c(analyte,"wd3","DateTime")),value.var="Value",fun.aggregate=mean)
#    setnames(dat,analyte,"Analyte")
    dat[,theta := wd3-rotation]
    dat[,theta.filter := abs(theta) < thetafilt]
    dat[,thetabin := cut(theta, bins, right=FALSE)]
    dat[,Ly := distance*sin(theta*pi/180)]

    dat.lybin <- dat[theta.filter==TRUE,list(Analyte = mean(Analyte),n = .N,Ly=mean(Ly),theta=mean(theta)),thetabin][order(thetabin)]
    # for consistency with ORD code
    dat.lybin[,n.filter := n > min.n*dat[,.N]]

    # return(dat.lybin)
    # Set initial values for gaussian fit estimation
    mu0 <- dat.lybin[which.max(Analyte), Ly]
    a0 <- dat.lybin[,max(Analyte)]
    sigma0 <- dat.lybin[,(max(Ly)-min(Ly))/5]
    start.0=list(mu=mu0,sigma=sigma0,a=a0)

    # Fit Gaussian curve to proxy distance
    lyfit <- nls(Analyte ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=dat.lybin[n.filter==TRUE],
                  weights = n,
                  start=start.0,
                  algorithm = "port")
    if (plot == TRUE) {
      mm=unlist(dat[theta.filter==TRUE,list(min(wd3),max(wd3))])
      gauss.dat = data.frame(Ly=seq(-distance,distance,length=500))
      gauss.dat$Fit = predict(lyfit,gauss.dat)
      plot.title=ifelse(is.null(plot.title),"file.name not set",plot.title)
      # Plot Gaussian Fit
      fig2 <- ggplot(dat.lybin[n.filter==TRUE], aes(x=Ly, y=Analyte) ) +
        geom_point(col="black") +
        geom_line(data=gauss.dat,aes(y=Fit), col="red") +
        theme_bw(base_size=16) +
        xlab("Ly") +
        ylab(analyte) + ggtitle(plot.title)
      print(fig2)
    }
    setattr(dat,paste(analyte,"Ly.rot",sep="."),as.numeric(coef(lyfit)[1]))
    setattr(dat,paste(analyte,"Ly.sigma",sep="."),as.numeric(coef(lyfit)[2]))
    setattr(dat,paste(analyte,"Ly.peak",sep="."),as.numeric(coef(lyfit)[3]))
    setattr(dat,paste(analyte,"Ly.binwidth",sep="."),binwidth)
    setattr(dat,paste(analyte,"Ly.min.n",sep="."),min.n)
    setattr(dat,paste(analyte,"thetafilt",sep="."),thetafilt)
    return(dat)
}

