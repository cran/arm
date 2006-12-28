balanceplot <- function (rawdata, matched, pscore.fit, 
                longcovnames=NULL, 
                main="Standardized Difference in Means",
                cex.main=1, cex.vars=0.8, cex.pts=0.8,
                mar=c(0, 15, 4, 2), mgp=c(2, 0.25, 0), 
                oma=c(0,0,0,0), tcl=-0.2,
                ...)
{
                
    treat <- deparse(pscore.fit$formula[-1][[1]])
    covnames <- dimnames(summary(pscore.fit)$coef)[[1]]
    covnames <- covnames[-1]

    # diff.mean.rawdata
    
    cat("\n=========diff.mean.rawdata==========\n")
    diff.means=matrix(0,length(covnames),6)
    for(i in 1:length(covnames)){
        diff.means[i,1:2] <- c(mean(rawdata[(rawdata[,treat]==1),covnames[i]]),
            mean(rawdata[(rawdata[,treat]==0),covnames[i]]))
        diff.means[i,3] <- diff.means[i,1]-diff.means[i,2]
        diff.means[i,5] <- sqrt(var(rawdata[(rawdata[,treat]==1),covnames[i]])/
            sum((rawdata[,treat]==1)) + var(rawdata[(rawdata[,treat]==0),covnames[i]])/sum((rawdata[,treat]==0)))
        diff.means[i,6] <- sqrt((var(rawdata[(rawdata[,treat]==1),covnames[i]])+
            var(rawdata[(rawdata[,treat]==0),covnames[i]]))/2)
        diff.means[i,4] <- diff.means[i,3]/diff.means[i,6]
    }
    dimnames(diff.means) <- list(covnames,
        c("Treat","control","diff","diff.std","se","sd"))
    print(round(diff.means,2))
    
    # diff.means.matched
    cat("\n=========diff.mean.matched.data==========\n")
    diff.means.matched=matrix(0,length(covnames),6)
    for(i in 1:length(covnames)){
        diff.means.matched[i,1:2] <- c(mean(matched[(matched[,treat]==1),
            covnames[i]]),mean(matched[(matched[,treat]==0),covnames[i]]))
        diff.means.matched[i,3] <- diff.means.matched[i,1]-diff.means.matched[i,2]
        diff.means.matched[i,5] <- sqrt(var(matched[(matched[,treat]==1),covnames[i]])/
            sum((rawdata[,treat]==1)) + var(rawdata[(rawdata[,treat]==0),covnames[i]])/sum((rawdata[,treat]==0)))
        diff.means.matched[i,6] <- sqrt((var(rawdata[(rawdata[,treat]==1),covnames[i]])+
            var(rawdata[(rawdata[,treat]==0),covnames[i]]))/2)
        diff.means.matched[i,4] <- diff.means.matched[i,3]/diff.means.matched[i,6]
    }

    dimnames(diff.means.matched) <- list(covnames,
        c("Treat","control","diff","diff.std","se","sd"))
    print(round(diff.means.matched,2))

    # prepare for plot use
    
    est <- diff.means[,3]
    sd <- diff.means[,6]
    est2 <- diff.means.matched[,3]
    sd2 <- diff.means.matched[,6]
    
    # x.range <- range (c(est,est2)/c(sd,sd2))
    # x.range[2] <- x.range[2] +.3
    # A <- -x.range[1]/(x.range[2]-x.range[1])
    # B <- 1/(x.range[2]-x.range[1])
    # pts <- A + B*(est/sd)              # before matched
    # pts2 <- A + B*(est2/sd2)           # after macthed
    
    pts <-  est/sd                      # before matched
    pts2 <- est2/sd2                    # after macthed
    x.range <- c( min(c(pts, pts2))-.2, max(c(pts,pts2)+.2) )
    idx <- 1:length(covnames)
    
    # tune the graphic console
    par (mar=mar, mgp=mgp, oma=oma, tcl=tcl)
    
    # plot the estimates
    plot(pts, idx,
    bty="n", xlab="", ylab="",
    xaxt="n", yaxt="n", xaxs="i", 
    yaxs="i", type="n",
    ylim=c(0,length(covnames)+0.5),
    xlim=x.range,
    main=main, cex.main=cex.main)
    abline(v=0, lty=2)
    points(pts, idx, cex=cex.pts)          # before matched
    points(pts2, idx, pch=19, cex=cex.pts) # after macthed
    axis(3, cex.axis=0.8)
    if (is.null(longcovnames)){
        axis(2, at=length(covnames):1, labels=covnames, 
            las=2, hadj=1, lty=0, cex.axis=cex.vars)
    }
    else{
        axis(2, at=length(covnames):1, labels=longcovnames, 
            las=2, hadj=1, lty=0, cex.axis=cex.vars)
    }
}
