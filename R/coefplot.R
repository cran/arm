coefplot.default <- function (object,
                longnames=NULL, 
                xlim=NULL, ylim=NULL, 
                xlab="", ylab="", main="",   
                intercept=FALSE, varnames=TRUE, 
                cex.var=0.8, cex.pts=0.9, col.pts=1)
{
    # collect informations
    coefs <- summary(object)$coef[,1]
    n.x <- length(coefs)
    sd <- summary(object)$coef[,2][1:(n.x)]
    if (intercept){
        idx <- seq(1, n.x)
        if (is.null(longnames)){
            longnames <- names(coefs)
        }
        else{
            longnames <- c("(Intercept)", longnames)       
        }
    }
    else{
        if (is.null(longnames)){
            longnames <- na.exclude(names(coefs)[-1])
        }
        else {
            longnames <- longnames           
        }
        idx <- seq(1, n.x-1)
        coefs <- na.exclude(coefs[-1][1:n.x])
        sd <- na.exclude(sd[-1][1:n.x]) 
    }                                                                   
    

    # create x.aixs.scale and y.axis.scale
    if (is.null(xlim)){
        min.x <- round(range(coefs-2*sd)[1], 1)-0.1                               
        max.x <- round(range(coefs+2*sd)[2], 1)+0.1                                 
        xlim <- c(min.x, max.x)                                                                                    
    }
    
    if (is.null(ylim)){
        ylim <- c((n.x+0.5), 0.5)  
    }
    
    plot(coefs, idx, type="n",                                     
        xlim=xlim, ylim=ylim, 
        xlab=xlab, ylab=ylab,                   
        main=main, axes=F)                                                   
    axis(1)                                
    axis(3)
    abline(v=0, lty=2)                                                 
    points(coefs, idx, pch=19, cex=cex.pts, col=col.pts)
    segments (coefs+sd, idx, coefs-sd, idx, lwd=2, col=col.pts)     
    segments (coefs+2*sd, idx, coefs-2*sd, idx, lwd=1, col=col.pts)
    
    # plot variable names or not
    if (varnames){
        axis(2, 1:n.x, longnames[1:n.x], las=2, tck=FALSE, 
            lty=0, hadj=1, cex.axis=cex.var)  
    }
}                                                                         

coefplot.bugs <- function (object,
                longnames=NULL, 
                xlim=NULL, ylim=NULL, 
                xlab="", ylab="", main="",   
                varnames=TRUE, 
                cex.var=0.8, cex.pts=0.9, col.pts=1)
{
# collect informations
    n.x <- length(object$summary[,"50%"])
    coefs <- object$summary[,"50%"][1:(n.x-1)]
    CI50 <- array(c(object$summary[,"25%"], object$summary[,"75%"]), c(n.x,2))[1:(n.x-1),]
    CI95 <- array(c(object$summary[,"2.5%"], object$summary[,"97.5%"]), c(n.x,2))[1:(n.x-1),]

    idx <- seq(1, n.x-1)
    if (is.null(longnames)){
        longnames <- names(coefs)
    }
                                                        
    # create x.aixs.scale and y.axis.scale
    if (is.null(xlim)){
        min.x <- round(range(CI95[,1])[1], 1)-0.1                               
        max.x <- round(range(CI95[,2])[2], 1)+0.1                                 
        xlim <- c(min.x, max.x)                                                                                    
    }
    
    if (is.null(ylim)){
        ylim <- c((n.x-1+0.5), 0.5)  
    }
    
    plot(coefs, idx, type="n",                                     
        xlim=xlim, ylim=ylim, 
        xlab=xlab, ylab=ylab,                   
        main=main, axes=F)                                                   
    axis(1)                                
    axis(3)
    abline(v=0, lty=2)                                                 
    points(coefs, idx, pch=19, cex=cex.pts, col=col.pts)
    segments (CI50[,1], idx, CI50[,2], idx, lwd=2, col=col.pts)     
    segments (CI95[,1], idx, CI95[,2], idx, lwd=1, col=col.pts)
    
    # plot variable names or not
    if (varnames){
        axis(2, 1:(n.x-1), longnames[1:(n.x-1)], las=2, tck=FALSE, 
            lty=0, hadj=1, cex.axis=cex.var)  
    }
}
