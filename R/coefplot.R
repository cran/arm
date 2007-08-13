#coefplot <- function(object,...) UseMethod("coefplot","ANY")

#coefplot.coefplot <- function(object,...) object

coefplot.default <- function(coefs, sds, 
            varnames=NULL, CI=2, vertical=TRUE,
            cex.var=0.8, cex.pts=0.9, col.pts=1,
            var.las=2,...)
{
     # collect informations
    if (is.list(coefs)){
        coefs <- unlist(coefs)
    }
    n.x <- length(coefs)
    idx <- seq(1, n.x)                                                                
  
    if (vertical){
        plot(c(coefs+2*sds, coefs-2*sds), c(idx,idx), type="n",                                     
            axes=F,...)                                                   
        axis(1)                                
        axis(3)
        axis(2, n.x:1, varnames[n.x:1], las=var.las, tck=FALSE, 
            lty=0, cex.axis=cex.var) 
        abline(v=0, lty=2)                                                 
        points(coefs, idx, pch=19, cex=cex.pts, col=col.pts)
        if (CI==2){
            segments (coefs+sds, idx, coefs-sds, idx, lwd=2, col=col.pts)     
            segments (coefs+2*sds, idx, coefs-2*sds, idx, lwd=1, col=col.pts)
        }
        else{
            segments (coefs+sds, idx, coefs-sds, idx, lwd=1, col=col.pts)    
        }
    }
    else{ # horizontal
        plot(c(idx,idx), c(coefs+2*sds, coefs-2*sds), type="n", axes=F,...)                                                   
        axis(2)                                
        axis(4)
        axis(1, 1:n.x, varnames[1:n.x], las=var.las, tck=FALSE, 
            lty=0, cex.axis=cex.var) 
        abline(h=0, lty=2)                                                 
        points(idx, coefs, pch=19, cex=cex.pts, col=col.pts)
        if (CI==2){
            segments (idx, coefs+sds, idx, coefs-sds, lwd=2, col=col.pts)     
            segments (idx, coefs+2*sds, idx, coefs-2*sds, lwd=1, col=col.pts)
        }
        else if (CI==1) {
            segments (idx, coefs+sds, idx, coefs-sds, lwd=1, col=col.pts)     
        }
    }   
}

setMethod("coefplot", signature(object = "numeric"), 
    function(object, ...)
    {
    coefplot.default(object, ...)
    }
)
    


setMethod("coefplot", signature(object = "lm"), 
    function(object, varnames=NULL, intercept=FALSE, ...)
    {
    # collect informations
    coefs <- summary(object)$coef[,1]
    sds <- summary(object)$coef[,2]
    ifelse (is.null(varnames), varnames <- names(coefs),
            varnames <- varnames)
    if (intercept){
        coefs <- coefs
        sds <- sds
        varnames <- varnames
    }
    else{
        coefs <- coefs[-1]
        sds <- sds[-1]
        varnames <- varnames[-1]
    }
    
    
    # plotting
    coefplot(coefs, sds, 
        varnames=varnames, ...)
    }
)
           
setMethod("coefplot", signature(object = "glm"),
    function(object, varnames=NULL, intercept=FALSE,...)
    {
    # collect informations
    coefs <- summary(object)$coef[,1]
    sds <- summary(object)$coef[,2]
    ifelse (is.null(varnames), varnames <- names(coefs),
            varnames <- varnames)
    if (intercept){
        coefs <- coefs
        sds <- sds
        varnames <- varnames
    }
    else{
        coefs <- coefs[-1]
        sds <- sds[-1]
        varnames <- varnames[-1]
    }
    
    
    # plotting
    coefplot(coefs, sds, 
        varnames=varnames, ...)
    }                                                                         
)


setMethod("coefplot", signature(object = "bugs"),
    function(object,
            varnames=NULL, CI=2, 
            cex.var=0.8, cex.pts=0.9, col.pts=1,...)
    {
    # collect informations
    n.x <- length(object$summary[,"50%"])
    coefs <- object$summary[,"50%"][1:(n.x-1)]
    CI50 <- array(c(object$summary[,"25%"], object$summary[,"75%"]), c(n.x,2))[1:(n.x-1),]
    CI95 <- array(c(object$summary[,"2.5%"], object$summary[,"97.5%"]), c(n.x,2))[1:(n.x-1),]
    idx <- seq(1, n.x-1)

    if (is.null(varnames)) varnames <- names(coefs)     
    
    # plotting
    plot(c(CI95[,1],CI95[,2]), c(idx,idx), type="n",                                     
        axes=F,...)                                                   
    axis(1)                                
    axis(3)
    axis(2, n.x:1, varnames[n.x:1], las=2, tck=FALSE, 
        lty=0, cex.axis=cex.var)  
    abline(v=0, lty=2)                                                 
    points(coefs, idx, pch=19, cex=cex.pts, col=col.pts)
    if (CI==2){
        segments (CI50[,1], idx, CI50[,2], idx, lwd=2, col=col.pts)     
        segments (CI95[,1], idx, CI95[,2], idx, lwd=1, col=col.pts)
    }
    else if (CI==1){
        segments (CI50[,1], idx, CI50[,2], idx, lwd=1, col=col.pts)     
    }
    }
)


setMethod("coefplot", signature(object = "polr"), 
    function(object, varnames=NULL,...)
    {
    # collect informations
    coefs <- summary(object)$coef[,1]
    sds <- summary(object)$coef[,2]
    ifelse(is.null(varnames), varnames <- names(coefs), 
        varnames <- varnames)

    # plotting
    coefplot(coefs, sds, 
        varnames=varnames, ...)
    }
)                                                                       
