coefplot <- function(fit,
                     longnames=NULL, 
                     x.label="", y.label="", main.label="", 
                     intercept=FALSE, varnames=TRUE, display.fit=FALSE, 
                     cex.var=0.8, cex.pts=0.9, ...)
{
                            
    object.class <- class(fit)[[1]]
    if (object.class=="lm" | object.class=="glm"){
    # collect informations
    coef <- summary(fit)$coef[,1]
    n.x <- length(coef)
    sd <- summary(fit)$coef[,2][1:(n.x)]
    if (intercept){
        idx <- seq(1, n.x)
        if (is.null(longnames)){
            longnames <- names(coef)
        }
        else{
            longnames <- c("(Intercept)", longnames)       
        }
    }
    else{
        if (is.null(longnames)){
            longnames <- na.exclude(names(coef)[-1])
        }
        else {
            longnames <- longnames           
        }
        idx <- seq(1, n.x-1)
        coef <- na.exclude(coef[-1][1:n.x])
        sd <- na.exclude(sd[-1][1:n.x]) 
    }                                                                   
    
    # display options
    if (display.fit){
        display(fit)
    }

    # create x.aixs.scale and y.axis.scale
    min.x <- round(range(coef-2*sd)[1], 1)-0.1                               
    max.x <- round(range(coef+2*sd)[2], 1)+0.1                                 
    x.scale <- c(min.x, max.x)                                            
    y.scale <- c((n.x+0.5), 0.5)                                          

    #=========size and split the devices==================================
    #layout(matrix(c(1,2),1,2),c(3,7))
    #====plot varnames====================================================
    #par(mar=c(3.5,2,2,0.5), mgp=c(2,0.5,0))                              
    #plot (coef, idx, axes=F, type="n",                                   
    #    xlab="", ylab="", main="", ylim=y.scale)                         
    #axis(4, 1:n.x, longnames[1:n.x], las=2, tck=FALSE, lty=0, hadj=1, cex=cex01)
    #=====================================================================
    
    # plotting!!
    par(mgp=c(2,0.5,0), tcl=-0.2)                 
    plot(coef, idx, axes=F, type="n",                                     
        xlim=x.scale, ylim=y.scale, 
        xlab=x.label, ylab=y.label,                   
        main=main.label)                                                        
    axis(1)                                
    axis(3)
    abline(v=0, lty=2)                                                 
    points(coef, idx, pch=19, cex=cex.pts)
    segments (coef+sd, idx, coef-sd, idx, lwd=2)     
    segments (coef+2*sd, idx, coef-2*sd, idx, lwd=1)
    
    # plot variable names or not
    if (varnames){
    axis(2, 1:n.x, longnames[1:n.x], las=2, tck=FALSE, lty=0, hadj=1, cex.axis=cex.var)  
    }}
    else if (object.class=="lmer" | object.class=="glmer"){
            stop(message = "lmer class has not been implemented!\n")
    }
    else if (object.class=="bugs") {
        stop(message = "bugs class has not been implemented!\n")
    } 
}                                                                         
