
corrplot <- function(data, var.names=NULL, details=TRUE, 
                     n.col.legend=5, cex.col=0.7, cex.var=0.9, digits=1, 
                     color=FALSE, abs=FALSE,...)
{
    
    # some check!
    if (is.matrix(data)|is.data.frame(data)){
    }
    else {
        stop ("Data must be a matrix or a data frame!")
    }
    if (sum(sapply(data, FUN=is.character))>0)
        stop ("Data contains non-numeric variables!")
    if (n.col.legend > 8)
        stop ("Suggestion: More than 8 levels of colors is difficult to read!")

    
    
    # prepare correlation matrix
    if (abs){
        z.plot <- abs(cor(data, data, use="pairwise.complete.obs"))
    }
    else{
        z.plot <- cor(data, data, use="pairwise.complete.obs")
    }
    if (is.null(var.names)){
        z.names <- dimnames(data)[[2]]
    }
    else{
        z.names <- var.names
    }

    # mask out the upper half
    for(i in 1:dim(z.plot)[1])
        for(j in i:dim(z.plot)[2])
            z.plot[i,j]<- NA

    # graphic windows layout 1 X 2: correlation fig & color legend
    layout(matrix(c(2,1), 1, 2, byrow=FALSE), c(10.5,1.5)) # draw on the 2nd column first
                                                       # so we can make additional change 
                                                       # on the correlation figure
                                                       
    # prepate z.breaks                                                   
     ## z.breaks <- c(min(z.plot), seq(0, max(z.plot), length=n.legend)) # if negative corr is a special case
    if (details){
        z.breaks <- sort(c(0, seq(min(z.plot, na.rm=T), max(z.plot, na.rm=T), length=n.col.legend))) 
                     # 0 is a benchmark
        for (i in 1:4){
            n1 <- length(unique(round(z.breaks, digits=digits)))
            n2 <- length(z.breaks)
        ifelse ((n1 != n2), digits <- digits + 1, digits <- digits)
        }
        if (digits > 3){
            stop ("Too many digits! Try to adjust n.col.legend to get better presentation!")  
        }
    }
    else {
        postive.z <- na.exclude(unique(round(z.plot[z.plot>0], digits=1)))
        negative.z <- na.exclude(unique(round(z.plot[z.plot<0], digits=1)))
        max.z <- max(z.plot, na.rm=T)
        min.z <- min(z.plot, na.rm=T)
        z.breaks <- sort(unique(c(postive.z, negative.z)))
        n.breaks <- length(z.breaks)
        if (n.breaks > 8){
            postive.z <- seq(0, max(postive.z), length=4)
            negative.z <- seq(min(negative.z), 0, length=4)
            z.breaks <- sort(unique(c(postive.z, negative.z)))
            n.breaks <- length(z.breaks)
            z.breaks[1] <- min.z
            z.breaks[n.breaks] <- max.z 
            n.col.legend <- length(z.breaks) - 1
        } 
        else {
            z.breaks[1] <- min.z
            z.breaks[n.breaks] <- max.z 
            n.col.legend <- length(z.breaks) - 1
        }
    }
    
    # color option    
    if (color){
        z.colors <- heat.colors(n.col.legend)[n.col.legend:1]
    }
    else {
        z.colors <- gray(n.col.legend:1/n.col.legend)
    }
    
    
    # the color legend
    par(mar=c(2, 0.1, 2, 0.1), pty="m")
    plot(c(0,1), c(min(z.breaks), max(z.breaks)),
        type="n", bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
    for(i in 2:(length(z.breaks))){
        rect(xleft=0.5, ybottom=z.breaks[i-1],
            xright=1, ytop=z.breaks[i],
            col=z.colors[i-1])
        text(x=0.45, y=z.breaks[i-1],
            labels=format(round(z.breaks[i-1], digits)),
            cex=cex.col, adj=1)
    }
    rect(xleft=0.5, ybottom=z.breaks[length(z.breaks)],
        xright=1, y=z.breaks[length(z.breaks)],
        col=z.colors[length(z.colors)])
    text(x=0.45, y=z.breaks[length(z.breaks)],
        labels=format(round(z.breaks[length(z.breaks)], digits)),
        cex=cex.col, adj=1)

    # correlation fig
    par(mar=c(0.1, 0.1, 2, 0.1), pty="m")
    image(x=1:dim(z.plot)[1], y=1:dim(z.plot)[2], z=z.plot,
        xaxt="n", yaxt="n", bty="n",
        col=z.colors,
        breaks=z.breaks,
        xlim=c(-2, dim(z.plot)[1]+0.5), ylim=c(-1, dim(z.plot)[2]+0.5),
        xlab="", ylab="")
    text(x=1:dim(z.plot)[1], y=1:dim(z.plot )[2],
        labels=z.names,
        cex=cex.var, adj=1)
}
