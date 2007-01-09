\name{coefplot}
\alias{coefplot}

\title{Function for Making Coefficient Plot }
\description{
 A function that plot the coefficients from a lm or glm fit
}
\usage{
coefplot(fit, longnames=NULL,
        xlim=NULL, ylim=NULL, 
        xlab="", ylab="", main="", ...,   
        intercept=FALSE, varnames=TRUE, display.fit=FALSE, 
        cex.var=0.8, cex.pts=0.9)
}

\arguments{
  \item{fit}{fitted objects-lm or glm.}
  \item{longnames}{long variable names (vector). 
                    If not provided, use default variable names.}
  \item{xlim}{the x limits (x1, x2) of the plot.  Note that 'x1 > x2' is
          allowed and leads to a "reversed axis".}
  \item{ylim}{the y limits of the plot}
  \item{xlab}{a label for the x axis, default is "".}
  \item{ylab}{a label for the y axis, default is "".}
  \item{main}{a main title for the plot, default is "". See also 'title'.}
  \item{...}{other plot options may be passed to this function, see 
    \code{\link{plot.default}} and \code{\link{par}}.}
  \item{intercept}{If TRUE will plot intercept, default=FALSE.}
  \item{varnames}{ If FALSE will not plot variable names, default=TRUE.}
  \item{display.fit}{If TRUE will display fitted output, default=FALSE.}
  \item{cex.var}{The fontsize of the varible names, default=0.8.}
  \item{cex.pts}{The size of data points, default=0.9.}
  

}
\details{
  This function plots coefficients from lm and glm 
  with 1 sd and 2 sd interval bars.
}
\value{
   Plot of the coefficients from a lm or glm fit. You can add the intercept, 
   the variable names and the display the result of the fitted model. 
}
\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}
            
\author{Yu-Sung Su \email{ys463@columbia.edu}
}
        
\seealso{\code{\link{display}},
         \code{\link{par}},
         \code{\link{lm}},
         \code{\link{glm}},
         \code{\link{bayesglm}}
        }
\examples{
 y1 <- rnorm(1000,50,23)
 y2 <- rbinom(1000,1,prob=0.72)
 x1 <- rnorm(1000,50,2) 
 x2 <- rbinom(1000,1,prob=0.63) 
 x3 <- rpois(1000, 2) 
 x4 <- runif(1000,40,100) 
 x5 <- rbeta(1000,2,2) 
 
 longnames <- c("a long name01","a long name02","a long name03",
                "a long name04","a long name05")

 fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5)
 fit2 <- glm(y2 ~ x1 + x2 + x3 + x4 + x5, 
            family=binomial(link="logit"))
 
 # plot 1
 par (mfrow=c(2,2))
 coefplot(fit1)
 coefplot(fit2)
 
 
 # plot 2
 par (mar=c(2,8,2,0.5))
 coefplot(fit1, longnames=longnames, display=TRUE)
 
 # plot 3
 par (mar=c(2,2,2,2))
 coefplot(fit2, longnames=longnames, varnames=FALSE)
 
 # plot comparison to show bayesglm works better than glm
 n <- 100
 x1 <- rnorm (n)
 x2 <- rbinom (n, 1, .5)
 b0 <- 1
 b1 <- 1.5
 b2 <- 2
 y <- rbinom (n, 1, invlogit(b0+b1*x1+b2*x2))
 y <- ifelse (x2==1, 1, y)
 x1 <- rescale(x1)
 x2 <- rescale(x2, "center")
 
 M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
       display (M1)
 M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"))
       display (M2)
 
 ## stacked plot
 par(mar=c(2,5,3,1), mgp=c(2,0.25,0), oma=c(0,0,2,0), tcl=-0.2)
 
 coefplot(M2, xlim=c(0,7.5), intercept=TRUE)
 points(coef(M1), c(3:1)-0.1, col="red", pch=19)
 segments(coef(M1) + se.coef(M1), c(3:1)-0.1,
    coef(M1) - se.coef(M1), c(3:1)-0.1, lwd=2, col="red")
 segments(coef(M1) + 2*se.coef(M1), c(3:1)-0.1,
    coef(M1) - 2*se.coef(M1), c(3:1)-0.1, col="red")    
 mtext("Coefficients", side=3, at=0.1, outer=TRUE)
 mtext("Estimate", side=3, at=0.6, outer=TRUE)
 
 ## arrayed plot
 par(mfrow=c(1,2), mar=c(2,5,5,1), mgp=c(2,0.25,0), tcl=-0.2)
 x.scale <- c(0, 7.5) # fix x.scale for comparison
 
 coefplot(M1, xlim=x.scale, main="glm", intercept=TRUE)
 coefplot(M2, xlim=x.scale, main="bayesglm", intercept=TRUE) 
}

\keyword{dplot}
