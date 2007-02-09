\name{coefplot}
\docType{genericFunction}
\alias{coefplot}
\alias{coefplot,numeric-method}
\alias{coefplot,lm-method}
\alias{coefplot,glm-method}
\alias{coefplot,bugs-method}
\alias{coefplot,polr-method}
\alias{coefplot.default}
\alias{coefplot.lm}
\alias{coefplot.glm}
\alias{coefplot.bugs}
\alias{coefplot.polr}

\title{Generic Function for Making Coefficient Plot}
\description{
 Functions that plot the coefficients $\pm$ 1 and 2 sd 
 from a lm, glm, bugs, and polr fits.
}
\usage{
coefplot(object, ...)

coefplot.default(coefs, sds, 
                varnames=NULL, CI=2, vertical=TRUE, 
                xlim=NULL, ylim=NULL, 
                xlab="", ylab="", main="",   
                cex.var=0.8, cex.pts=0.9, col.pts=1,
                var.las=2)

# methods for coefplot()           
coefplot.lm (object, varnames=NULL, intercept=FALSE, ...)
coefplot.glm (object, varnames=NULL, intercept=FALSE, ...)
coefplot.polr (object, varnames=NULL, ...)
coefplot.bugs (object, varnames=NULL, CI=2,
               xlim=NULL, ylim=NULL, 
               xlab="", ylab="", main="",   
               cex.var=0.8, cex.pts=0.9, col.pts=1)
}

\arguments{
  \item{object}{fitted objects-lm, glm, bugs and polr, or a vector of coefficients.}
  \item{...}{further arguments passed to or from other methods.}
  \item{coefs}{a vector of coefficients.}
  \item{sds}{a vector of sds of coefficients.}
  \item{varnames}{a vector of variable names, default is NULL, which will use
    the names of variables.}
  \item{CI}{confidence interval, default is 2, which will plot $\pm 2$ sds or
    95\% CI. If CI=1, plot $\pm 1$ sds or 50\% CI instead.}
  \item{vertical}{orientation of the plot, default is TRUE which will plot
    variable names in the 2nd axis. If FALSE, plot variable names in the first
    axis instead.}
  \item{xlim}{the x limits (x1, x2) of the plot.  Note that 'x1 > x2' is
          allowed and leads to a "reversed axis".}
  \item{ylim}{the y limits of the plot.}
  \item{xlab}{a label for the x axis, default is "".}
  \item{ylab}{a label for the y axis, default is "".}
  \item{main}{a main title for the plot, default is "". See also \code{title}.} 
  \item{cex.var}{The fontsize of the varible names, default=0.8.}
  \item{cex.pts}{The size of data points, default=0.9.}
  \item{col.pts}{color of points and segments, default is black.}
  \item{var.las}{the orientation of variable names against the axis, default is 2. 
    see the usage of \code{las} in \code{\link{par}}.}
  \item{intercept}{If TRUE will plot intercept, default=FALSE to get better presentation.}
}
\details{
  This function plots coefficients from lm, glm and polr
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
 coefplot(fit2, col.pts="blue")
 
 
 # plot 2
 par (mar=c(2,8,2,0.5))
 coefplot(fit1, longnames, intercept=TRUE, CI=1)
 
 # plot 3
 par (mar=c(2,2,2,2))
 coefplot(fit2, vertical=FALSE, var.las=1)
 
 # plot 4: comparison to show bayesglm works better than glm
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
 
    coefplot(M2, xlim=c(-1,5), intercept=TRUE)
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

# plot 5: the ordered logit model from polr
 M3 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
 par (mar=c(2,8,2,0.5))
 coefplot(M3)
   
 M4 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
 par (mar=c(2,8,2,0.5))
 coefplot(M4)

# plot 6: plot bugs 
 M5 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
 M5.sim <- mcsamp(M5)
 coefplot(M5.sim)
 
# plot 7: plot coefficients & sds vectors
 coef.vect <- c(0.2, 1.4, 2.3, 0.5)
 sd.vect <- c(0.12, 0.24, 0.23, 0.15)
 longnames <- c("var1", "var2", "var3", "var4")
 coefplot (coef.vect, sd.vect, longnames)
 coefplot (coef.vect, sd.vect, longnames, vertical=FALSE, var.las=1)
}

\keyword{dplot}
\keyword{methods}
