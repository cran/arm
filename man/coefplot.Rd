\name{coefplot}
\alias{coefplot}

\title{Function for Making Coefficient Plot }
\description{
 A function that plot the coefficients from a lm or glm fit
}
\usage{
coefplot(fit, longnames=NULL, 
         x.label="", y.label="", main.label="", 
         intercept=FALSE, varnames=TRUE, display.fit=FALSE, 
         cex.var=0.8, cex.pts=0.9,...)
}

\arguments{
  \item{fit}{fitted objects-lm or glm}
  \item{longnames}{long variable names (vector). 
                    If not provided, use default variable names}
  \item{x.label}{label for x-axis}
  \item{y.label}{label for y-axis }
  \item{main.label}{label for the whole plot }
  \item{intercept}{If TRUE will plot intercept, default=FALSE }
  \item{varnames}{ If FALSE will not plot variable names, default=TRUE}
  \item{display.fit}{If TRUE will display fitted output, default=FALSE}
  \item{cex.var}{The fontsize of the varible names, default=0.8}
  \item{cex.pts}{The size of data points, default=0.9}
  \item{...}{Other plot options may be passed to this function}

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
         \code{\link{glm}}
        }
\examples{
 y1 <- rnorm(1000,50,23)
 y2 <- rbinom(1000,1,prob=0.72)
 x1 <- rnorm(1000,50,2) 
 x2 <- rbinom(1000,1,prob=0.63) 
 x3 <- rpois(1000, 2) 
 x4 <- runif(1000,40,100) 
 x5 <- rbeta(1000,2,2) 
 x6 <- rnorm(1000,100,30)
 x7 <- rpois(1000,10) 
 x8 <- rbinom(1000,1,prob=0.4) 
 x9 <- rbeta(1000,5,4) 
 x10 <- runif(1000,-10,-1) 
 longnames <- c("a long name01","a long name02","a long name03",
                "a long name04","a long name05","a long name06",
                "a long name07","a long name08","a long name09",
                "a long name10")

 fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 +x5 +x6 +x7 +x8 + x9 + x10)
 fit2 <- glm(y2 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 
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
}

\keyword{dplot}
