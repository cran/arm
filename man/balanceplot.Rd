\name{balanceplot}
\alias{balanceplot}

\title{Plot of Balance Statistics}
\description{
  This function plots the balance statistics before and after matching.
}
\usage{
balanceplot(rawdata, matched, pscore.fit, 
    longcovnames = NULL, factor=TRUE,
    main = "Standardized Difference in Means", 
    cex.main = 1, cex.vars = 0.8, cex.pts = 0.8, ...)
}

\arguments{
  \item{rawdata}{data before using \code{matching} function, see the example below.}
  \item{matched}{matched data using \code{matching} function, see the example below.}
  \item{pscore.fit}{glm.fit object to get propensity scores.}
  \item{longcovnames}{long covariate names.  If not provided, plot will 
            use covariate variable name by default}
  \item{factor}{default is TRUE which will display the factorized categorical variables. In
        a situation where no equal levels of factorized categorical variables is observed,
        use factor=FALSE to proceed.}
  \item{main}{title of the plot}
  \item{cex.main}{font size of main title}
  \item{cex.vars}{font size of variabel names}
  \item{cex.pts}{point size of the estimates}
  \item{\dots}{other plot options may be passed to this function}
}
\details{
  This function plots the balance statistics before and after matching.  
  The open circle dots represent the unmatched balance statistics.  The 
  solid dots represent the matched balance statistics. The closer the value
  of the estimates to the zero, the better the treated and control groups are 
  balanced after matching.
}

\note{ 
 The function does not work with predictors that contain factor(x), log(x) or all
 other data transformation. Create new objects for these variables.  Attach
 them into the original dataset before doing the matching procedure.
}

\references{Andrew Gelman and Jennifer Hill, Data Analysis Using 
Regression and Multilevel/Hierarchical Models, Cambridge University Press, 2006. 
(Chater 10)
}

\author{Jennifer Hill \email{jh1030@columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu}
        }

\seealso{\code{\link{matching}},
        \code{\link{par}}
        }
        
\examples{
# matching first
data(lalonde)
attach(lalonde)
fit <- glm(treat ~ re74 + re75 + age + factor(educ) + 
            black + hisp + married + nodegr + u74 + u75, 
            family=binomial(link="logit"))
pscores <- predict(fit, type="link")
matches <- matching(z=lalonde$treat, score=pscores)
matched <- lalonde[matches$matched,]

# make the plot
par(mar=c(0,6,4,2),mgp=c(2,0.25,0),tcl=-0.2)  
balanceplot(lalonde, matched, fit, xlim=c(-0.3,0.2))
}


\keyword{methods}
\keyword{models}
\keyword{dplot}
