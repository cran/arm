\name{balanceplot}
\alias{balanceplot}

\title{Plot of Balance Statistics}
\description{
  This function plots the balance statistics before and after matching.
}
\usage{
balanceplot(matched, pscore.fit, 
    longcovnames = NULL, 
    main = "Standardized Difference in Means", 
    cex.main = 1, cex.vars = 0.8, cex.pts = 0.8, 
    mar = c(0, 5, 4, 2), mgp = c(2, 0.25, 0), 
    oma = c(0, 0, 0, 0), tcl = -0.2, ...)
}

\arguments{
  \item{matched}{matched data using \code{matching} function, see the example below.}
  \item{pscore.fit}{glm.fit object to get propensity scores.}
  \item{longcovnames}{long covariate names.  If not provided, plot will 
            use covariate variable name by default}
  \item{main}{title of the plot}
  \item{cex.main}{font size of main title}
  \item{cex.vars}{font size of variabel names}
  \item{cex.pts}{point size of the estimates}
  \item{mar}{margin of the plot, see ?\code{par} for details}
  \item{mgp}{axis margin of the plot, see ?\code{par} for details}
  \item{oma}{outer margin of the plot, see ?\code{par} for details}
  \item{tcl}{length of ticks, see ?\code{par} for details}
  \item{\dots}{other plot options may be passed to this function}
}
\details{
  This function plots the balance statistics before and after matching.  
  The open circle dots represent the unmatched balance statistics.  The 
  solid dots represent the matched balance statistics. The closer the value
  of the estimates to the zero, the better the treated and control groups are 
  balanced after matching.
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
data(lalonde)
attach(lalonde)
fit <- glm(treat ~ re74 + re75 + age + factor(educ) + 
            black + hisp + married + nodegr + u74 + u75, 
            family=binomial(link="logit"))
pscores <- predict(fit, type="link")
matches <- matching(z=lalonde$treat, score=pscores)
matched <- lalonde[matches$matched,]  
balanceplot(matched, fit)
}


\keyword{methods}
\keyword{dplot}
