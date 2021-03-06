\name{hdr}
\alias{hdr}
\alias{hdr.boxplot}
\alias{hdr.den}
\title{Highest Density Regions}
\description{
Calculates and plots highest density regions in one dimension including the HDR boxplot.
}
\usage{
hdr(x, prob = c(50, 95, 99), den, h=hdrbw(BoxCox(x,lambda),mean(prob)), lambda=1, 
    nn=5000, all.modes=FALSE)
hdr.den(x, prob = c(50, 95, 99), den, h=hdrbw(BoxCox(x,lambda),mean(prob)), lambda=1, 
    xlab=NULL, ylab="Density", ...)
hdr.boxplot(x, prob = c(99, 50), h=hdrbw(BoxCox(x,lambda),mean(prob)), lambda=1, 
    boxlabels = "", col = gray((9:1)/10), main="", xlab="", ylab="", pch=1, ...)
}
\arguments{
  \item{x}{Numeric vector containing data. In \code{hdr} and \code{hdr.den},
    if \code{x} is missing then \code{den} must be provided, and the
    HDR is computed from the given density.
    For \code{hdr.boxplot}, \code{x} can be a list containing several vectors.}
  \item{prob}{Probability coverage required for HDRs}
  \item{den}{Density of data as list with components \code{x} and \code{y}.
    If omitted, the density is estimated from \code{x} using \code{\link[stats]{density}}.}
  \item{h}{Optional bandwidth for calculation of density.}
  \item{lambda}{Box-Cox transformation parameter where \code{0 <= lambda <= 1}.}
  \item{nn}{Number of random numbers used in computing f-alpha quantiles.}
  \item{all.modes}{Return all local modes or just the global mode?}
  \item{boxlabels}{Label for each box plotted.}
  \item{col}{Colours for regions of each box.}
  \item{main}{Overall title for the plot.}
  \item{xlab}{Label for x-axis.}
  \item{ylab}{Label for y-axis.}
  \item{pch}{Plotting character.}
  \item{\dots}{Other arguments passed to plot.}
}
\details{Either \code{x} or \code{den} must be provided. When \code{x} is provided, the density is estimated using
kernel density estimation. A Box-Cox transformation is used if \code{lambda!=1}, as described in Wand, Marron and 
Ruppert (1991). This allows the density estimate to be non-zero only on the positive real line. The default kernel bandwidth \code{h} is 
selected using the algorithm of Samworth and Wand (2010).

Hyndman's (1996) density quantile algorithm is used for calculation. \code{hdr.den} plots the density with the HDRs 
superimposed. \code{hdr.boxplot} displays a boxplot based on HDRs.
}
\value{\code{hdr.boxplot} retuns nothing. \code{hdr} and \code{hdr.den} return a list of three components:
  \item{hdr}{The endpoints of each interval in each HDR}
  \item{mode}{The estimated mode of the density.}
  \item{falpha}{The value of the density at the boundaries of each HDR.}
}
\references{Hyndman, R.J. (1996) Computing and graphing highest density regions.
\emph{American Statistician}, \bold{50}, 120-126.

 Samworth, R.J. and Wand, M.P. (2010). Asymptotics and optimal bandwidth 
  selection for highest density region estimation. 
  \emph{The Annals of Statistics}, \bold{38}, 1767-1792.
  
Wand, M.P., Marron, J S., Ruppert, D. (1991) Transformations in density estimation.
\emph{Journal of the American Statistical Association}, \bold{86}, 343-353.
}

\author{Rob J Hyndman}

\seealso{\code{\link{hdr.boxplot.2d}}}
\examples{
# Old faithful eruption duration times
hdr(faithful$eruptions)
hdr.boxplot(faithful$eruptions)
hdr.den(faithful$eruptions)

# Simple bimodal example
x <- c(rnorm(100,0,1), rnorm(100,5,1))
par(mfrow=c(1,2))
boxplot(x)
hdr.boxplot(x)
par(mfrow=c(1,1))
hdr.den(x)

# Highly skewed example
x <- exp(rnorm(100,0,1))
par(mfrow=c(1,2))
boxplot(x)
hdr.boxplot(x,lambda=0)
}
\keyword{smooth}
\keyword{distribution}
\keyword{hplot}
