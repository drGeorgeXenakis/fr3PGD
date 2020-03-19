\name{EstimateAPAR}
\alias{EstimateAPAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate APAR
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EstimateAPAR(state, weather, parms, general.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{weather}{
%%     ~~Describe \code{weather} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{general.info}{
%%     ~~Describe \code{general.info} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (state, weather, parms, general.info) 
{
    molPAR_MJ <- parms[["molPAR_MJ"]]
    fullCanAge <- parms[["fullCanAge"]]
    k <- parms[["k"]]
    RAD.day <- weather[["SolarRad"]]
    year <- weather[["Year"]]
    month <- weather[["Month"]]
    t <- state[["t"]]
    LAI <- state[["LAI"]]
    RAD <- RAD.day * general.info$daysinmonth[month]
    phi.p <- molPAR_MJ * RAD
    CanCover <- if (fullCanAge > 0 & t < fullCanAge) 
        (t + 0.01)/fullCanAge
    else 1
    lightIntcptn <- 1 - exp(-k * LAI/CanCover)
    phi.pa <- phi.p * lightIntcptn * CanCover
    state[c("Year", "Month", "RAD.day", "RAD", "CanCover", "lightIntcptn", 
        "phi.p", "phi.pa")] <- c(year, month, RAD.day, RAD, CanCover, 
        lightIntcptn, phi.p, phi.pa)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
