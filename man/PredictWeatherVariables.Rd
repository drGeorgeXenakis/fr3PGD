\name{PredictWeatherVariables}
\alias{PredictWeatherVariables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predicted weather variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PredictWeatherVariables(weather)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weather}{
%%     ~~Describe \code{weather} here~~
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
function (weather) 
{
    VPD.Tmax <- 6.1078 * exp(17.269 * weather$Tmax/(237.3 + weather$Tmax))
    VPD.Tmin <- 6.1078 * exp(17.269 * weather$Tmin/(237.3 + weather$Tmin))
    VPD <- (VPD.Tmax - VPD.Tmin)/2
    weather$VPDx <- VPD.Tmax
    weather$VPDn <- VPD.Tmin
    weather$VPD <- VPD
    return(weather)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
