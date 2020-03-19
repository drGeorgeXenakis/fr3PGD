\name{GetDayLength}
\alias{GetDayLength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the day length
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetDayLength(Lat, month, dayofyear = c(Jan = 16, Feb = 44, Mar = 75, Apr = 105, May = 136, Jun = 166, Jul = 197, Aug = 228, Sep = 258, Oct = 289, Nov = 319, Dec = 350))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Lat}{
%%     ~~Describe \code{Lat} here~~
}
  \item{month}{
%%     ~~Describe \code{month} here~~
}
  \item{dayofyear}{
%%     ~~Describe \code{dayofyear} here~~
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
function (Lat, month, dayofyear = c(Jan = 16, Feb = 44, Mar = 75, 
    Apr = 105, May = 136, Jun = 166, Jul = 197, Aug = 228, Sep = 258, 
    Oct = 289, Nov = 319, Dec = 350)) 
{
    SLAt <- sin(pi * Lat/180)
    cLat <- cos(pi * Lat/180)
    sinDec <- 0.4 * sin(0.0172 * (dayofyear[month] - 80))
    cosH0 <- -sinDec * SLAt/(cLat * sqrt(1 - sinDec^2))
    if (cosH0 > 1) {
        fDayLength <- 0
    }
    else if (cosH0 < -1) {
        fDayLength <- 1
    }
    else {
        fDayLength <- acos(cosH0)/pi
    }
    daylength <- fDayLength * 86400
    return(daylength)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
