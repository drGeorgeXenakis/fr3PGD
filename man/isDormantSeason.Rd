\name{isDormantSeason}
\alias{isDormantSeason}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check dormant season
%%  ~~function to do ... ~~
}
\description{
Routine to check if the current month is within a dormant period or not. This is to "switch off" leaf area and hence inhibit photosynthesis,
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isDormantSeason(current.month, leafgrow, leaffall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{current.month}{
%%     ~~Describe \code{current.month} here~~
  The current month (ADD MORE)
}
  \item{leafgrow}{
%%     ~~Describe \code{leafgrow} here~~
  Leaf grow (ADD MORE)
}
  \item{leaffall}{
%%     ~~Describe \code{leaffall} here~~
  Leaf fall (ADD MORE)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
NEEDS MORE
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
function (current.month, leafgrow, leaffall) 
{
    is.dormant <- as.logical()
    if (any(c(leafgrow, leaffall) == 0)) {
        is.dormant <- FALSE
    }
    else if (leafgrow > leaffall) {
        if (current.month >= leaffall && current.month < leafgrow) {
            is.dormant <- TRUE
        }
        else is.dormant <- FALSE
    }
    else if (leafgrow < leaffall) {
        if (current.month < leafgrow || current.month >= leaffall) {
            is.dormant <- TRUE
        }
        else is.dormant <- FALSE
    }
    return(is.dormant)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
