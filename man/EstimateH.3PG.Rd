\name{EstimateH.3PG}
\alias{EstimateH.3PG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate height
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EstimateH.3PG(N, Wa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{Wa}{
%%     ~~Describe \code{Wa} here~~
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
function (N, Wa) 
{
    k <- 4.8965 - 0.3972 * N/1000
    if (N >= 12000) {
        k <- 4.8965 - 0.3972 * 12000/1000
    }
    a <- 0.3482 + 0.0014 * N/1000
    hdom <- k * Wa^a
    return(hdom)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
