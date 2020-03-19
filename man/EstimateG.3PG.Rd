\name{EstimateG.3PG}
\alias{EstimateG.3PG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate basal area
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EstimateG.3PG(N, Wa)
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
    k <- 0.6222 + 0.1719 * N/1000
    a <- 0.6987 - 0.0196 * N/1000
    G <- k * Wa^a
    return(G)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
