\name{CreateNewPlantation}
\alias{CreateNewPlantation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create new plantation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateNewPlantation(state, parms, fma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{fma}{
%%     ~~Describe \code{fma} here~~
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
function (state, parms, fma) 
{
    Wl.s <- parms[["Wl.s"]]
    Wr.s <- parms[["Wr.s"]]
    Wsbr.s <- parms[["Wsbr.s"]]
    if (missing(fma)) {
        N <- state[["N"]]
    }
    else {
        N <- unique(fma$Npl)
    }
    state[["N"]] <- N
    state[["Wl"]] <- N * Wl.s/1000
    state[["Wr"]] <- N * Wr.s/1000
    state[["Wsbr"]] <- N * Wsbr.s/1000
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
