\name{RemoveSprouts}
\alias{RemoveSprouts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove sprouts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RemoveSprouts(state, parms, fma)
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
    pWl.sprouts <- parms[["pWl.sprouts"]]
    pWsbr.sprouts <- parms[["pWsbr.sprouts"]]
    N <- state[["N"]]
    Nharv <- state[["Nharv"]]
    nsprouts <- unique(fma[, "nsprouts"])
    Nharv <- ifelse(is.na(Nharv), N/nsprouts, Nharv)
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    Nsprouts <- N
    N <- nsprouts * Nharv
    state[["Wl"]] <- Wl - (Nsprouts - N) * Wl/Nsprouts * pWl.sprouts
    state[["Wsbr"]] <- Wsbr - (Nsprouts - N) * Wl/Nsprouts * 
        pWsbr.sprouts
    state[["rm.sprouts"]] <- 1
    state[["N"]] <- N
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
