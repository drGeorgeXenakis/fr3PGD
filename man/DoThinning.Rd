\name{DoThinning}
\alias{DoThinning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Do thinning
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DoThinning(state, parms, fma, presc)
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
  \item{presc}{
%%     ~~Describe \code{presc} here~~
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
function (state, parms, fma, presc) 
{
    N <- state[["N"]]
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    Wr <- state[["Wr"]]
    op.s <- fma[1, ]
    pNr <- op.s$pNr
    if (is.na(pNr)) {
        pNr <- max((N - op.s$Nres)/N, 0)
    }
    state[["N"]] <- N * (1 - pNr)
    state[["Wl"]] <- Wl * (1 - pNr * op.s$thinWl)
    state[["Wsbr"]] <- Wsbr * (1 - pNr * op.s$thinWsbr)
    state[["Wr"]] <- Wr * (1 - pNr * op.s$thinWr)
    state[["Nharv"]] <- N * pNr
    if (pNr == 1) {
        state[["cycle"]] <- state[["cycle"]] + 1
        if (state[["cycle"]] <= max(presc$cycle)) {
            state[["rotation"]] <- unique(presc[presc$cycle == 
                state[["cycle"]], "rotation"])
        }
        state[["rm.sprouts"]] <- 0
        state[["t"]] <- 0
    }
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
