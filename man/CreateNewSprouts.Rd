\name{CreateNewSprouts}
\alias{CreateNewSprouts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create new sprouts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateNewSprouts(state, parms, newsprouts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{newsprouts}{
%%     ~~Describe \code{newsprouts} here~~
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
function (state, parms, newsprouts) 
{
    Wl.s <- parms[["Wl.s"]]
    Wsbr.s <- parms[["Wsbr.s"]]
    N <- state[["N"]]
    Nharv <- state[["Nharv"]]
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    N0 <- N
    if (newsprouts == 1) {
        N <- 4.26831 * Nharv + 285.96252 * (1/12)
    }
    else {
        N <- N + 285.96252 * (1/12)
    }
    Wl <- Wl + (N - N0) * Wl.s/1000
    Wsbr <- Wsbr + (N - N0) * Wsbr.s/1000
    state[["N"]] <- N
    state[["Wl"]] <- Wl
    state[["Wsbr"]] <- Wsbr
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
