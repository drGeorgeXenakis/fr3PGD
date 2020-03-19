\name{EstimateMortality}
\alias{EstimateMortality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate Mortality
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EstimateMortality(state, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
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
function (state, parms) 
{
    wSx1000 <- parms[["wSx1000"]]
    mF <- parms[["mF"]]
    mR <- parms[["mR"]]
    mS <- parms[["mS"]]
    thinPower <- parms[["thinPower"]]
    oldW <- state[["Wsbr"]]
    oldN <- state[["N"]]
    if (oldW > (wSx1000 * (1000/oldN)^thinPower)) {
        N.new <- optimize(f = function(x) ((oldW - mS * (oldN - 
            x) * oldW/oldN) - (wSx1000 * (1000/x)^thinPower * 
            0.001) * x)^2, interval = c(1, 5000))$minimum
        N <- min(oldN, N.new)
    }
    else {
        N <- oldN
    }
    Ndead <- oldN - N
    if (Ndead > 0) {
        Wl <- state[["Wl"]]
        Wsbr <- oldW
        Wr <- state[["Wr"]]
        Wdl <- mF * Ndead * (Wl/N)
        Wds <- mS * Ndead * (Wsbr/N)
        Wdr <- mR * Ndead * (Wr/N)
        state[["Wl"]] <- Wl - Wdl
        state[["Wsbr"]] <- Wsbr - Wds
        state[["Wr"]] <- Wr - Wdr
        state[["N"]] <- state[["N"]] - Ndead
    }
    else {
        Wdl <- 0
        Wds <- 0
        Wdr <- 0
    }
    state[c("Ndead", "Wdl", "Wds", "Wdr")] <- c(Ndead, Wdl, Wds, 
        Wdr)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
