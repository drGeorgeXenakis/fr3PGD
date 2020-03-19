\name{AllocateBiomass}
\alias{AllocateBiomass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Allocate biomass
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllocateBiomass(state, site, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{site}{
%%     ~~Describe \code{site} here~~
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
function (state, site, parms) 
{
    m0 <- parms[["m0"]]
    pRx <- parms[["pRx"]]
    pRn <- parms[["pRn"]]
    FR <- site[["FR"]]
    PhysMod <- state[["PhysMod"]]
    m <- m0 + (1 - m0) * FR
    pR <- pRx * pRn/(pRn + (pRx - pRn) * m * PhysMod)
    pFS2 <- parms[["pFS2"]]
    pFS20 <- parms[["pFS20"]]
    pfsPower <- log(pFS20/pFS2)/log(10)
    pfsConst <- pFS2/(2^pfsPower)
    dg <- state[["dg"]]
    pFS <- pfsConst * dg^pfsPower
    pS <- (1 - pR)/(pFS + 1)
    pF <- 1 - pR - pS
    NPP <- state[["NPP"]]
    difWl <- NPP * pF
    difWr <- NPP * pR
    difWsbr <- NPP * pS
    gammaFx <- parms[["gammaFx"]]
    gammaF0 <- parms[["gammaF0"]]
    tgammaF <- parms[["tgammaF"]]
    t <- state[["t"]]
    Wl <- state[["Wl"]]
    Littfall <- gammaFx * gammaF0/(gammaF0 + (gammaFx - gammaF0) * 
        exp(-12 * log(1 + gammaFx/gammaF0) * t/tgammaF))
    difLitter <- Littfall * Wl
    state[["Wlitt"]] <- state[["Wlitt"]] + difLitter
    Wr <- state[["Wr"]]
    Rttover <- parms[["Rttover"]]
    difRoots <- Rttover * Wr
    TotalLitter <- state[["Wlitt"]]
    Wsbr <- state[["Wsbr"]]
    state[["Wl"]] <- Wl + difWl - difLitter
    state[["Wr"]] <- Wr + difWr - difRoots
    state[["Wsbr"]] <- Wsbr + difWsbr
    TotalLitter <- TotalLitter + difLitter
    state[c("pR", "pFS", "pS", "pF", "difWl", "difWr", "difWsbr", 
        "Littfall", "difLitter", "difRoots", "TotalLitter")] <- c(pR, 
        pFS, pS, pF, difWl, difWr, difWsbr, Littfall, difLitter, 
        difRoots, TotalLitter)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
