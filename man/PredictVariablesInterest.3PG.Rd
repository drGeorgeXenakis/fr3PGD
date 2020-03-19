\name{PredictVariablesInterest.3PG}
\alias{PredictVariablesInterest.3PG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predicte variables of interest
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PredictVariablesInterest.3PG(state, parms, cod.pred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{cod.pred}{
%%     ~~Describe \code{cod.pred} here~~
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
function (state, parms, cod.pred) 
{
    SLA0 <- parms[["SLA0"]]
    SLA1 <- parms[["SLA1"]]
    tSLA <- parms[["tSLA"]]
    fracBB0 <- parms[["fracBB0"]]
    fracBB1 <- parms[["fracBB1"]]
    tBB <- parms[["tBB"]]
    t <- state[["t"]]
    N <- state[["N"]]
    Wl <- state[["Wl"]]
    Wr <- state[["Wr"]]
    Wsbr <- state[["Wsbr"]]
    SLA <- SLA1 + (SLA0 - SLA1) * exp(-log(2) * (t/tSLA)^2)
    LAI <- Wl * SLA * 0.1
    fracBB <- fracBB1 + (fracBB0 - fracBB1) * exp(-log(2) * (t/tBB))
    Ww <- Wsbr * (1 - fracBB)
    Wb <- (Wsbr - Ww) * 0.5675
    Wbr <- (Wsbr - Ww) - Wb
    Wa <- Wl + Wsbr
    W <- Wl + Wr + Wsbr
    hdom <- EstimateH.3PG(N = N, Wa = Wa)
    wsbrg <- Wsbr * 1000/N
    if (cod.pred == "3PG") {
        rhoMax <- parms[["rhoMax"]]
        rhoMin <- parms[["rhoMin"]]
        tRho <- parms[["tRho"]]
        WoodDensity <- rhoMax + (rhoMin - rhoMax) * exp(-log(2) * 
            t/tRho)
        Vu <- Wsbr * (1 - fracBB)/WoodDensity
        dg <- (wsbrg/parms[["aS"]])^(1/parms[["nS"]])
        G <- (dg/200)^2 * pi * N
    }
    else if (cod.pred == "Allometric") {
        G <- EstimateG.3PG(N = N, Wa = Wa)
        Vub <- EstimateV.3PG(N = N, Ww = Ww)
        dg <- ObtainDg(N = N, G = G)
    }
    state[c("LAI", "Ww", "Wb", "Wbr", "Wa", "W", "hdom", "wsbrg", 
        "G", "Vu", "dg")] <- c(LAI, Ww, Wb, Wbr, Wa, W, hdom, 
        wsbrg, G, Vu, dg)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
