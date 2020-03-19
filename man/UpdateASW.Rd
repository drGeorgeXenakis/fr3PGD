\name{UpdateASW}
\alias{UpdateASW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update ASW
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UpdateASW(state, weather, site, parms, general.info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{weather}{
%%     ~~Describe \code{weather} here~~
}
  \item{site}{
%%     ~~Describe \code{site} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{general.info}{
%%     ~~Describe \code{general.info} here~~
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
function (state, weather, site, parms, general.info) 
{
    Rain <- weather[["Rain"]]
    MonthIrrig <- weather[["MonthIrrig"]]
    ASW <- state[["ASW"]]
    ASWrain <- ASW + Rain + MonthIrrig
    LAImaxIntcptn <- parms[["LAImaxIntcptn"]]
    MaxIntcptn <- parms[["MaxIntcptn"]]
    RainIntcptn <- MaxIntcptn * ifelse(LAImaxIntcptn <= 0, 1, 
        min(1, LAI/LAImaxIntcptn)) * Rain
    Qa <- parms[["Qa"]]
    Qb <- parms[["Qb"]]
    month <- weather[["Month"]]
    latitude <- site[["latitude"]]
    RAD.day <- state[["RAD.day"]]
    h <- GetDayLength(Lat = latitude, month = month)
    netRad <- Qa + Qb * (RAD.day * 1e+06/h)
    MinCond <- parms[["MinCond"]]
    MaxCond <- parms[["MaxCond"]]
    LAIgcx <- parms[["LAIgcx"]]
    fCg0 <- parms[["fCg0"]]
    CO2 <- site[["CO2"]]
    PhysMod <- state[["PhysMod"]]
    LAI <- state[["LAI"]]
    fCg <- fCg0/(1 + (fCg0 - 1) * CO2/350)
    CanCond <- (MinCond + (MaxCond - MinCond) * (min(1, LAI/LAIgcx))) * 
        PhysMod * fCg
    CanCond <- ifelse(CanCond == 0, 1e-04, CanCond)
    e20 <- parms[["e20"]]
    rhoAir <- parms[["rhoAir"]]
    lambda <- parms[["lambda"]]
    VPDconv <- parms[["VPDconv"]]
    BLcond <- parms[["BLcond"]]
    VPD <- weather[["VPD"]]
    Etransp <- (e20 * netRad + rhoAir * lambda * VPDconv * VPD * 
        BLcond)/(1 + e20 + BLcond/CanCond)
    CanTransp <- Etransp/lambda * h
    Transp <- general.info$daysinmonth[month] * CanTransp
    EvapTransp <- min(Transp + RainIntcptn, ASWrain)
    MaxASW <- site[["MaxASW"]]
    excessSW <- max(ASWrain - EvapTransp - MaxASW, 0)
    state[["ASW"]] <- ASWrain - EvapTransp - excessSW
    scaleSW <- EvapTransp/(Transp + RainIntcptn)
    GPP <- state[["GPP"]]
    NPP <- state[["NPP"]]
    state[["GPP"]] <- GPP * scaleSW
    state[["NPP"]] <- NPP * scaleSW
    state[c("RainIntcptn", "netRad", "CanCond", "Etransp", "CanTransp", 
        "Transp", "EvapTransp", "excessSW", "scaleSW")] <- c(RainIntcptn, 
        netRad, CanCond, Etransp, CanTransp, Transp, EvapTransp, 
        excessSW, scaleSW)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
