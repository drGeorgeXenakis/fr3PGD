\name{CalculateModifiers}
\alias{CalculateModifiers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate modifiers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CalculateModifiers(state, weather, site, parms, general.info)
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
    Tmin <- parms[["Tmin"]]
    Tmax <- parms[["Tmax"]]
    Topt <- parms[["Topt"]]
    Tav <- weather[["Tmean"]]
    if (Tav < Tmin | Tav > Tmax) {
        fT <- 0
    }
    else {
        fT <- ((Tav - Tmin)/(Topt - Tmin)) * ((Tmax - Tav)/(Tmax - 
            Topt))^((Tmax - Topt)/(Topt - Tmin))
    }
    kF <- parms[["kF"]]
    FrostDays <- weather[["FrostDays"]]
    fF <- 1 - kF * (FrostDays/30)
    CO2 <- site[["CO2"]]
    fCalphax <- parms[["fCalphax"]]
    fCalpha <- fCalphax * CO2/(350 * (fCalphax - 1) + CO2)
    fN0 <- parms[["fN0"]]
    FR <- site[["FR"]]
    fN <- fN0 + (1 - fN0) * FR
    t <- state[["t"]]
    RelAge <- t/parms[["MaxAge"]]
    rAge <- parms[["rAge"]]
    nAge <- parms[["nAge"]]
    fAge <- 1/(1 + (RelAge/rAge)^nAge)
    CoeffCond <- parms[["CoeffCond"]]
    VPD <- weather[["VPD"]]
    fVPD <- exp(-CoeffCond * VPD)
    MaxASW <- site[["MaxASW"]]
    parms.soil <- general.info$parms.soil
    parms.sw.site <- parms.soil[which(parms.soil$soilclass == 
        site[["soilclass"]]), ]
    SWconst <- parms.sw.site[["SWconst"]]
    SWpower <- parms.sw.site[["SWpower"]]
    ASW <- state[["ASW"]]
    MoistRatio <- ASW/MaxASW
    fSW <- 1/(1 + ((1 - MoistRatio)/SWconst)^SWpower)
    PhysMod <- fAge * min(fVPD, fSW)
    state[c("fT", "fF", "fCalpha", "fN", "fAge", "fVPD", "fSW", 
        "PhysMod")] <- c(fT, fF, fCalpha, fN, fAge, fVPD, fSW, 
        PhysMod)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
