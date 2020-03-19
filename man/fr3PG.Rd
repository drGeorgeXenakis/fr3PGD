\name{fr3PGD}
\alias{fr3PGD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A wrapper function to run the model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fr3PGD(weather, presc, t = 0, N = 1200, Wl = 0, Wr = 0, Wsbr = 0, Wlitt = 0, rotation = 1, cycle = 1, rm.sprouts = F, nyears = 100, latitude = 39, FR = 0.5, soilclass = 0, ASW = 100, MaxASW = 200, MinASW = 0, CO2 = 400, pFS2 = 0.3, pFS20 = 0.09, pfsPower = -0.522878745280338, pfsConst = 0.43104582317421, aS = 0.056, nS = 2.7, pRx = 0.5, pRn = 0.25, Tmin = 6, Topt = 16, Tmax = 40, kF = 1, SWconst0 = 0.7, SWpower0 = 9, m0 = 0, fN0 = 1, fNn = 0, MaxAge = 50, nAge = 4, rAge = 0.95, gammaFx = 0.013, gammaF0 = 0.001, tgammaF = 6, Rttover = 0.01, MaxCond = 0.02, LAIgcx = 3.33, BLcond = 0.2, wSx1000 = 300, thinPower = 1.5, mF = 0, mR = 0.2, mS = 0.2, SLA0 = 11, SLA1 = 4, tSLA = 2.5, k = 0.5, fullCanAge = 3, MaxIntcptn = 0.15, LAImaxIntcptn = 0, alpha = 0.055, Y = 0.47, poolFractn = 0, e20 = 2.2, rhoAir = 1.2, lambda = 2460000, VPDconv = 0.000622, fracBB0 = 0.75, fracBB1 = 0.15, tBB = 2, rhoMin = 0.45, rhoMax = 0.45, tRho = 4, Qa = -90, Qb = 0.8, gDM_mol = 24, molPAR_MJ = 2.3, CoeffCond = 0.05, fCalpha700 = 1.4, fCg700 = 0.7, fCalphax = 2.33333333333333, fCg0 = 1.75, MinCond = 0, Wl.s = 0.526, Wsbr.s = 0.2035, Wr.s = 0.22775, pWl.sprouts = 0.5, pWsbr.sprouts = 0.9, cod.pred = "3PG", cod.clim = "Average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weather}{
%%     ~~Describe \code{weather} here~~
}
  \item{presc}{
%%     ~~Describe \code{presc} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{Wl}{
%%     ~~Describe \code{Wl} here~~
}
  \item{Wr}{
%%     ~~Describe \code{Wr} here~~
}
  \item{Wsbr}{
%%     ~~Describe \code{Wsbr} here~~
}
  \item{Wlitt}{
%%     ~~Describe \code{Wlitt} here~~
}
  \item{rotation}{
%%     ~~Describe \code{rotation} here~~
}
  \item{cycle}{
%%     ~~Describe \code{cycle} here~~
}
  \item{rm.sprouts}{
%%     ~~Describe \code{rm.sprouts} here~~
}
  \item{nyears}{
%%     ~~Describe \code{nyears} here~~
}
  \item{latitude}{
%%     ~~Describe \code{latitude} here~~
}
  \item{FR}{
%%     ~~Describe \code{FR} here~~
}
  \item{soilclass}{
%%     ~~Describe \code{soilclass} here~~
}
  \item{ASW}{
%%     ~~Describe \code{ASW} here~~
}
  \item{MaxASW}{
%%     ~~Describe \code{MaxASW} here~~
}
  \item{MinASW}{
%%     ~~Describe \code{MinASW} here~~
}
  \item{CO2}{
%%     ~~Describe \code{CO2} here~~
}
  \item{pFS2}{
%%     ~~Describe \code{pFS2} here~~
}
  \item{pFS20}{
%%     ~~Describe \code{pFS20} here~~
}
  \item{pfsPower}{
%%     ~~Describe \code{pfsPower} here~~
}
  \item{pfsConst}{
%%     ~~Describe \code{pfsConst} here~~
}
  \item{aS}{
%%     ~~Describe \code{aS} here~~
}
  \item{nS}{
%%     ~~Describe \code{nS} here~~
}
  \item{pRx}{
%%     ~~Describe \code{pRx} here~~
}
  \item{pRn}{
%%     ~~Describe \code{pRn} here~~
}
  \item{Tmin}{
%%     ~~Describe \code{Tmin} here~~
}
  \item{Topt}{
%%     ~~Describe \code{Topt} here~~
}
  \item{Tmax}{
%%     ~~Describe \code{Tmax} here~~
}
  \item{kF}{
%%     ~~Describe \code{kF} here~~
}
  \item{SWconst0}{
%%     ~~Describe \code{SWconst0} here~~
}
  \item{SWpower0}{
%%     ~~Describe \code{SWpower0} here~~
}
  \item{m0}{
%%     ~~Describe \code{m0} here~~
}
  \item{fN0}{
%%     ~~Describe \code{fN0} here~~
}
  \item{fNn}{
%%     ~~Describe \code{fNn} here~~
}
  \item{MaxAge}{
%%     ~~Describe \code{MaxAge} here~~
}
  \item{nAge}{
%%     ~~Describe \code{nAge} here~~
}
  \item{rAge}{
%%     ~~Describe \code{rAge} here~~
}
  \item{gammaFx}{
%%     ~~Describe \code{gammaFx} here~~
}
  \item{gammaF0}{
%%     ~~Describe \code{gammaF0} here~~
}
  \item{tgammaF}{
%%     ~~Describe \code{tgammaF} here~~
}
  \item{Rttover}{
%%     ~~Describe \code{Rttover} here~~
}
  \item{MaxCond}{
%%     ~~Describe \code{MaxCond} here~~
}
  \item{LAIgcx}{
%%     ~~Describe \code{LAIgcx} here~~
}
  \item{BLcond}{
%%     ~~Describe \code{BLcond} here~~
}
  \item{wSx1000}{
%%     ~~Describe \code{wSx1000} here~~
}
  \item{thinPower}{
%%     ~~Describe \code{thinPower} here~~
}
  \item{mF}{
%%     ~~Describe \code{mF} here~~
}
  \item{mR}{
%%     ~~Describe \code{mR} here~~
}
  \item{mS}{
%%     ~~Describe \code{mS} here~~
}
  \item{SLA0}{
%%     ~~Describe \code{SLA0} here~~
}
  \item{SLA1}{
%%     ~~Describe \code{SLA1} here~~
}
  \item{tSLA}{
%%     ~~Describe \code{tSLA} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{fullCanAge}{
%%     ~~Describe \code{fullCanAge} here~~
}
  \item{MaxIntcptn}{
%%     ~~Describe \code{MaxIntcptn} here~~
}
  \item{LAImaxIntcptn}{
%%     ~~Describe \code{LAImaxIntcptn} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{poolFractn}{
%%     ~~Describe \code{poolFractn} here~~
}
  \item{e20}{
%%     ~~Describe \code{e20} here~~
}
  \item{rhoAir}{
%%     ~~Describe \code{rhoAir} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{VPDconv}{
%%     ~~Describe \code{VPDconv} here~~
}
  \item{fracBB0}{
%%     ~~Describe \code{fracBB0} here~~
}
  \item{fracBB1}{
%%     ~~Describe \code{fracBB1} here~~
}
  \item{tBB}{
%%     ~~Describe \code{tBB} here~~
}
  \item{rhoMin}{
%%     ~~Describe \code{rhoMin} here~~
}
  \item{rhoMax}{
%%     ~~Describe \code{rhoMax} here~~
}
  \item{tRho}{
%%     ~~Describe \code{tRho} here~~
}
  \item{Qa}{
%%     ~~Describe \code{Qa} here~~
}
  \item{Qb}{
%%     ~~Describe \code{Qb} here~~
}
  \item{gDM_mol}{
%%     ~~Describe \code{gDM_mol} here~~
}
  \item{molPAR_MJ}{
%%     ~~Describe \code{molPAR_MJ} here~~
}
  \item{CoeffCond}{
%%     ~~Describe \code{CoeffCond} here~~
}
  \item{fCalpha700}{
%%     ~~Describe \code{fCalpha700} here~~
}
  \item{fCg700}{
%%     ~~Describe \code{fCg700} here~~
}
  \item{fCalphax}{
%%     ~~Describe \code{fCalphax} here~~
}
  \item{fCg0}{
%%     ~~Describe \code{fCg0} here~~
}
  \item{MinCond}{
%%     ~~Describe \code{MinCond} here~~
}
  \item{Wl.s}{
%%     ~~Describe \code{Wl.s} here~~
}
  \item{Wsbr.s}{
%%     ~~Describe \code{Wsbr.s} here~~
}
  \item{Wr.s}{
%%     ~~Describe \code{Wr.s} here~~
}
  \item{pWl.sprouts}{
%%     ~~Describe \code{pWl.sprouts} here~~
}
  \item{pWsbr.sprouts}{
%%     ~~Describe \code{pWsbr.sprouts} here~~
}
  \item{cod.pred}{
%%     ~~Describe \code{cod.pred} here~~
}
  \item{cod.clim}{
%%     ~~Describe \code{cod.clim} here~~
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
function (weather, presc, t = 0, N = 1200, Wl = 0, Wr = 0, Wsbr = 0, 
    Wlitt = 0, rotation = 1, cycle = 1, rm.sprouts = F, nyears = 100, 
    latitude = 39, FR = 0.5, soilclass = 0, ASW = 100, MaxASW = 200, 
    MinASW = 0, CO2 = 400, pFS2 = 0.3, pFS20 = 0.09, pfsPower = -0.522878745280338, 
    pfsConst = 0.43104582317421, aS = 0.056, nS = 2.7, pRx = 0.5, 
    pRn = 0.25, Tmin = 6, Topt = 16, Tmax = 40, kF = 1, SWconst0 = 0.7, 
    SWpower0 = 9, m0 = 0, fN0 = 1, fNn = 0, MaxAge = 50, nAge = 4, 
    rAge = 0.95, gammaFx = 0.013, gammaF0 = 0.001, tgammaF = 6, 
    Rttover = 0.01, MaxCond = 0.02, LAIgcx = 3.33, BLcond = 0.2, 
    wSx1000 = 300, thinPower = 1.5, mF = 0, mR = 0.2, mS = 0.2, 
    SLA0 = 11, SLA1 = 4, tSLA = 2.5, k = 0.5, fullCanAge = 3, 
    MaxIntcptn = 0.15, LAImaxIntcptn = 0, alpha = 0.055, Y = 0.47, 
    poolFractn = 0, e20 = 2.2, rhoAir = 1.2, lambda = 2460000, 
    VPDconv = 0.000622, fracBB0 = 0.75, fracBB1 = 0.15, tBB = 2, 
    rhoMin = 0.45, rhoMax = 0.45, tRho = 4, Qa = -90, Qb = 0.8, 
    gDM_mol = 24, molPAR_MJ = 2.3, CoeffCond = 0.05, fCalpha700 = 1.4, 
    fCg700 = 0.7, fCalphax = 2.33333333333333, fCg0 = 1.75, MinCond = 0, 
    Wl.s = 0.526, Wsbr.s = 0.2035, Wr.s = 0.22775, pWl.sprouts = 0.5, 
    pWsbr.sprouts = 0.9, cod.pred = "3PG", cod.clim = "Average") 
{
    source("/home/forestresearch/Documents/FORPREF-Wind/3PG_ForestGALES/01_Implementation_3PG.R")
    parms <- c(pFS2, pFS20, pfsPower, pfsConst, aS, nS, pRx, 
        pRn, Tmin, Topt, Tmax, kF, SWconst0, SWpower0, m0, fN0, 
        fNn, MaxAge, nAge, rAge, gammaFx, gammaF0, tgammaF, Rttover, 
        MaxCond, LAIgcx, BLcond, wSx1000, thinPower, mF, mR, 
        mS, SLA0, SLA1, tSLA, k, fullCanAge, MaxIntcptn, LAImaxIntcptn, 
        alpha, Y, poolFractn, e20, rhoAir, lambda, VPDconv, fracBB0, 
        fracBB1, tBB, rhoMin, rhoMax, tRho, Qa, Qb, gDM_mol, 
        molPAR_MJ, CoeffCond, fCalpha700, fCg700, fCalphax, fCg0, 
        MinCond, Wl.s, Wsbr.s, Wr.s, pWl.sprouts, pWsbr.sprouts)
    names(parms) <- c("pFS2", "pFS20", "pfsPower", "pfsConst", 
        "aS", "nS", "pRx", "pRn", "Tmin", "Topt", "Tmax", "kF", 
        "SWconst0", "SWpower0", "m0", "fN0", "fNn", "MaxAge", 
        "nAge", "rAge", "gammaFx", "gammaF0", "tgammaF", "Rttover", 
        "MaxCond", "LAIgcx", "BLcond", "wSx1000", "thinPower", 
        "mF", "mR", "mS", "SLA0", "SLA1", "tSLA", "k", "fullCanAge", 
        "MaxIntcptn", "LAImaxIntcptn", "alpha", "Y", "poolFractn", 
        "e20", "rhoAir", "lambda", "VPDconv", "fracBB0", "fracBB1", 
        "tBB", "rhoMin", "rhoMax", "tRho", "Qa", "Qb", "gDM_mol", 
        "molPAR_MJ", "CoeffCond", "fCalpha700", "fCg700", "fCalphax", 
        "fCg0", "MinCond", "Wl.s", "Wsbr.s", "Wr.s", "pWl.sprouts", 
        "pWsbr.sprouts")
    vars.ini <- c(t, N, Wl, Wr, Wsbr, Wlitt, rotation, cycle, 
        rm.sprouts, nyears)
    names(vars.ini) <- c("t", "N", "Wl", "Wr", "Wsbr", "Wlitt", 
        "rotation", "cycle", "rm.sprouts", "nyears")
    site.info <- c(latitude, FR, soilclass, ASW, MaxASW, MinASW, 
        CO2)
    names(site.info) <- c("latitude", "FR", "soilclass", "ASW", 
        "MaxASW", "MinASW", "CO2")
    parms.general <- list(daysinmonth = c(Jan = 31, Feb = 28, 
        Mar = 31, Apr = 30, May = 31, Jun = 30, Jul = 31, Aug = 31, 
        Sep = 30, Oct = 31, Nov = 30, Dec = 31), parms.soil = data.frame(soilclass.name = c("Sandy", 
        "Sandy loam", "Clay loam", "Clay", "Non standard", "No effect of ASW"), 
        soilclass = c(1, 2, 3, 4, NA, 0), SWconst = c(0.7, 0.6, 
            0.5, 0.4, parms[["SWconst0"]], 1), SWpower = c(9, 
            7, 5, 3, parms[["SWpower0"]], 1)))
    proj <- Run3PG(stand.init = vars.ini, weather = weather, 
        site = site.info, general.info = parms.general, presc = presc, 
        parms = parms, cod.pred = cod.pred, cod.clim = cod.clim)
    return(as.data.frame(proj))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
