\name{Run3PG}
\alias{Run3PG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The main function running the model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Run3PG(stand.init, weather, site, parms, general.info = parms.general, presc = presc, cod.pred = "3PG", cod.clim = "Average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stand.init}{
%%     ~~Describe \code{stand.init} here~~
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
  \item{presc}{
%%     ~~Describe \code{presc} here~~
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
function (stand.init, weather, site, parms, general.info = parms.general, 
    presc = presc, cod.pred = "3PG", cod.clim = "Average") 
{
    state.init <- InitializeState(stand = stand.init, site = site, 
        parms = parms)
    weather <- PredictWeatherVariables(weather = weather)
    proj.results <- list()
    state <- PredictVariablesInterest.3PG(state = state.init, 
        parms = parms, cod.pred = cod.pred)
    t.proj <- 0
    proj.results[[1]] <- c(t.proj = t.proj, state)
    if (!missing(presc)) {
        fma.c <- presc[presc$cycle == state[["cycle"]], ]
        t.nsprouts <- unique(fma.c[, "t.nsprouts"])
        fst.row.fma.app <- min(c(which(fma.c$t > state[["t"]]), 
            nrow(fma.c)))
        fma.app <- fma.c[fst.row.fma.app:nrow(fma.c), ]
        fma.app$t[which(fma.app$t < state[["t"]])] <- state[["t"]]
    }
    if (cod.clim == "Average") {
        N = stand.init[["nyears"]] * 12
    }
    else if (cod.clim == "Month") {
        N = nrow(weather)
    }
    j = 1
    weather.i <- as.numeric()
    for (i in 1:N) {
        if (cod.clim == "Average") {
            if (j > 12) 
                j = 1
            weather.i <- weather[j, ]
        }
        else if (cod.clim == "Month") {
            weather.i <- weather[i, ]
        }
        state.apar <- EstimateAPAR(state = state, weather = weather.i, 
            parms = parms, general.info = general.info)
        state.mods <- CalculateModifiers(state = state.apar, 
            weather = weather.i, site = site, parms = parms, 
            general.info = general.info)
        state.npp <- EstimateNPP(state = state.mods, parms = parms)
        state.asw <- UpdateASW(state = state.npp, weather = weather.i, 
            site = site, parms = parms, general.info = general.info)
        state.walloc <- AllocateBiomass(state = state.asw, site = site, 
            parms = parms)
        state.mort <- EstimateMortality(state = state.walloc, 
            parms = parms)
        state.mort[["t"]] <- state.mort[["t"]] + 1/12
        state.end <- PredictVariablesInterest.3PG(state = state.mort, 
            parms = parms, cod.pred = cod.pred)
        if (!missing(presc) && nrow(fma.app) > 0 && ((abs(fma.app$t[1] - 
            state.end[["t"]]) < 1/24) | (fma.app$t[1] < state.end[["t"]]))) {
            state.end <- DoThinning(state = state.end, parms = parms, 
                fma = fma.app, presc = presc)
            fma.app <- fma.app[-1, ]
            state.end <- PredictVariablesInterest.3PG(state = state.end, 
                parms = parms, cod.pred = cod.pred)
        }
        if (!missing(presc) && state.end[["N"]] == 0) {
            if (state.end[["cycle"]] > max(presc$cycle)) {
                t.proj <- t.proj + 1/12
                proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
                break
            }
            fma.c <- presc[which(presc$cycle == state.end[["cycle"]]), 
                ]
            t.nsprouts <- unique(fma.c[, "t.nsprouts"])
            fma.app <- fma.c
            it.newsprouts <- 1
            if (state.end[["rotation"]] == 1) {
                state.end <- CreateNewPlantation(state = state.end, 
                  parms = parms, fma = fma.app)
                state.end <- PredictVariablesInterest.3PG(state = state.end, 
                  parms = parms, cod.pred = cod.pred)
            }
        }
        if (!missing(presc) && state.end[["rotation"]] > 1 && 
            state.end[["t"]] < t.nsprouts) {
            state.end <- CreateNewSprouts(state = state.end, 
                parms = parms, newsprouts = it.newsprouts)
            state.end <- PredictVariablesInterest.3PG(state = state.end, 
                parms = parms, cod.pred = cod.pred)
            it.newsprouts <- it.newsprouts + 1
        }
        if (!missing(presc) && state.end[["rotation"]] > 1 && 
            state.end[["t"]] > (t.nsprouts - 1/24) && state.end[["rm.sprouts"]] == 
            0) {
            state.end <- RemoveSprouts(state = state.end, parms = parms, 
                fma = fma.app)
        }
        t.proj <- t.proj + 1/12
        proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
        state <- state.end
        j = j + 1
    }
    proj.df <- data.frame(do.call(rbind, proj.results))
    return(proj.df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
