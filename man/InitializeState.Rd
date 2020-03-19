\name{InitializeState}
\alias{InitializeState}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize state variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InitializeState(stand, site, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stand}{
%%     ~~Describe \code{stand} here~~
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
function (stand, site, parms) 
{
    names.state <- c("Year", "Month", "t", "hdom", "N", "G", 
        "dg", "Vu", "LAI", "Wl", "Wr", "Wsbr", "rotation", "cycle", 
        "Nharv", "rm.sprouts", "Ww", "Wb", "Wbr", "Wa", "W", 
        "wsbrg", "ASW", "Wlitt", "RAD.day", "RAD", "CanCover", 
        "lightIntcptn", "phi.p", "phi.pa", "fT", "fF", "fCalpha", 
        "fN", "fAge", "fVPD", "fSW", "PhysMod", "alphaC", "GPP", 
        "NPP", "RainIntcptn", "netRad", "fCg", "CanCond", "Etransp", 
        "CanTransp", "Transp", "EvapTransp", "excessSW", "scaleSW", 
        "pR", "pFS", "pS", "pF", "difWl", "difWr", "difWsbr", 
        "Littfall", "difLitter", "difRoots", "TotalLitter", "Ndead", 
        "Wdl", "Wds", "Wdr")
    .length.state.vec <- length(names.state)
    state.vector <- rep(NA, .length.state.vec)
    names(state.vector) <- names.state
    state.init <- state.vector
    state.init[names(stand)] <- stand
    state.init["ASW"] <- site[["ASW"]]
    if (state.init[["t"]] == 0) {
        state.init <- CreateNewPlantation(state = state.init, 
            parms = parms)
    }
    return(state.init)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
