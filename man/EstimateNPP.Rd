\name{EstimateNPP}
\alias{EstimateNPP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate NPP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EstimateNPP(state, parms)
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
    alpha <- parms[["alpha"]]
    gDM_mol <- parms[["gDM_mol"]]
    Y <- parms[["Y"]]
    phi.pa <- state[["phi.pa"]]
    fT <- state[["fT"]]
    fF <- state[["fF"]]
    fCalpha <- state[["fCalpha"]]
    fN <- state[["fN"]]
    PhysMod <- state[["PhysMod"]]
    alphaC <- alpha * fT * fF * fCalpha * fN * PhysMod
    GPP <- alphaC * gDM_mol * phi.pa/100
    NPP <- Y * GPP
    state[c("alphaC", "GPP", "NPP")] <- c(alphaC, GPP, NPP)
    return(state)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
