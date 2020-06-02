EstimateMortality <-
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
