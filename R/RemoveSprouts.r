RemoveSprouts <-
function (state, parms, fma) 
{
    pWl.sprouts <- parms[["pWl.sprouts"]]
    pWsbr.sprouts <- parms[["pWsbr.sprouts"]]
    N <- state[["N"]]
    Nharv <- state[["Nharv"]]
    nsprouts <- unique(fma[, "nsprouts"])
    Nharv <- ifelse(is.na(Nharv), N/nsprouts, Nharv)
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    Nsprouts <- N
    N <- nsprouts * Nharv
    state[["Wl"]] <- Wl - (Nsprouts - N) * Wl/Nsprouts * pWl.sprouts
    state[["Wsbr"]] <- Wsbr - (Nsprouts - N) * Wl/Nsprouts * 
        pWsbr.sprouts
    state[["rm.sprouts"]] <- 1
    state[["N"]] <- N
    return(state)
}
