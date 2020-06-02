CreateNewPlantation <-
function (state, parms, fma) 
{
    Wl.s <- parms[["Wl.s"]]
    Wr.s <- parms[["Wr.s"]]
    Wsbr.s <- parms[["Wsbr.s"]]
    if (missing(fma)) {
        N <- state[["N"]]
    }
    else {
        N <- unique(fma$Npl)
    }
    state[["N"]] <- N
    state[["Wl"]] <- N * Wl.s/1000
    state[["WlDormant"]] <- state[["Wl"]]
    state[["Wr"]] <- N * Wr.s/1000
    state[["Wsbr"]] <- N * Wsbr.s/1000
    return(state)
}
