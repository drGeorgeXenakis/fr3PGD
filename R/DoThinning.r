DoThinning <-
function (state, parms, fma, presc) 
{
    N <- state[["N"]]
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    Wr <- state[["Wr"]]
    op.s <- fma[1, ]
    pNr <- op.s$pNr
    if (is.na(pNr)) {
        pNr <- max((N - op.s$Nres)/N, 0)
    }
    state[["N"]] <- N * (1 - pNr)
    state[["Wl"]] <- Wl * (1 - pNr * op.s$thinWl)
    state[["Wsbr"]] <- Wsbr * (1 - pNr * op.s$thinWsbr)
    state[["Wr"]] <- Wr * (1 - pNr * op.s$thinWr)
    state[["Nharv"]] <- N * pNr
    if (pNr == 1) {
        state[["cycle"]] <- state[["cycle"]] + 1
        if (state[["cycle"]] <= max(presc$cycle)) {
            state[["rotation"]] <- unique(presc[presc$cycle == 
                state[["cycle"]], "rotation"])
        }
        state[["rm.sprouts"]] <- 0
        state[["t"]] <- 0
    }
    return(state)
}
